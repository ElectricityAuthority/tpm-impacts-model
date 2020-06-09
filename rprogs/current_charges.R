###############################################
### Title: Current charges                  ###
### Description: calculate current and      ###
### adjusted charges.                       ###
### Date: 29 November 2019                  ###
###############################################

# Derive an adjusted total revenue amount 
#  (i.e. Transpower revenue forecast minus LCE minus prudential/notional charges)

## Get status quo LCE revenue
LCE_status_quo <- total_revenue_forecast %>% 
  filter(scenario_type == "status quo", 
         revenue_type == "Loss and constraint excess revenue") %>% 
  pull(revenue_million)

## Get status quo 'Prudent discount and Notional embedding agreement charges'
prudential_discount_status_quo <- total_revenue_forecast %>% 
  filter(scenario_type == "status quo", 
         revenue_type == "Prudent discount and Notional embedding agreement charges") %>% 
  pull(revenue_million)

## Calculate the adjusted revenue total
forecast_revenue_adj <- transpower_revenue_forecast - LCE_status_quo - prudential_discount_status_quo

# # Sum of current disclosed charges
# total_current_charges <- sum(current_charges$charge_dollars)

# Adjust connection charge
connection_charge_adj <- current_charges %>% 
  filter(charge_type == "Connection charge") %>% 
  mutate(
    # Multiply current charge by delta of current connection charge vs adjusted forecast revenue
    charge_dollars_adjusted = charge_dollars * ((forecast_revenue_adj * 1000000) / total_current_charges)
  )

# Adjust HVDC charge

## Get total forecast revenue for HVDC pole 2 and 3 investment
HVDC_benefit_charge <- benefit_revenue_forecast %>% 
  filter(year == "2021/22", investment == "HVDC Pole 2 and 3") %>% 
  pull(revenue_million)

HVDC_charge_adj <- current_charges %>% 
  filter(charge_type == "HVDC charge") %>% 
  mutate(
    
    # Get proportion by customer for current HVDC charges
    current_charge_proportion = charge_dollars / sum(charge_dollars),
    
    # Multiply current charge proportion by forecast HVDC benefit charge
    charge_dollars_adjusted = current_charge_proportion * (HVDC_benefit_charge * 1000000)
  )

# Adjust interconnection charge

## Get current (status quo) HVDC and interconnection charge
HVDC_interconnection_charge_total <- total_revenue_forecast %>% 
  filter(scenario_type == "status quo", revenue_type == "HVDC and interconnection charge") %>% 
  pull(revenue_million)

## Subtract the HVDC benefit charge from the 
##  HVDC/interconnection charge to isolate the interconnection charge
interconnection_charge <- HVDC_interconnection_charge_total - HVDC_benefit_charge

interconnection_charge_adj <- current_charges %>% 
  filter(charge_type == "Interconnection charge") %>% 
  mutate(
    
    # Get proportion by customer for current interconnection charges
    current_charge_proportion = charge_dollars / sum(charge_dollars),
    
    # Multiply current charge proportion by interconnection charge
    charge_dollars_adjusted = current_charge_proportion * (interconnection_charge * 1000000)
  )

# Join adjusted charges in to single dataframe
adjusted_charges <- connection_charge_adj %>% 
  union_all(
    HVDC_charge_adj %>% 
      select(-current_charge_proportion)
  ) %>% 
  union_all(
    interconnection_charge_adj %>% 
      select(-current_charge_proportion)
  ) %>% 
  # Adjustment 2: add KCE (Mangahao) to adjusted charges dataframe (with 0 charges)
  purrr::when(
    adj2_flag == TRUE ~ 
      add_row(
        .,
        transpower_customer = "KCE (Mangahao)",
        charge_type = "Connection charge",
        charge_dollars = 0,
        charge_dollars_adjusted = 0
      ),
    
    ~ .
  )

# Adjustment 7: update the connection charge in the revenue forecast dataframe
# based on the adjusted connection charge calculated above
revenue_forecast_raw <- revenue_forecast_raw %>%
  purrr::when(
    adj7_flag == TRUE ~ 
      mutate(
        .,
        revenue_million = case_when(
          # Use the sum of the adjusted connection charges to update
          revenue_type == "Connection charge" ~ sum(connection_charge_adj$charge_dollars_adjusted) / 1000000,
          TRUE ~ revenue_million
        )
      ),
    
    ~ .
  )
