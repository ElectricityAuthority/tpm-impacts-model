###############################################
### Title: Current charges                  ###
### Description:                            ###
### Date: 29 November 2019                  ###
###############################################

# Derive an adjusted total revenue amount 
#  (i.e. Total forecast revenue minus LCE minus prudential/notional charges)
### TODO: maybe change this bit to just filtering the total_revenue_forecast by
###  HVDC and interconnection + connection charge and taking the sum. Would
###  just need to understand how things change when/if the total forecast revenue
###  changes (i.e. the status quo values in the revenue CSV are static and sum to 
###  the total revenue but there is no dynamic link between them).
LCE_status_quo <- total_revenue_forecast %>% 
  filter(scenario_type == "status quo", revenue_type == "Loss and constraint excess revenue") %>% 
  pull(revenue_million)

prudential_discount_status_quo <- total_revenue_forecast %>% 
  filter(scenario_type == "status quo", revenue_type == "Prudent discount and Notional embedding agreement charges") %>% 
  pull(revenue_million)

forecast_revenue_adj <- forecast_revenue_total - LCE_status_quo - prudential_discount_status_quo

# Sum of current disclosed charges
total_current_charges <- sum(current_charges$charge_dollars)

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
  )