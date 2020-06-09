###############################################
### Title: Calculate revenue                ###
### Description: Create the final           ###
### revenue dataset by calculating the      ###
### residual for the proposed scenario.      ###
### Date: 29 November 2019                  ###
###############################################

# Calculate sum of prudent discount and LCE revenue
prudent_LCE_sum <- revenue_forecast_raw %>% 
  filter(scenario_type == "status quo",
         revenue_type %in% c("Prudent discount and Notional embedding agreement charges",
                             "Loss and constraint excess revenue")) %>% 
  summarise(revenue_million = sum(revenue_million)) %>% 
  pull(revenue_million)

# Sum of current disclosed charges
total_current_charges <- sum(current_charges$charge_dollars)

# Sum of unadjusted connection charges
total_connection_charges_unadj <- current_charges %>% 
  filter(charge_type == "Connection charge") %>%
  summarise(charge_dollars = sum(charge_dollars)) %>% 
  pull(charge_dollars)

# Calculate forecast revenue
total_revenue_forecast <- revenue_forecast_raw %>% 
  
  # Adjustment 7: update connection charge
  purrr::when(
    adj7_flag == TRUE ~
      mutate(
        .,
        
        revenue_million = if_else(
          revenue_type == "Connection charge",
          (total_connection_charges_unadj / 1000000) * 
            ((transpower_revenue_forecast - prudent_LCE_sum) / (total_current_charges / 1000000)),
          revenue_million
        )
        
      ),
    ~ .
    
  ) %>% 
  
  # Pivot to wide dataset
  pivot_wider(names_from = revenue_type, values_from = revenue_million) %>% 
  mutate(
    
    # Calculate residual
    Residual = if_else(
      scenario_type == "proposed",
      transpower_revenue_forecast - `HVDC and interconnection charge` -
        `Loss and constraint excess revenue` - `Benefit charge` -
        `Connection charge` - `Prudent discount and Notional embedding agreement charges`,
      0
    )
    
  ) %>% 
  
  # Adjustment 7: update HVDC + interconnection charge (for status quo)
  purrr::when(
    adj7_flag == TRUE ~
      mutate(
        .,
        
        `HVDC and interconnection charge` = transpower_revenue_forecast - Residual - 
          `Loss and constraint excess revenue` - `Benefit charge` - 
          `Connection charge` - `Prudent discount and Notional embedding agreement charges`
        
      ),
    ~ .
    
  ) %>% 
  
  # Turn back in to long dataset
  pivot_longer(-scenario_type, names_to = "revenue_type", values_to = "revenue_million") %>% 
  # Set revenue to 0 where effectively so (it's > 0 for some because of rounding)
  mutate(revenue_million = ifelse(revenue_million < 1e-5, 0, revenue_million))
  
