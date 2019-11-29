###############################################
### Title: Calculate revenue                ###
### Description: Create the final           ###
### revenue dataset by calculating the      ###
### residual for the proposed scenario.      ###
### Date: 29 November 2019                  ###
###############################################

# Calculate residual 
total_revenue_forecast <- total_revenue_forecast_raw %>% 
  
  # Pivot to wide dataset
  pivot_wider(names_from = revenue_type, values_from = revenue_million) %>% 
  mutate(
    
    # Calculate residual
    Residual =  forecast_revenue_total - `HVDC and interconnection charge` - 
      `Loss and constraint excess revenue` - `Benefit charge` - 
      `Connection charge` - `Prudent discount and Notional embedding agreement charges`,
    
    # Set residual for status quo to 0 (it's > 0 because of rounding)
    Residual = ifelse(Residual < 1e-5, 0, Residual)
    
  ) %>% 
  
  # Turn back in to long dataset
  pivot_longer(-scenario_type, names_to = "revenue_type", values_to = "revenue_million")