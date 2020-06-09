###############################################
### Title: ACOT payments                    ###
### Description: Calculate ACOT $/MWh.      ###
### Date: 29 November 2019                  ###
###############################################

ACOT_payments_dollarsperMWh <- ACOT_payments %>%
  # Join on forecast demand
  inner_join(
    demand_mean_forecast,
    by = "transpower_customer"
  ) %>%
  # Calculate ACOT $/MWh
  mutate(
    ACOT_dollarsperMWh = ACOT_payment / mean_demand_forecast_MWh
  )
