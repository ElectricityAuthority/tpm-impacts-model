###############################################
### Title: Comparisons                      ###
### Description: Compare calculated inputs, ###
### intermediate datasets, and results with ###
### released versions                       ###
### Date: 29 November 2019                  ###
###############################################

# Update 14 April 2020: these comparisons will be correct when all adjustment flags are set to FALSE

library(testthat)

# Set tolerance parameter
tolerance_param <- 0.0001 # i.e. within 0.01%
# tolerance_param <- 0 # i.e. no tolerance for difference

# Compare TPM results/intermediate datasets (function)
compare_TPM_results <- function(calculated_df, released_csv, tol = tolerance_param){
  
  calculated <- calculated_df %>% 
    # Needs to be coerced to data.frame for expect_equal to work
    data.frame()
  
  released <- read_csv(paste0("compare/", released_csv,".csv"), col_types = cols()) %>% 
    # Needs to be coerced to data.frame for expect_equal to work
    data.frame()
  
  return(
    expect_equal(
      released, 
      calculated, 
      tolerance = tol,
      label = released_csv,
      expected.label = paste0(deparse(substitute(calculated_df)))
    )
  )
  
}

# Forecast revenue
compare_TPM_results(
  calculated_df = total_revenue_forecast, 
  released_csv = "total_revenue_released"
)

# Mean demand by POC and network
compare_TPM_results(
  calculated_df = demand_mean_POC, 
  released_csv = "demand_mean_POC_released"
)

# Adjusted charges (2022 revenue)
adjusted_charges_calculated <- adjusted_charges %>% 
  select(-charge_dollars) %>% 
  pivot_wider(names_from = charge_type, values_from = charge_dollars_adjusted)

compare_TPM_results(
  calculated_df = adjusted_charges_calculated, 
  released_csv = "adjusted_charges_released"
)

# Mean demand by customer
demand_mean_calculated <- demand_mean %>% select(-mean_demand_pct)

compare_TPM_results(
  calculated_df = demand_mean_calculated, 
  released_csv = "demand_mean_customer_released"
)

# Mean generation by POC and network
compare_TPM_results(
  calculated_df = generation_mean_POC, 
  released_csv = "generation_mean_POC_released"
)

# Mean generation (less net gen) by customer
compare_TPM_results(
  calculated_df = generation_mean_net, 
  released_csv = "generation_mean_net_customer_released"
)

# Forecast demand (2021-22) by customer
compare_TPM_results(
  calculated_df = demand_mean_forecast, 
  released_csv = "demand_mean_forecast_customer_released"
)

# Forecast (net) generation (2021-22) by customer
compare_TPM_results(
  calculated_df = generation_mean_net_forecast, 
  released_csv = "generation_mean_forecast_net_customer_released"
)

# Residual final
compare_TPM_results(
  calculated_df = residual_final %>% select(transpower_customer, residual_charge), 
  released_csv = "residual_impact_released"
)

# vSPD 

## Nga Wha adjustment
nga_wha_adj_calculated <- nga_wha_adj %>% 
  arrange(node_benefit_investment, modelyear, type) %>% 
  # Filter out HVDC with reserve as it's not used in the final calculations
  # (and vSPD output wasn't provided for it).
  filter(!node_benefit_investment == "HVDC_with_reserve_FlexiNZD"
  )

compare_TPM_results(
  calculated_df = nga_wha_adj_calculated, 
  released_csv = "nga_wha_adjustment_released"
)

## Multi customer adjustment
multi_customer_adj_calculated <- multi_customer_adj %>% 
  # Remove POCs that 'fall below the 1% threshold' to align with released table
  filter(!node %in% c("HWB0331", "PEN0331", "TNG0111", "WHI0111")) %>% 
  # Remove loadMWh and generationMWh columns
  select(-c(loadMWh, generationMWh))

compare_TPM_results(
  calculated_df = multi_customer_adj_calculated, 
  released_csv = "multi_customer_adjustment_released"
)

## Net vSPD
vSPD_net_mean_calculated <- vSPD_net_mean %>%
  pivot_wider(names_from = type, values_from = mean_benefit) %>%
  # Rearrange column order
  select(node_benefit_investment, transpower_customer, generation_benefit, demand_benefit) %>%
  # Filter out HVDC with no reserve (as no released results and not used in the end)
  # Also, filter out SouthPark Utilities for same reason
  filter(!node_benefit_investment == "HVDC_with_reserve_FlexiNZD",
         !transpower_customer == "Southpark Utilities")

compare_TPM_results(
  calculated_df = vSPD_net_mean_calculated,
  released_csv = "vSPD_net_mean_released"
)

# Benefit charges
compare_TPM_results(
  calculated_df = benefit_charges_final, 
  released_csv = "benefit_charges_released"
)

# Indicative charges

## Proposal
proposal_calculated <- indicative_charges %>% 
  select(transpower_customer, proposal) %>% 
  arrange(transpower_customer)

compare_TPM_results(
  calculated_df = proposal_calculated, 
  released_csv = "proposal_released"
)

# ACOT impacts
ACOT_impact_calculated <- ACOT_payments_dollarsperMWh %>% select(transpower_customer, ACOT_dollarsperMWh)

compare_TPM_results(
  calculated_df = ACOT_impact_calculated, 
  released_csv = "ACOT_impacts_released"
)

# Capping
capping_final_calculated <- capping_final %>% 
  select(transpower_customer, capped_proposal) %>% 
  arrange(transpower_customer)

compare_TPM_results(
  calculated_df = capping_final_calculated, 
  released_csv = "capped_proposal_released"
)

# Residential impacts
residential_impacts_calculated <- residential_impacts %>% 
  select(
    transpower_customer, current_impact, proposed_impact, 
    impact_diff, total_impact_no_ACOT
  ) %>% 
  arrange(transpower_customer)

compare_TPM_results(
  calculated_df = residential_impacts_calculated, 
  released_csv = "residential_impact_released"
  
)


