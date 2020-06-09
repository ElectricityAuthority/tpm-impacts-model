###############################################
### Title: Comparisons  (March adjustments) ###
### Description: Compare calculated inputs, ###
### intermediate datasets, and results with ###
### (updated) Excel version                 ###
### Date: 14 April 2020                     ###
###############################################

# Note: comparisons will be correct when all adjustment flags are set to TRUE

library(testthat)

# Set tolerance parameter
tolerance_param <- 0.0001 # i.e. within 0.01%
# tolerance_param <- 0 # i.e. no tolerance for difference

# Compare TPM results/intermediate datasets (function)
compare_TPM_results_adj <- function(calculated_df, excel_df, tol = tolerance_param) {
    calculated <- calculated_df %>%
      # Needs to be coerced to data.frame for expect_equal to work
      data.frame()
    
    excel <-
      read_csv(paste0("compare/adjusted/", excel_df, ".csv"),
               col_types = cols()) %>%
      # Needs to be coerced to data.frame for expect_equal to work
      data.frame()
    
    return(
      expect_equal(
        excel,
        calculated,
        tolerance = tol,
        label = excel_df,
        expected.label = paste0(deparse(substitute(calculated_df)))
      )
    )
    
  }

# Forecast revenue
compare_TPM_results_adj(calculated_df = total_revenue_forecast,
                        excel_df = "total_revenue_excel")

# Mean demand by POC and network
compare_TPM_results_adj(calculated_df = demand_mean_POC,
                        excel_df = "demand_mean_POC_excel")

# Adjusted charges (2022 revenue)
adjusted_charges_calculated <- adjusted_charges %>%
  select(-charge_dollars) %>%
  pivot_wider(names_from = charge_type, values_from = charge_dollars_adjusted) %>%
  mutate_if(is.numeric, replace_na, 0)

compare_TPM_results_adj(calculated_df = adjusted_charges_calculated,
                        excel_df = "adjusted_charges_excel")

# Mean demand by customer
demand_mean_calculated <- demand_mean %>%
  select(-mean_demand_pct)

compare_TPM_results_adj(calculated_df = demand_mean_calculated,
                        excel_df = "demand_mean_customer_excel")

# Mean generation by POC and network
compare_TPM_results_adj(calculated_df = generation_mean_POC,
                        excel_df = "generation_mean_POC_excel")

# Mean generation (less net gen) by customer
compare_TPM_results_adj(calculated_df = generation_mean_net,
                        excel_df = "generation_mean_net_customer_excel")

# Forecast demand (2021-22) by customer
compare_TPM_results_adj(calculated_df = demand_mean_forecast,
                        excel_df = "demand_mean_forecast_customer_excel")

# Forecast (net) generation (2021-22) by customer
compare_TPM_results_adj(calculated_df = generation_mean_net_forecast,
                        excel_df = "generation_mean_forecast_net_customer_excel")

# Residual final
compare_TPM_results_adj(
  calculated_df = residual_final %>% select(transpower_customer, residual_charge),
  excel_df = "residual_impact_excel"
)

# vSPD

## Nga Wha adjustment (sum across years)
nga_wha_adj_calculated <- nga_wha_adj %>%
  arrange(node_benefit_investment, modelyear, type) %>%
  # Filter out HVDC with reserve as it's not used in the final calculations
  # (and vSPD output wasn't provided for it).
  filter(!node_benefit_investment == "HVDC_with_reserve_FlexiNZD"
  ) %>%
  group_by(node_benefit_investment, transpower_customer, node, type) %>%
  summarise(benefit = sum(benefit)) %>%
  ungroup()

compare_TPM_results_adj(calculated_df = nga_wha_adj_calculated,
                        excel_df = "nga_wha_adjustment_excel")

## Multi customer adjustment
multi_customer_adj_calculated <- multi_customer_adj %>%
  # Remove POCs that 'fall below the 1% threshold' to align with Excel table
  filter(!node %in% c("HWB0331", "PEN0331", "TNG0111", "WHI0111")) %>%
  # Remove loadMWh and generationMWh columns
  select(-c(loadMWh, generationMWh))

compare_TPM_results_adj(calculated_df = multi_customer_adj_calculated,
                        excel_df = "multi_customer_adjustment_excel")

## Net vSPD
vSPD_net_mean_calculated <- vSPD_net_mean %>%
  pivot_wider(names_from = type, values_from = mean_benefit) %>%
  # Rearrange column order
  select(node_benefit_investment,
         transpower_customer,
         generation_benefit,
         demand_benefit) %>%
  # Filter out HVDC with no reserve (as no Excel results and not used in the end)
  # Also, filter out SouthPark Utilities for same reason
  filter(
    !node_benefit_investment == "HVDC_with_reserve_FlexiNZD",
    !transpower_customer == "Southpark Utilities"
  )

compare_TPM_results_adj(calculated_df = vSPD_net_mean_calculated,
                        excel_df = "vSPD_net_mean_excel")

# Benefit charges
compare_TPM_results_adj(
  calculated_df = benefit_charges_final,
  excel_df = "benefit_charges_excel"
)

# Indicative charges

## Proposal
proposal_calculated <- indicative_charges %>%
  select(transpower_customer, proposal) %>%
  arrange(transpower_customer) 

compare_TPM_results_adj(calculated_df = proposal_calculated,
                        excel_df = "proposal_excel")

# ACOT impacts
ACOT_impact_calculated <- ACOT_payments_dollarsperMWh %>%
  select(transpower_customer, ACOT_dollarsperMWh)

compare_TPM_results_adj(calculated_df = ACOT_impact_calculated,
                        excel_df = "ACOT_impacts_excel")

# Capping
capping_final_calculated <- capping_final %>%
  select(transpower_customer, capped_proposal) %>% 
  arrange(transpower_customer)

compare_TPM_results_adj(calculated_df = capping_final_calculated,
                        excel_df = "capped_proposal_excel")

# Residential impacts
residential_impacts_calculated <- residential_impacts %>%
  select(
    transpower_customer,
    current_impact,
    proposed_impact,
    impact_diff,
    total_impact_no_ACOT
  ) %>%
  arrange(transpower_customer)

compare_TPM_results_adj(calculated_df = residential_impacts_calculated,
                        excel_df = "residential_impact_excel")
