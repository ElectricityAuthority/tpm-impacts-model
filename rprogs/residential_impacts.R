###############################################
### Title: Residential impact               ###
### Description: Calculate the impact of    ###
### the capped proposal on residential      ###
### customers.                              ###
### Date: 29 November 2019                  ###
###############################################

residential_impacts <- residential_energy_use %>%
  
  # Join on current (adjusted) charge
  inner_join(
    adjusted_charges %>% 
      filter(charge_type %in% c("HVDC charge", "Interconnection charge")) %>% 
      group_by(transpower_customer) %>% 
      summarise(charge_dollars_adjusted = sum(charge_dollars_adjusted)) %>% 
      rename(current_charge = charge_dollars_adjusted),
    by = "transpower_customer"
  ) %>% 
  
  # Join on capped proposal
  inner_join(
    capping_final %>% 
      select(transpower_customer, capped_proposal),
    by = "transpower_customer"
  ) %>% 
  
  # Join on forecast mean demand
  inner_join(
    demand_mean_forecast,
    by = "transpower_customer"
  ) %>% 
  
  # Calculate current and proposed $/MWh
  mutate(
    current_charge_dollarsperMWh = current_charge / mean_demand_forecast_MWh,
    capped_proposal_dollarsperMWh = capped_proposal / mean_demand_forecast_MWh
  ) %>% 
  
  # Calculate current and proposed TPM impact on residential bill ($)
  mutate(
    current_impact = current_charge_dollarsperMWh / 1000 * average_use_kWh,
    proposed_impact = capped_proposal_dollarsperMWh / 1000 * average_use_kWh,
    impact_diff = proposed_impact - current_impact
  ) %>% 
  
  # Attach ACOT $/MWh
  left_join(
    ACOT_payments_dollarsperMWh %>% 
      select(transpower_customer, ACOT_dollarsperMWh),
    by = "transpower_customer"
  ) %>% 
  
  # Fill ACOT $/MWh with 0 where NA
  mutate_at(vars(ACOT_dollarsperMWh), ~ coalesce(., 0)) %>% 
  
  # Calculate ACOT impact on residential bill ($)
  mutate(
    ACOT_impact = ACOT_dollarsperMWh / 1000 * average_use_kWh,
    total_impact_no_ACOT = impact_diff - ACOT_impact
  )


# write_csv(residential_impacts, "results/residential_impacts.csv")

