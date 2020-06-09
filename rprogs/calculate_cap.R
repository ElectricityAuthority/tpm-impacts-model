###############################################
### Title: Calculate cap                    ###
### Description: Calculate capped proposal  ###
### for each customer.                      ###
### Date: 29 November 2019                  ###
###############################################

# Calculate the total LCE allocation minus the allocation for connection charge
LCE_allocation_minus_connection_charge <- LCE_allocation %>% 
  filter(charge_type != "Connection") %>% 
  summarise(allocation_million = sum(allocation_million)) %>% 
  pull(allocation_million)

# Calculate the 'current' HVDC+Interconnection charge for each customer
HVDC_interconnection_charge <- current_charges %>% 
  filter(charge_type %in% c("HVDC charge", "Interconnection charge")) %>% 
  group_by(transpower_customer) %>% 
  summarise(charge_dollars = sum(charge_dollars)) %>%
  
  # Adjustment 8: change to ensure that status quo is net of LCE
  purrr::when(
    adj8_flag == TRUE ~
      mutate(
        ., 
        total_charge = sum(charge_dollars), 
        total_charge_minus_LCE = total_charge - (LCE_allocation_minus_connection_charge * 1000000),
        charge_dollars = charge_dollars * (total_charge_minus_LCE / total_charge)
      ) %>% 
      select(-c(total_charge, total_charge_minus_LCE)),
    
    ~ .
  )

# Distributor capping
distributor_capping <- distributor_charges %>% 
  
  # Attach forecast demand for each customer
  inner_join(
    demand_mean_forecast,
    by = "transpower_customer"
  ) %>% 
  
  # Join on (non-capped) proposal
  inner_join(
    indicative_charges %>% 
      select(transpower_customer, proposal),
    by = "transpower_customer"
  ) %>% 
  
  # Attach total current (HVDC+Interconnection) charge
  inner_join(
    HVDC_interconnection_charge %>%
      rename(HVDC_interconnection_charge = charge_dollars),
    by = "transpower_customer"
  ) %>% 
  mutate(
    
    energy_cost = energy_cost_estimated * mean_demand_forecast_MWh,
    total_cost = line_charge + energy_cost,
    total_cost_dollarsperMWh = total_cost / mean_demand_forecast_MWh,
    EDB_charge_proposal = line_charge - HVDC_interconnection_charge,
    lines_charge_proposal = EDB_charge_proposal + proposal,
    total_cost_w_TPM_proposal = lines_charge_proposal + energy_cost,
    total_cost_w_TPM_proposal_dollarsperMWh = total_cost_w_TPM_proposal / mean_demand_forecast_MWh
    
  )

# Industrials capping
industrial_capping <- demand_mean_forecast %>% 
  semi_join(
    customer_type %>% 
      filter(type == "Industrial"),
    by = "transpower_customer") %>% 
  # Attach HVDC+Interconnection charge
  inner_join(
    HVDC_interconnection_charge %>% 
      rename(HVDC_interconnection_charge = charge_dollars),
    by = "transpower_customer"
  ) %>% 
  # Attach adjusted connection charge
  inner_join(
    connection_charge_adj %>% 
      select(transpower_customer, charge_dollars_adjusted) %>% 
      rename(connection_charge_adj = charge_dollars_adjusted),
    by = "transpower_customer"
  ) %>%
  
  # Join on (non-capped) proposal
  inner_join(
    indicative_charges %>% 
      select(transpower_customer, proposal),
    by = "transpower_customer"
  ) %>% 
  mutate(
    
    # Current HVDC+Interconnection charge + adjusted connection charge divided by forecast demand
    HVDCInt_current_conn_adj_dollarsperMWh = (HVDC_interconnection_charge + connection_charge_adj) / mean_demand_forecast_MWh,
    
    # Proposal + adjusted connection charge divided by forecast demand
    proposal_adj_conn_dollarsperMWh = (proposal + connection_charge_adj) / mean_demand_forecast_MWh,
    
    # Energy cost ($)
    energy_cost = energy_cost_estimated * mean_demand_forecast_MWh,
    
    # Electricity cost with status quo TPM ($/MWh)
    electricity_cost_SQ_TPM_dollarsperMWh = HVDCInt_current_conn_adj_dollarsperMWh + energy_cost_estimated,
    
    # Electricity cost with proposed TPM ($/MWh)
    electricity_cost_proposed_dollarsperMWh = proposal_adj_conn_dollarsperMWh + energy_cost_estimated,
    
    # Electricity cost with status quo TPM ($)
    electricity_cost_SQ_TPM = electricity_cost_SQ_TPM_dollarsperMWh * mean_demand_forecast_MWh,
    
    # Electricity cost with proposed TPM ($)
    electricity_cost_proposed = electricity_cost_proposed_dollarsperMWh * mean_demand_forecast_MWh
  )

# Final capping calculations
capping_final <- customer_type %>%
  
  # Join on demand forecast
  left_join(
    demand_mean_forecast,
    by = "transpower_customer"
  ) %>% 
  
  # Join on generation forecast (net)
  left_join(
    generation_mean_net_forecast,
    by = "transpower_customer"
  ) %>% 
  
  # Join on distributor total bill
  left_join(
    distributor_capping %>% 
      select(transpower_customer, total_cost) %>% 
      rename(distributor_bill = total_cost),
    by = c("transpower_customer")
  ) %>% 
  
  # Join on industrial bill
  left_join(
    industrial_capping %>% 
      select(
        transpower_customer, 
        electricity_cost_SQ_TPM) %>% 
      rename(industrial_bill = electricity_cost_SQ_TPM),
    by = c("transpower_customer")
  ) %>% 
  
  # Join together in to total bill
  mutate(
    total_bill = case_when(
      type == "Distributor" ~ distributor_bill,
      type == "Industrial" ~ industrial_bill,
      type == "Generator" ~ 0,
      TRUE ~ 0
    )
  ) %>% 
  
  # Drop individual bills
  select(-c(distributor_bill, industrial_bill)) %>% 
  
  # Attach total current (HVDC+Interconnection) charge
  left_join(
    HVDC_interconnection_charge %>%
      rename(current_charges_total = charge_dollars),
    by = "transpower_customer"
  ) %>%
  
  # Join on (non-capped) proposal
  inner_join(
    indicative_charges %>% 
      select(transpower_customer, proposal),
    by = c("transpower_customer")
  ) %>% 
  
  mutate(
    cap_on_increase = total_bill * 0.035,
    increase = proposal - current_charges_total,
    cap_breach = ifelse(
      increase > cap_on_increase & type %in% c("Distributor", "Industrial"), 
      increase - cap_on_increase, 0),
    capped_charges_before_funding_cap = proposal - cap_breach,
    dollars_until_cap_reached = ifelse(
      (cap_on_increase - increase) < 0 | type == "Generator", 0, cap_on_increase - increase),
    proposal_pct = proposal / sum(proposal),
    proposal_pct_after_cap = ifelse(cap_breach > 0, 0, proposal_pct),
    cap_funding_round1 = ifelse(
      cap_breach > 0, 0, sum(cap_breach) * proposal_pct_after_cap / sum(proposal_pct_after_cap)
    ),
    cap_funding_round1_adj = case_when(
      type == "Generator" ~ cap_funding_round1,
      type %in% c("Distributor", "Industrial") ~ ifelse(
        cap_funding_round1 > dollars_until_cap_reached, 
        dollars_until_cap_reached, cap_funding_round1),
      TRUE ~ 0
    ) ,
    capped_proposal = proposal - cap_breach + cap_funding_round1_adj,
    cap_proposal_diff = capped_proposal - proposal
  ) 

# write_csv(capping_final, "results/capping_final.csv")
