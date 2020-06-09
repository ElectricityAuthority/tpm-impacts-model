###############################################
### Title: Calculate vSPD benefits          ###
### Description: calculate demand and       ###
### generation benefits by customer.        ###
### Date: 29 November 2019                  ###
###############################################

node_benefits <- node_benefits_raw %>%
  
  # Calculate demand and generation benefits
  mutate(
    
    # Demand benefit (aka load benefit)
    demand_benefit = ifelse(deltaCS > 0, deltaCS, 0),
    
    # Generation benefit
    generation_benefit = ifelse(deltaSS > 0, deltaSS, 0)
    
  ) %>% 
  
  # Adjustment 5: set demand benefit for WPT0111 to zero
  purrr::when(
    
    adj5_flag == TRUE ~
      
      mutate(
        .,
        demand_benefit = if_else(node == "WPT0111", 0, demand_benefit)
      ),
    
    ~ .
    
  ) %>% 
  
  # Adjustment 6: set demand benefits for DOB0331 and RFN1101 to zero for 2014-15.
  # Set demand benefits for HKK0661 to zero for 2015-16
  purrr::when(
    
    adj6_flag == TRUE ~
      
      mutate(
        .,
        demand_benefit = case_when(
          node %in% c("DOB0331", "RFN1101") & modelyear == "2014_2015" ~ 0,
          node == "HKK0661" & modelyear == "2015_2016" ~ 0,
          TRUE ~ demand_benefit
        )
      ),
    
    ~ .
    
  ) %>% 
  
  # Adjustment 11: set generation benefits for COB0661 COB0 to zero 
  purrr::when(
    
    adj11_flag == TRUE ~
      
      mutate(
        .,
        generation_benefit = case_when(
          node == "COB0661 COB0" ~ 0,
          TRUE ~ generation_benefit
        )
      ),
    
    ~ .
    
  ) %>% 
  
  # Adjustment 14: Southdown generator closure - SWN2201 (remove generation benefit)
  purrr::when(
    
    adj14_flag == TRUE ~
      
      mutate(
        .,
        generation_benefit = case_when(
          node %in% c("SWN2201 SWN0", "SWN2201 SWN5") ~ 0,
          TRUE ~ generation_benefit
        )
      ),
    
    ~ .
    
  ) %>% 
  
  # Adjustment 15: Contact generator closure - OTA2202 (remove generation benefit)
  purrr::when(
    
    adj15_flag == TRUE ~
      
      mutate(
        .,
        generation_benefit = case_when(
          node == "OTA2202 OTC0" ~ 0,
          TRUE ~ generation_benefit
        )
      ),
    
    ~ .
    
  )

# Multiple customer adjustment

## Identify POCs with more than one customer
multiple_customers <- customer_poc_network %>% 
  distinct(poc, transpower_customer) %>% 
  count(poc) %>% 
  filter(n > 1) %>%
  select(-n) %>% 
  # Join records back on
  inner_join(
    customer_poc_network %>% 
      select(poc, network, transpower_customer), 
    by = "poc"
  )

## Total demand for each multi-customer POC
multi_customer_total_demand <- multiple_customers %>% 
  left_join(
    demand_sum,
    by = c("poc", "network")
  ) %>% 
  group_by(poc, network, transpower_customer) %>% 
  summarise(
    mean_demand_MWh = sum(demand_MWh, na.rm = TRUE) / length(demand_years)
  )

## Demand allocation for each customer behind each POC
multi_customer_demand_allocation <- multi_customer_total_demand %>% 
  group_by(poc) %>% 
  mutate(mean_demand_allocation = round(mean_demand_MWh / sum(mean_demand_MWh), 2)) %>% 
  # Set allocation proportion < 0.01 to 0 and > 0.99 to 1
  mutate(
    mean_demand_allocation = case_when(
      is.nan(mean_demand_allocation) ~ 0,
      mean_demand_allocation < 0.01 ~ 0,
      mean_demand_allocation > 0.99 ~ 1,
      TRUE ~ mean_demand_allocation
    ),
    
    # Hard-coded modification to get to align with released
    
    ## Assign Network Waitaki as receiving 38% of load benefit at TWZ0331 
    ## (instead of 37%. i.e. rounding up to equal 100% with other two customers)
    mean_demand_allocation = case_when(
      poc == "TWZ0331" & transpower_customer == "Network Waitaki" ~ 0.38, 
      TRUE ~ mean_demand_allocation
    )
  )

## Total generation for each multi-customer POC
multi_customer_total_generation <- multiple_customers %>% 
  left_join(
    generation_sum,
    by = c("poc", "network")
  ) %>% 
  group_by(poc, network, transpower_customer) %>% 
  summarise(
    mean_generation_MWh = sum(generation_MWh, na.rm = TRUE) / length(generation_years)
  )

## Generation allocation for each customer behind each POC
multi_customer_generation_allocation <- multi_customer_total_generation %>% 
  group_by(poc) %>% 
  mutate(mean_generation_allocation = round(mean_generation_MWh / sum(mean_generation_MWh), 2)) %>% 
  
  # Set allocation proportion < 0.01 to 0 and > 0.99 to 1
  mutate(
    
    mean_generation_allocation = case_when(
      is.nan(mean_generation_allocation) ~ 0,
      mean_generation_allocation < 0.01 ~ 0,
      mean_generation_allocation > 0.99 ~ 1,
      TRUE ~ mean_generation_allocation
    ),
    
    # Hard-coded modifications to get to align with released
    
    ## Assign The Lines Company as receiving 100% of gen benefit at TKU0331
    mean_generation_allocation = case_when(
      poc == "TKU0331" & transpower_customer == "The Lines Company" ~ 1, 
      poc == "TKU0331" & transpower_customer == "Genesis Power" ~ 0,
      TRUE ~ mean_generation_allocation
    ),
    
    ## Assign Southern Generation as receiving 100% of gen benefit at MAT1101
    mean_generation_allocation = case_when(
      poc == "MAT1101" & transpower_customer == "Southern Generation" ~ 1, 
      poc == "MAT1101" & transpower_customer == "TrustPower" ~ 0,
      TRUE ~ mean_generation_allocation
    )
  )

## Final allocation table
multi_customer_allocation <- multi_customer_demand_allocation %>%
  full_join(
    multi_customer_generation_allocation,
    by = c("poc", "network", "transpower_customer")
  ) %>% 
  # Remove POCs already split out in vSPD
  filter(!poc %in% c("HWA1101", "TUI1101", "WKM2201", "WTK0111")) %>% 
  # Replace NAs with 0s
  replace_na(list(mean_generation_MWh = 0, mean_generation_allocation = 0))

## Calculate multi-customer adjustment
multi_customer_adj <- node_benefits %>% 
  # Join on allocation
  inner_join(
    multi_customer_allocation, 
    by = c("node" = "poc")
  ) %>%  
  
  # Adjust mean benefit for generation and demand
  mutate(
    
    demand_benefit = demand_benefit * mean_demand_allocation,
    
    generation_benefit = generation_benefit * mean_generation_allocation,
    
  ) %>% 
  select(node, modelyear, node_benefit_investment, transpower_customer, 
         generation_benefit, demand_benefit, generationMWh, loadMWh) %>% 
  arrange(node, modelyear, node_benefit_investment, transpower_customer)

# Traditional vSPD by year and total (to be used as an input to the netting module)
vSPD_traditional <- node_benefits %>%
  inner_join(
    vSPD_node_customer %>% 
      select(vSPD_node, transpower_customer), by = c("node" = "vSPD_node")
  ) %>% 
  # First, remove the benefit rows for the POCs with multiple customers
  anti_join(multi_customer_adj, by = c("node", "modelyear", "node_benefit_investment", "transpower_customer")) %>%
  select(
    node, modelyear, node_benefit_investment, transpower_customer, generation_benefit, demand_benefit,
    generationMWh, loadMWh
  ) %>% 
  # Attach the re-calculated benefits for multiple customers
  union_all(multi_customer_adj) %>%
  pivot_longer(
    -c(node_benefit_investment, modelyear, transpower_customer, node, generationMWh, loadMWh), 
    names_to = "type", 
    values_to = "benefit"
  ) 

# Adjust Nga Wha if nga_wha_adjust_flag == TRUE
if(nga_wha_adjust_flag == TRUE){
  
  # Calculate Nga Wha adjustment
  source("rprogs/nga_wha_module.R")
  
  nga_wha_adj <- bind_rows(
    
    # Adjust benefits for NIGUP, HVDC and WRK
    nga_wha_adjustment(investment = "HVDC_no_reserve_FlexiNZD"),
    nga_wha_adjustment(investment = "NIGUP_Flexi_NZDperMWh"),
    nga_wha_adjustment(investment = "WRK_Flexi_NZDperMWh"),
    
    # Set benefits for other investments to 0
    node_benefits_raw %>% 
      filter(!str_detect(node_benefit_investment, "NIGUP|HVDC_no_reserve|WRK"), node == "KOE1101") %>% 
      mutate(
        demand_benefit = 0,
        generation_benefit = 0,
        transpower_customer = "Top Energy"
      ) %>% 
      select(node_benefit_investment, modelyear, transpower_customer,
             node, demand_benefit, generation_benefit)
    
  ) %>% 
    pivot_longer(-c(node_benefit_investment, modelyear, transpower_customer, node), 
                 values_to = "benefit", names_to = "type")
  
  # Nga Wha adjustment 
  # First, remove current KOE1101 benefits
  vSPD_traditional <- vSPD_traditional %>% 
    filter(
      node != "KOE1101"
    ) %>% 
    # Then, replace with re-calculated benefits
    union_all(
      nga_wha_adj
    )
  
}

source("rprogs/vSPD_netting_module.R")

# Net vSPD by year and total
vSPD_net <- vSPD_traditional %>% 
  pivot_wider(names_from = type, values_from = benefit) %>% 
  # First, remove the benefit rows for the POC/customer combinations that have been netted
  anti_join(final_net_vSPD_adjust, by = c("node", "modelyear", "node_benefit_investment", "transpower_customer")) %>% 
  # Attach the re-calculated benefits from the netting adjustment
  union_all(final_net_vSPD_adjust) %>%
  pivot_longer(
    -c(node_benefit_investment, modelyear, transpower_customer, node, generationMWh, loadMWh), 
    names_to = "type", 
    values_to = "benefit"
  )

# Take mean of benefit by investment and customer
vSPD_net_mean <- vSPD_net %>%
  group_by(node_benefit_investment, modelyear, transpower_customer, type) %>%
  summarise(benefit = sum(benefit)) %>% 
  ungroup() %>% 
  group_by(node_benefit_investment, transpower_customer, type) %>% 
  summarise( 
    # Using generation years as proxy for years used in mean for both benefit types...
    mean_benefit = sum(benefit) / length(generation_years) 
  ) %>% 
  ungroup()

# Calculate the percentage benefit for each investment by customer
vSPD_net_pct <- vSPD_net_mean %>% 
  group_by(node_benefit_investment) %>% 
  mutate(
    pct_benefit = mean_benefit / sum(mean_benefit)
  ) %>% 
  ungroup() %>% 
  # Sum percent benefit by customer and investment
  group_by(node_benefit_investment, transpower_customer) %>% 
  summarise(pct_benefit = sum(pct_benefit)) %>% 
  ungroup()

# Proposed benefit charges by customer

## Get total revenue per investment and map to vSPD output
benefit_revenue_vSPD <- benefit_revenue_forecast %>% 
  inner_join(
    investment_node_benefits_map,
    by = "investment"
  ) %>% 
  # Group by node_benefit_investment and sum in case there is more 
  # than one revenue line item per node_benefit
  group_by(node_benefit_investment) %>% 
  summarise(revenue_million = sum(revenue_million)) %>% 
  ungroup()

## Function for calculating benefit charges by investment selection
calculate_benefit_charges <- function(input_df, investment_vector){
  
  input_df %>% 
    inner_join(
      benefit_revenue_vSPD,
      by = "node_benefit_investment"
    ) %>% 
    # Filter to the specific investments that make up this proposal
    filter(
      node_benefit_investment %in% investment_vector
    ) %>% 
    group_by(transpower_customer, node_benefit_investment) %>% 
    summarise(
      total_benefit = sum(pct_benefit * revenue_million)
    ) %>% 
    ungroup()
  
}

## Final benefit charges dataframe
benefit_charges_final <- calculate_benefit_charges(
  input_df = vSPD_net_pct, 
  investment_vector = c(
    "BPE_HAY_TPM", "HVDC_no_reserve_FlexiNZD", "LSI_reliability_TPM",
    "LSI_renewables_TPM", "NIGUP_Flexi_NZDperMWh", "WRK_Flexi_NZDperMWh")
) %>% 
  group_by(transpower_customer) %>% 
  summarise(total_benefit = sum(total_benefit))

