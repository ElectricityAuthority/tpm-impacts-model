###############################################
### Title: Calculate residual charges       ###
### Description: Calculate AMD in order to  ###
### calculate residual charges for each     ###
### customer.                               ###
### Date: 29 November 2019                  ###
###############################################

# Anytime max demand (AMD) by year, POC and network
demand_AMD <- demand_sum %>% 
  
  # Adjustment 5: group ORO1101 and ORO1102 together and recalculate AMD.
  # Easiest way is to change ORO1102 to ORO1101 (or vice-versa) and re-aggregate.
  purrr::when(
    
    adj5_flag == TRUE ~ 
      mutate(
        .,
        poc = if_else(poc == "ORO1102", "ORO1101", poc)
      ) %>% 
      group_by(poc, network, year, trade_date, trading_period) %>% 
      summarise(demand_MWh = sum(demand_MWh)) %>% 
      ungroup(),
    
    ~ .
    
  ) %>% 
  
  # Adjustment 6: group RFN POCs together and recalculate AMD (for latest 2 years).
  # Easiest way is to change RFN1102 to RFN1101 (or vice-versa) and re-aggregate.
  # Also, subset all Westpower POCs to latest 2 years
  purrr::when(
    
    adj6_flag == TRUE ~ 
      
      filter(
        .,
        !(network == "WPOW" & year %in% c("20140701_20150630", "20150701_20160630"))) %>% 
      mutate(
        poc = if_else(poc == "RFN1102", "RFN1101", poc)
      ) %>%
      group_by(poc, network, year, trade_date, trading_period) %>% 
      summarise(demand_MWh = sum(demand_MWh)) %>% 
      ungroup(),
    
    ~ .
    
  ) %>% 
  
  group_by(
    poc,
    network,
    year
  ) %>% 
  summarise(
    AMD = max(demand_MWh)
  ) %>% 
  ungroup() %>% 
  
  # Adjustment 9: adjust Eastland Network AMDs (WRA0111, GIS0501 and TUI0111)
  purrr::when(
    
    adj9_flag == TRUE ~
      
      mutate(
        .,
        AMD = case_when(
          poc %in% c("WRA0111", "GIS0501", "TUI0111") ~ 0,
          TRUE ~ AMD
        )
      ),
    
    ~ .
    
  ) %>% 
  
  # Adjustment 10: adjust Northpower AMD (KEN0331) - set to zero
  purrr::when(
    
    adj10_flag == TRUE ~
      
      mutate(
        .,
        AMD = case_when(
          poc %in% c("KEN0331") ~ 0,
          TRUE ~ AMD
        )
      ),
    
    ~ .
    
  ) %>% 
  
  # Adjustment 12: adjust Orion AMDs for 2014-15 (ADD0111, ADD0661, 
  # MLN0661 and MLN0664)
  purrr::when(

    adj12_flag == TRUE ~

      mutate(
        .,
        AMD = case_when(
          poc %in% c("ADD0111", "ADD0661", "MLN0661", "MLN0664") ~ 0,
          TRUE ~ AMD
        )
      ),

    ~ .

  )

# Mean AMD by POC and network
mean_demand_AMD <- demand_AMD %>% 
  
  group_by(poc, network) %>%
  
  # Adjustment 6: change the denominator for Westpower POCs to 2 for calculating AMD
  purrr::when(
    
    adj6_flag == TRUE ~
      
      mutate(
        ., 
        denom = if_else(network == "WPOW", 2L, length(demand_years)),
        AMD = AMD /denom
      ) %>% 
      
      summarise(
        mean_AMD = sum(AMD)
      ),
    
    summarise(
      ., 
      mean_AMD = sum(AMD) / length(demand_years)
    )
    
  ) %>% 
  
  ungroup() %>% 
  
  # Join on customer
  inner_join(
    customer_poc_network %>% 
      select(poc, network, transpower_customer),
    by = c("poc", "network")
  ) 


# Total mean AMD by customer 
mean_demand_AMD_customer <- mean_demand_AMD %>% 
  
  group_by(transpower_customer) %>% 
  summarise(mean_AMD = sum(mean_AMD)) %>% 
  ungroup() %>%
  
  # Calculate percentages of mean AMD by customer
  mutate(
    mean_AMD_pct = mean_AMD / sum(mean_AMD)
  )

# Get total residual for proposal from total_revenue_forecast
total_residual <- total_revenue_forecast %>%
  filter(scenario_type == "proposed", revenue_type == "Residual") %>% 
  select(revenue_million) %>% 
  as.numeric()

# Create final residual table
residual_final <- mean_demand_AMD_customer %>%
  mutate(
    residual_charge = total_residual * mean_AMD_pct
  ) %>% 
  # Adjustment 2: add residual charges (of 0) to KCE (Mangahao)
  purrr::when(
    adj2_flag == TRUE ~ 
      union_all(
        .,
        crossing(
          transpower_customer = "KCE (Mangahao)",
          residual_charge = 0
        )
      ),
    
    ~ .
  )
