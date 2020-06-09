###############################################
### Title: Calculate mean demand            ###
### Description: Use GR010 inputs to        ###
### calculate mean demand (load) by POC     ###
### and customer. Also, forecast mean       ###
### demand using growth parameters.         ###
### Date: 29 November 2019                  ###
###############################################

# Convert demand input to long form
demand_long <- demand_raw %>%
  group_by(trade_date, poc, network, year) %>% 
  summarise_at(vars(contains("TP")), sum) %>% 
  pivot_longer(
    -c(trade_date, poc, network, year),
    names_to = "trading_period",
    values_to = "demand_MW"
  ) %>%
  mutate(
    # Convert from MW to MWh
    demand_MWh = demand_MW * 0.5
  )

# Sum of demand (MWh) by year, trading period, trading date, network and POC
demand_sum <- demand_long %>%
  group_by(
    poc,
    network,
    year,
    trade_date,
    trading_period
  ) %>%
  summarise(
    demand_MWh = sum(demand_MWh)
  ) %>%
  ungroup() %>% 
  # Not all POC/Network combinations appear in 010 - append these to 
  # the data_frame with 0 generation
  union_all(
    customer_poc_network %>% 
      distinct(poc, network) %>% 
      anti_join(
        demand_long %>% 
          distinct(poc, network),
        by = c("poc", "network")
      ) %>% 
      mutate(
        demand_MWh = 0
      ) %>% 
      # Add one row for each demand year
      crossing(year = demand_years)
  )

# Mean demand (MWh) by POC and network
demand_mean_POC <- demand_sum %>%
  
  # Annual mean by POC and network
  group_by(
    poc, network
  ) %>%
  summarise(
    mean_demand_MWh = sum(demand_MWh) / length(demand_years)
  ) %>%
  ungroup() %>% 
  # Not all POC/Network combinations appear in 010 - append these to 
  # the data_frame with 0 demand (and reorder)
  union_all(
    customer_poc_network %>% 
      distinct(poc, network) %>% 
      anti_join(
        demand_sum %>% 
          distinct(poc, network),
        by = c("poc", "network")
      ) %>% 
      mutate(
        mean_demand_MWh = 0
      )
  ) %>% 
  arrange(poc, network)

# Mean demand (MWh) by customer
demand_mean <- demand_mean_POC %>%
  
  # Join on customer
  inner_join(
    customer_poc_network %>%
      select(poc, network, transpower_customer),
    by = c("poc", "network")
  ) %>%
  
  # Group by customer and sum to get total average by customer
  group_by(transpower_customer) %>%
  summarise(mean_demand_MWh = sum(mean_demand_MWh)) %>%
  ungroup() %>%
  
  # Calculate percentages of mean demand by customer
  mutate(
    mean_demand_pct = mean_demand_MWh / sum(mean_demand_MWh)
  )

# Mean demand (MWh) by customer (2021/22 forecast)
demand_mean_forecast <- demand_mean %>%
  select(-mean_demand_pct) %>%
  mutate(
    mean_demand_forecast_MWh = mean_demand_MWh * (1 + estimated_growth)^growth_years
  ) %>%
  select(-mean_demand_MWh)
