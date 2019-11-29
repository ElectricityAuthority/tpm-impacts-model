###############################################
### Title: Calculate mean demand            ###
### Description:                            ###
### Date: 29 November 2019                  ###
###############################################

# Convert to long form
demand_long <- demand_raw %>%
  pivot_longer(
    -c(trade_date, poc, network, purchaser, year), 
    names_to = "trading_period", 
    values_to = "demand_MW"
  ) %>%
  mutate(
    # Convert from MW to MWh
    demand_MWh = demand_MW / 2
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
  ungroup()

# Mean demand (MWh) by customer
demand_mean <- demand_sum %>% 
  
  # First, sum MWh by POC, network and year
  group_by(poc, network, year) %>% 
  summarise(
    demand_MWh = sum(demand_MWh)
  ) %>% 
  ungroup() %>% 
  
  # Then take the annual mean by POC and network
  group_by(
    poc, network
  ) %>% 
  summarise(
    # mean_demand_MWh = mean(demand_MWh)
    mean_demand_MWh = sum(demand_MWh) / length(demand_years)
  ) %>% 
  ungroup() %>% 
  
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
    # mean_demand_pct = scales::percent(mean_demand_MWh / sum(mean_demand_MWh))
    mean_demand_pct = mean_demand_MWh / sum(mean_demand_MWh)
  )

# Mean demand (MWh) by customer (2021/22 forecast)
## NOTE: this could/should parameterised to be any year in the future

estimated_load_growth <- 0.01 # (i.e. 1% pa). This should be moved to the top at some point?
load_growth_years <- as.numeric(difftime(ymd("20220630"), ymd("20160630")) / 365)

demand_mean_forecast <- demand_mean %>% 
  select(-mean_demand_pct) %>% 
  mutate(
    mean_demand_forecast_MWh = mean_demand_MWh * (1 + estimated_load_growth)^load_growth_years 
  ) %>% 
  select(-mean_demand_MWh)