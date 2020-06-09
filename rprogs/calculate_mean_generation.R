###############################################
### Title: Calculate mean generation        ###
### Description: Use GR010 inputs to        ###
### calculate mean generation by POC and    ###
### customer. Also, forecast mean net       ###
### generation using growth parameters.     ###
### Date: 29 November 2019                  ###
###############################################

# Convert generation input to long form
generation_long <- generation_raw %>%
  group_by(trade_date, poc, network, year) %>% 
  summarise_at(vars(contains("TP")), sum) %>% 
  pivot_longer(
    -c(trade_date, poc, network, year),
    names_to = "trading_period",
    values_to = "generation_MW") %>%
  # Convert from MW to MWh
  mutate(
    generation_MWh = generation_MW * 0.5
  )

# Sum of generation (MWh) by year, network and POC
generation_sum <- generation_long %>%
  group_by(
    poc,
    network,
    year
  ) %>%
  summarise(
    generation_MWh = sum(generation_MWh)
  ) %>%
  ungroup() %>% 
  # Not all POC/Network combinations appear in 010 - append these to 
  # the data_frame with 0 generation
  union_all(
    customer_poc_network %>% 
      distinct(poc, network) %>% 
      anti_join(
        generation_long %>% 
          distinct(poc, network),
        by = c("poc", "network")
      ) %>% 
      mutate(
        generation_MWh = 0
      ) %>% 
      # Add one row for each generation year
      crossing(year = generation_years)
  )

# Mean generation (MWh) by POC and network
generation_mean_POC <- generation_sum %>%
  
  group_by(poc, network) %>%
  summarise(
    mean_generation_MWh = sum(generation_MWh) / length(generation_years)
  ) %>%
  ungroup()

# Mean generation (MWh) by customer
generation_mean <- generation_mean_POC %>%
  
  # Join on customer
  inner_join(
    customer_poc_network %>%
      select(poc, network, transpower_customer),
    by = c("poc", "network")
  ) %>%
  
  # Group by customer and sum to get total average by customer
  group_by(transpower_customer) %>%
  summarise(mean_generation_MWh = sum(mean_generation_MWh)) %>%
  ungroup() %>%

  # Calculate percentages of mean generation by customer
  mutate(
    mean_generation_pct = mean_generation_MWh / sum(mean_generation_MWh)
  )

# Mean generation (MWh) by customer (2021/22 forecast)

## First, get the average netted generation
generation_mean_net <- generation_sum %>%
  # Join on DG_flag
  inner_join(
    customer_poc_network %>% 
      select(poc, network, DG_flag),
    by = c("poc", "network")
  ) %>%
  
  # Set generation to 0 for DG
  mutate(
    generation_MWh = ifelse(DG_flag == "DG", 0, generation_MWh)
  ) %>% 
  group_by(poc, network) %>%
  summarise(
    mean_generation_MWh = sum(generation_MWh) / length(generation_years)
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
  summarise(mean_generation_MWh = sum(mean_generation_MWh)) %>%
  ungroup()

# Forecast using growth rate and growth years
generation_mean_net_forecast <- generation_mean_net %>%
  mutate(
    mean_generation_forecast_MWh = mean_generation_MWh * (1 + estimated_growth)^growth_years
  ) %>%
  select(-mean_generation_MWh)

