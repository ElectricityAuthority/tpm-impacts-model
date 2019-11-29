###############################################
### Title: Calculate mean generation        ###
### Description:                            ###
### Date: 29 November 2019                  ###
###############################################


# Convert to long form
generation_long <- generation_raw %>%
  pivot_longer(
    -c(trade_date, poc, network, seller, year), 
    names_to = "trading_period", 
    values_to = "generation_MW") %>%
  # Convert from MW to MWh
  mutate(
    generation_MWh = generation_MW / 2
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
  ungroup()

# Mean generation (MWh) by customer
generation_mean <- generation_sum %>% 
  group_by(poc, network) %>% 
  summarise(
    # mean_generation_MWh = mean(generation_MWh)
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
  ungroup() %>% 
  
  # Calculate percentages of mean generation by customer
  mutate(
    mean_generation_pct = mean_generation_MWh / sum(mean_generation_MWh)
  )