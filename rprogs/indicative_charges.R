###############################################
### Title: Indicative charges               ###
### Description: Calculate indicative       ###
### charges (status quo vs. proposed) for   ###
### all combinations.                       ###
### Date: 29 November 2019                  ###
###############################################

# Calculate indicative charges
indicative_charges <- adjusted_charges %>% 
  
  # Remove the non-adjusted dollars column
  select(-charge_dollars) %>% 
  # Filter to connection charge only
  filter(charge_type == "Connection charge") %>% 
  # Remove charge_type column
  select(-charge_type) %>% 
  # Join on customer type
  inner_join(
    customer_type,
    by = "transpower_customer"
  ) %>% 
  
  # Join on the benefit amounts
  inner_join(
    benefit_charges_final,
    by = "transpower_customer"
  ) %>% 
  
  # Multiply benefit by $1 million
  mutate(
    total_benefit = total_benefit * 1000000
  ) %>% 
  
  # Attach residual amount
  inner_join(
    residual_final,
    by = "transpower_customer"
  ) %>% 
  
  # Multiply residual by $1 million
  mutate(
    residual_charge = residual_charge * 1000000
  ) %>% 
  
  # Calculate proposal (benefit charge + residual charge)
  mutate(
    proposal = total_benefit + residual_charge
  )

# write_csv(indicative_charges, "results/indicative_charges.csv")
