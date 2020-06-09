###############################################
### Title: Read inputs                      ###
### Description: Read in all input files.   ###
### Date: 29 November 2019                  ###
###############################################

###############
### Mapping ###
###############

## POC/Network/Customer concordance
customer_poc_network <- read_csv(
  paste0(
    mapping_path, 
    "customer_poc_network_map.csv"
  ),
  col_types = cols()
) %>% 
  # Adjustment 1: change customer name from "Tuaropaki Power" to "Tuaropaki (Mercury)"
  purrr::when(
    adj1_flag == TRUE ~ 
      mutate(
        .,
        transpower_customer = if_else(
          transpower_customer == "Tuaropaki Power",
          "Tuaropaki (Mercury)",
          transpower_customer
        )
      ),
    
    ~ .
    
  ) %>% 
  # Adjustment 4: update customer names to Horizon Energy
  purrr::when(
    adj4_flag == TRUE ~ 
      mutate(
        .,
        transpower_customer = if_else(
          transpower_customer %in% c("Southern Generation") & poc == "MAT1101",
          "Horizon Energy",
          transpower_customer
        )
      ),
    
    ~ .
  )

## vSPD node to customer concordance
vSPD_node_customer <- read_csv(
  paste0(
    mapping_path, 
    "vSPD_node_to_customer_map.csv"
  ),
  col_types = cols()
) %>% 
  # Adjustment 1: change customer name from "Tuaropaki Power" to "Tuaropaki (Mercury)"
  purrr::when(
    adj1_flag == TRUE ~ 
      mutate(
        .,
        transpower_customer = if_else(
          transpower_customer == "Tuaropaki Power",
          "Tuaropaki (Mercury)",
          transpower_customer
        )
      ),
    
    ~ .
    
  ) %>% 
  # Adjustment 2: update customer name at MHO0331 from Nova to KCE (Mangahao)
  purrr::when(
    adj2_flag == TRUE ~ 
      mutate(
        .,
        transpower_customer = if_else(
          transpower_customer == "Nova" & poc == "MHO0331",
          "KCE (Mangahao)",
          transpower_customer
        )
      ),
    
    ~ .
    
  ) %>% 
  # Adjustment 4: update customer names to Horizon Energy
  purrr::when(
    adj4_flag == TRUE ~ 
      mutate(
        .,
        transpower_customer = if_else(
          transpower_customer %in% c("Southern Generation") & vSPD_node == "MAT1101",
          "Horizon Energy",
          transpower_customer
        )
      )%>% 
      distinct(), # This collapses the MAT1101 node to a single row for Horizon
    
    ~ .
    
  )


## Customer/type concordance
customer_type <- read_csv(
  paste0(
    mapping_path, 
    "customer_type_map.csv"
  ),
  col_types = cols()
) %>% 
  # Adjustment 1: change customer name from "Tuaropaki Power" to "Tuaropaki (Mercury)"
  purrr::when(
    adj1_flag == TRUE ~ 
      mutate(
        .,
        transpower_customer = if_else(
          transpower_customer == "Tuaropaki Power",
          "Tuaropaki (Mercury)",
          transpower_customer
        )
      ),
    
    ~ .
    
  ) %>% 
  # Adjustment 2: add KCE (Mangahao) to customer type dataframe as a generator
  purrr::when(
    adj2_flag == TRUE ~ 
      add_row(
        ., 
        transpower_customer = "KCE (Mangahao)",
        type = "Generator"
      ),
    
    ~ .
  )

## Investment revenue to node benefit names names map
investment_node_benefits_map <- read_csv(
  paste0(
    mapping_path, 
    "investment_node_benefits_map.csv"
  ),
  col_types = cols()
)

###############
### Charges ###
###############

## Current charges (2021/22)
current_charges <- read_csv(
  paste0(
    charges_path, 
    "current_charges.csv"
  ),
  col_types = cols()
) %>% 
  # Adjustment 1: change customer name from "Tuaropaki Power" to "Tuaropaki (Mercury)"
  purrr::when(
    adj1_flag == TRUE ~ 
      mutate(
        .,
        transpower_customer = if_else(
          transpower_customer == "Tuaropaki Power",
          "Tuaropaki (Mercury)",
          transpower_customer
        )
      ),
    
    ~ .
    
  )

## LCE allocation
LCE_allocation <- read_csv(
  paste0(
    charges_path, 
    "LCE_allocation.csv"
  ),
  col_types = cols()
)

###############
### Revenue ###
###############

## Investment benefit revenue
benefit_revenue_forecast <- read_csv(
  paste0(
    revenue_path,
    "benefit_revenue_forecast.csv"
  ),
  col_types = cols()
) %>% 
  filter(year == eval_year) %>% 
  
  # Adjustment 7: adjust benefit revenues down
  purrr::when(
    adj7_flag == TRUE ~ 
      mutate(
        .,
        revenue_million = revenue_million * (transpower_revenue_forecast / transpower_revenue_forecast_old)
      ),
    
    ~ .
  )

## Total revenue forecast
revenue_forecast_raw <- read_csv(
  paste0(
    revenue_path, 
    "total_revenue_forecast.csv"
  ),
  col_types = cols()
) %>%
  
  # Adjustment 7:
  purrr::when(
    adj7_flag == TRUE ~ 
      mutate(
        .,
        
        # Adjust total benefit revenue down
        revenue_million = case_when(
          scenario_type == "proposed" & revenue_type == "Benefit charge" ~ 
          revenue_million * (transpower_revenue_forecast / transpower_revenue_forecast_old),
          TRUE ~ revenue_million
        )
      ),
    
    ~ .
  )

##############
### Demand ###
##############

## GR010 demand
## NOTE: the demand CSVs are compressed as zip files. The following function
## uncompresses the files on the fly and then reads the CSV.
demand_raw <- demand_years %>%
  map_dfr(
    ~ read_csv(
      paste0(
        demand_path, 
        "GR010_demand_MW_", 
        ., 
        ".zip"
      ),
      # n_max = n_max,
      col_types = cols()) %>% 
      mutate(
        year = .x
      )
  ) %>% 
  mutate(trade_date = ymd(trade_date)) %>% 
  # Adjustment 5: remove load volumes for WPT0111
  purrr::when(
    adj5_flag == TRUE ~ 
      
      filter(., !poc == "WPT0111"),
    
    ~ .
    
  )

##################
### Generation ###
##################

## GR010 generation
generation_raw <- generation_years %>%
  map_dfr(
    ~ read_csv(
      paste0(
        generation_path, 
        "GR010_generation_MW_",
        ., 
        ".csv"
      ),
      # n_max = n_max,
      col_types = cols()) %>% 
      mutate(
        year = .x
      )
  )%>% 
  mutate(trade_date = ymd(trade_date))

#####################
### Node benefits ###
#####################
node_benefit_files <- list.files(node_benefits_path)

node_benefits_raw <- node_benefit_files %>%
  map_dfr(
    ~ read_csv(
      paste0(node_benefits_path, .),
      # n_max = n_max,
      col_types = cols()
    ) %>% 
      mutate(
        node_benefit_investment = str_replace_all(
          .x,
          "_node_benefits.csv",
          ""
        )
      )
  )

###########
### Cap ###
###########

## Distributor charges
distributor_charges <- read_csv(
  "inputs/cap/distributor_charges.csv",
  col_types = cols()
) 

############
### ACOT ###
############

## ACOT payments
ACOT_payments <- read_csv(
  paste0(
    ACOT_path, 
    "ACOT_payments.csv"
  ),
  col_types = cols()
)

###################
### Residential ###
###################

## Residential energy use
residential_energy_use <- read_csv(
  paste0(
    residential_path, 
    "residential_energy_use.csv"
  ),
  col_types = cols()
)
