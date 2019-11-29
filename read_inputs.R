###############################################
### Title: Read inputs                      ###
### Description: Read in all input files.   ###
### Date: 29 November 2019                  ###
###############################################

########################
### Customer mapping ###
########################

## POC/Network/Customer concordance
customer_poc_network <- vroom(
  paste0(
    mapping_path, 
    "customer_poc_network_map.csv"
  )
)

## Customer/type concordance
customer_type <- vroom(
  paste0(
    mapping_path, 
    "customer_type_map.csv"
  )
)

###############
### Charges ###
###############

## Current charges (2021/22)
current_charges <- vroom(
  paste0(
    charges_path, 
    "current_charges.csv"
  )
)

###############
### Revenue ###
###############

## Investment benefit revenue
benefit_revenue_forecast <- vroom(
  paste0(
    revenue_path,
    "benefit_revenue_forecast.csv"
  )
)

## Total revenue forecast
total_revenue_forecast_raw <- vroom(
  paste0(
    revenue_path, 
    "total_revenue_forecast.csv"
  )
) 

##############
### Demand ###
##############

## GR010 demand
demand_raw <- demand_years %>%
  map_dfr(
    ~ vroom(
      paste0(
        demand_path, 
        "GR010_demand_MW_", 
        ., 
        ".csv"
      ),
      n_max = n_max) %>% 
      mutate(
        year = .x
      )
  ) %>% 
  mutate(trade_date = ymd(trade_date))

##################
### Generation ###
##################

## GR010 generation
generation_raw <- generation_years %>%
  map_dfr(
    ~ vroom(
      paste0(
        generation_path, 
        "GR010_generation_MW_",
        ., 
        ".csv"
      ),
      n_max = n_max) %>% 
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
    ~ vroom(
      paste0(node_benefits_path, .),
      n_max = n_max) %>% 
      mutate(
        investment = str_replace_all(
          .x,
          "_node_benefits.csv",
          ""
        )
      )
  )

############
### ACOT ###
############
ACOT_payments <- vroom(
  paste0(
    ACOT_path, 
    "ACOT_payments.csv"
  )
)
