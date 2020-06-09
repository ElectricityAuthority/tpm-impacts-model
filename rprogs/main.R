###############################################
### Title: Main script                      ###
### Description: Run the model from end     ###
### to end                                  ###
### Date: 29 November 2019                  ###
###############################################

################
### 0. Setup ###
################

# Load libraries
renv::activate()
library(tidyverse)
library(lubridate)

# Set relative paths
demand_path <- "inputs/demand/"
generation_path <- "inputs/generation/"
node_benefits_path <- "inputs/node_benefits/"
ACOT_path <- "inputs/ACOT/"
mapping_path <- "inputs/mapping/"
revenue_path <- "inputs/revenue/"
charges_path <- "inputs/charges/"
residential_path <- "inputs/residential/"

# Set flags (to "all TRUE" or "all FALSE" using the all_flags parameter)
# When NULL, the flags from rprogs/model_adjustments_March2020.R are used

all_flags <- NULL

if(!is.null(all_flags)){
  
  list2env(as.list(setNames(rep(all_flags, 16), paste0(
    "adj", c(1:12, 14:17), "_flag"
  ))), envir = globalenv())
  
} else {
  
  # Source adjustment flags and functions from file
  source("rprogs/model_adjustments_March2020.R")
  
}

# Set parameters

## Evaluation year
eval_year <- "2021/22"

## Total Transpower revenue forecast (including adjustment 7 condition)
transpower_revenue_forecast <- if_else(adj7_flag == TRUE, 798.8, 848) # $million
if(adj7_flag){
  
  transpower_revenue_forecast_old <- 848
  
}

## Assumed cost of energy
energy_cost_estimated <- 75 # $/MWh

## Nga Wha adjustment flag 
nga_wha_adjust_flag <- TRUE

## Demand year vector
demand_years <- c(
  "20140701_20150630",
  "20150701_20160630",
  "20160701_20170630",
  "20170701_20180630"
)

## Generation year vector
generation_years <- c(
  "20140701_20150630",
  "20150701_20160630",
  "20160701_20170630",
  "20170701_20180630"
)

## Set growth and growth years (for demand and generation forecast)
estimated_growth <- 0.01 # (i.e. 1% pa)
growth_years <- as.numeric(difftime(ymd("20220630"), ymd("20160630")) / 365)

## Generate HTML report
generate_report <- TRUE

## Set options
options(
  # Turn off scientific notation
  scipen = 9999
)

#############################
### 1. Read in input data ###
#############################

source("rprogs/read_inputs.R")

############################
### 2. Calculate revenue ###
############################

source("rprogs/calculate_revenue.R")

####################################
### 3. Calculate current charges ###
####################################

source("rprogs/current_charges.R")

################################
### 4. Calculate mean demand ###
################################

source("rprogs/calculate_mean_demand.R")

####################################
### 5. Calculate mean generation ###
####################################

source("rprogs/calculate_mean_generation.R")

#######################################
### 6. Calculate residual options #####
#######################################

source("rprogs/calculate_residual_options.R")

##################################
### 7. Calculate vSPD benefits ###
##################################

source("rprogs/calculate_vSPD_benefits.R")

#######################################
### 8. Calculate indicative charges ###
#######################################

source("rprogs/indicative_charges.R")

################################
### 9. Calculate ACOT impact ###
################################

source("rprogs/calculate_ACOT_payments.R")

#####################################
### 10. Calculate capped proposal ###
#####################################

source("rprogs/calculate_cap.R")

#########################################
### 11. Calculate residential impacts ###
#########################################

source("rprogs/residential_impacts.R")

#################################
### 12. Create results report ###
#################################

if(generate_report){
  
  write_csv(indicative_charges, "results/indicative_charges.csv")
  write_csv(capping_final, "results/capping_final.csv")
  write_csv(residential_impacts, "results/residential_impacts.csv")
  
  rmarkdown::render("results/impact_model_results.Rmd")  
  
}




