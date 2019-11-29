###############################################
### Title: Main script                      ###
### Description: Run the model from end     ###
### to end                                  ###
### Date: 29 November 2019                  ###
###############################################

#############
### Setup ###
#############

# Load libraries
library(tidyverse)
library(lubridate)
library(vroom)

# Set relative paths 
demand_path <- "inputs/demand/"
generation_path <- "inputs/generation/"
node_benefits_path <- "inputs/node_benefits/"
ACOT_path <- "inputs/ACOT/"
mapping_path <- "inputs/mapping/"
revenue_path <- "inputs/revenue/"
charges_path <- "inputs/charges/"

# Set max rows for reading in while in development
max_rows <- function(){
  
  env <- rstudioapi::showQuestion(
    "Prod/Dev?", 
    "Do you want to run in dev?", 
    ok = "Yes", 
    cancel = "No"
  )
  
  # Max rows to read in
  if(env){
    
    n_max <- 10000
    
  } else {
    
    n_max <- Inf
    
  }
  
  return(n_max)
  
}

n_max <- max_rows()

# Set parameters

## Total Transpower revenue forecast
forecast_revenue_total <- 848 # $million

## Assumed cost of energy
energy_cost_estimated <- 75 # $/MWh 

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

# Read in input data
source("read_inputs.R")

#######################
### Current charges ###
#######################

source("current_charges.R")

###############
### Revenue ###
###############

source("calculate_revenue.R")

##############
### Demand ###
##############

source("calculate_mean_demand.R")

##################
### Generation ###
##################

source("calculate_mean_generation.R")

##################
### Residual #####
##################

source("calculate_residual_options.R")

############
### vSPD ###
############

source("calculate_vSPD_benefits.R")

##########################
### Indicative charges ###
##########################

source("indicative_charges.R")

############
### ACOT ###
############

source("ACOT_impact.R")

##########################
### Resdential impacts ###
##########################

source("residential_impacts.R")

###############
### Capping ###
###############

source("calculate_cap.R")

#############
### Notes ###
#############

# TP49 and TP50 > 0: first Sunday in April (20150405, 20160403, 20170402, 20180401)
# sum(TP47) or sum(TP48) == 0: last Sunday in September (20140928, 20150927, 20160925, 20170924)
# Create results sheets to compare results (and intermediate tables) against.

