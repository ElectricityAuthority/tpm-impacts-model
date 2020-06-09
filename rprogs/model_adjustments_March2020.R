###############################################
### Title: Model adjustments March 2020     ###
### Description: Flags for adjustments      ###
### to the main model based on submissions  ###
### etc.                                    ###
### Date: 26 March 2020                     ###
###############################################

# Adjustment flags

## Adjustment 1: change customer name from "Tuaropaki Power" to "Tuaropaki (Mercury)"
adj1_flag <- TRUE

## Adjustment 2: change customer name from Nova to KCE (Mangahao) for
## generation at POC: MHO0331 MH00
adj2_flag <- TRUE

## Adjustment 3: recognise Aniwhenua as distributed generation (POC: MAT1101 ANI0) 
## and net against EDG0331
adj3_flag <- TRUE

## Adjustment 4: re-allocate load to Horizon Energy from Southern Generation (at MAT1101)
adj4_flag <- TRUE

# Adjustment 5: adjust load for Buller to reflect permanent changes in demand and double 
# counting (adjust vSPD and residual load)
adj5_flag <- TRUE

# Adjustment 6: adjust load for Westpower to reflect permanent changes in demand and double 
# counting (adjust vSPD and Residual load)
adj6_flag <- TRUE

# Adjustment 7: revised Revenue for RCP3 based on final Commerce Commission decision - 
# total proposed revenue to reduce from $848m to $798.8m
adj7_flag <- TRUE

# Adjustment 8: change to ensure that status quo is net of LCE, for like-for-like comparison 
# with proposal. Adjust status quo for cap
adj8_flag <- TRUE

# Adjustment 9: adjust Eastland Network AMDs (WRA0111, GIS0501 and TUI0111) - set to zero
adj9_flag <- TRUE

# Adjustment 10: adjust Northpower AMD (KEN0331) - set to zero
adj10_flag <- TRUE

# Adjustment 11: adjustment for Cobb generation - set generation benefits for COB0661 COB0 to zero
adj11_flag <- TRUE

# Adjustment 12: adjust Orion AMDs for 2014-15 (ADD0111, ADD0661, 
# MLN0661 and MLN0664)
adj12_flag <- TRUE

# Adjustment 13: no change

# Adjustment 14: Southdown generator closure - SWN2201 (remove generation benefit)
adj14_flag <- TRUE

# Adjustment 15: Contact generator closure - OTA2202 (remove generation benefit)
adj15_flag <- TRUE

# Adjustment 16: Replace blank load cells in Ngawha adjustment
adj16_flag <- TRUE

# Adjustment 17: Net Cobb generation against Network Tasman POC: STK0331
adj17_flag <- TRUE
