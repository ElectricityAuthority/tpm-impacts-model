###############################################
### Title: Nga Wha adjustment module        ###
### Description: a function, for overriding ###
### December 2018 vSPD run volumes with     ###
### those from October 2018 and then        ###
### recalculating benefits.                 ###
### Date: 6 January 2020                    ###
###############################################

nga_wha_adjustment <- function(investment){
  
  # Adjustment 16: add previously missing load and gen to KOE1101
  if(adj16_flag == TRUE){
    
    Oct2018_run <- read_csv(paste0("inputs/nga_wha/Oct2018_load_and_gen_complete.csv"), col_types = cols())  
    
  } else {
    
    Oct2018_run <- read_csv(paste0("inputs/nga_wha/", investment, "_KOE1101_Oct2018.csv"), col_types = cols())
    
  }
  
  Dec2018_run <- read_csv(paste0("inputs/nga_wha/", investment, "_KOE1101_Dec2018.csv"), col_types = cols())
  
  Dec2018_revised <- Dec2018_run %>% 
    
    # Rename load and generation
    rename(
      generation_Dec2018 = generation,
      load_Dec2018 = load
    ) %>% 
    
    # Join on Oct volumes
    inner_join(
      Oct2018_run,
      by = "datetime"
    ) %>% 
    mutate(
      
      # Overwrite counterfactual load and gen too
      c_load = load,
      c_generation = generation,
      
      # Recalculate benefits
      F_load_ben = (1000 - price) * load / 2,
      F_gen_ben = generationrevenue - generationcost,
      CF_load_ben = (1000 - c_price) * c_load / 2,
      CF_gen_ben = c_generationrevenue - c_generationcost,
      
      load_ben = F_load_ben - CF_load_ben,
      gen_ben = F_gen_ben - CF_gen_ben
    )
  
  Dec2018_revised %>% 
    group_by(modelyear, node) %>% 
    summarise(
      demand_benefit = sum(load_ben),
      generation_benefit = 0
    ) %>% 
    ungroup() %>% 
    mutate(
      node_benefit_investment = investment,
      transpower_customer = "Top Energy"
    ) %>%
    
    # Reorder columns
    select(node_benefit_investment, modelyear, transpower_customer, 
           node, demand_benefit, generation_benefit)
  
}


