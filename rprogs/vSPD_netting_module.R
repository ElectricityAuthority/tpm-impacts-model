###############################################
### Title: vSPD netting module              ###
### Description: generic and non-generic    ###
### functions used for replicating the      ###
### netting decisions used for the 2019     ###
### proposal.                               ###
### Date: 6 January 2020                    ###
###############################################

# Create wide vSPD traditional dataset
vSPD_traditional_wide <- vSPD_traditional %>% 
  pivot_wider(names_from = type, values_from = benefit)

# Node nets off itself
net_off_itself <- function(nodes_to_net){
  
  node_net_final <- vSPD_traditional_wide %>% 
    filter(node %in% nodes_to_net) %>% 
    mutate(
      
      generationMWh_new = 0,
      loadMWh_new = loadMWh - generationMWh,
      generation_benefit_new = (generationMWh_new / generationMWh) * generation_benefit,
      demand_benefit_new = (loadMWh_new / loadMWh) * demand_benefit
      
    ) %>% 
    # Reconcile benefits where they have been modified
    mutate(
      generation_benefit = ifelse(is.na(generation_benefit_new), generation_benefit, generation_benefit_new),
      demand_benefit = ifelse(is.na(demand_benefit_new), demand_benefit, demand_benefit_new)
    ) %>% 
    # Drop '_new' columns
    select(-contains("_new")) %>% 
    arrange(node, node_benefit_investment, modelyear)
  
  return(node_net_final)
  
}

# Node nets off itself and then another
net_itself_then_another <- function(from_node, from_customer, to_node, to_customer){
  
  # First, net "from_node" off itself
  from_node_net <- vSPD_traditional_wide %>% 
    filter(node == from_node, transpower_customer == from_customer) %>% 
    mutate(
      net_load = pmax(0, loadMWh - generationMWh),
      net_gen = pmax(0, generationMWh - loadMWh)
    ) 
  
  # Then, net "to_node" off itself
  to_node_net <- vSPD_traditional_wide %>% 
    filter(node == to_node, transpower_customer == to_customer)  %>% 
    mutate(
      net_load = pmax(0, loadMWh - generationMWh),
      net_gen = pmax(0, generationMWh - loadMWh)
    ) 
  
  # Then use the excess generation at "from_node" to net off the load at "to_node"
  to_node_final <- to_node_net %>% 
    inner_join(
      from_node_net %>% 
        select(modelyear, node_benefit_investment, net_gen) %>% 
        rename(net_gen_from = net_gen),
      by = c("modelyear", "node_benefit_investment")
    ) %>% 
    mutate(
      generationMWh_new = 0,
      loadMWh_new = net_load - net_gen_from,
      generation_benefit_new = coalesce((generationMWh_new / generationMWh) * generation_benefit, 0),
      demand_benefit_new = (loadMWh_new / loadMWh) * demand_benefit
    ) %>% 
    # Reconcile changes
    mutate(
      generationMWh = generationMWh_new,
      loadMWh = loadMWh_new,
      generation_benefit = generation_benefit_new,
      demand_benefit = demand_benefit_new
    ) %>% 
    # Drop '_new' and 'net_' columns
    select(-contains("_new"), -contains("net_"))
  
  # Create final version of the "from_node"
  from_node_final <- from_node_net %>% 
    mutate(
      generationMWh_new = 0,
      loadMWh_new = net_load,
      generation_benefit_new = coalesce((generationMWh_new / generationMWh) * generation_benefit, 0),
      demand_benefit_new = (loadMWh_new / loadMWh) * demand_benefit
    ) %>% 
    # Reconcile changes
    mutate(
      generationMWh = generationMWh_new,
      loadMWh = loadMWh_new,
      generation_benefit = generation_benefit_new,
      demand_benefit = demand_benefit_new
    ) %>% 
    # Drop '_new' and 'net_' columns
    select(-contains("_new"), -contains("net_"))
  
  # Union results and output
  res <- to_node_final %>% 
    union_all(
      from_node_final
    )
  
  return(res)
  
}

# Generation nets off load
generation_nets_load <- function(gen_node, gen_customer, load_node, load_customer){
  
  # First, filter to generation node
  gen_node <- vSPD_traditional_wide %>% 
    filter(node == gen_node, transpower_customer == gen_customer) 
  
  # Then, filter to load node
  load_node <- vSPD_traditional_wide %>% 
    filter(node == load_node, transpower_customer == load_customer) 
  
  # Then use the generation node to net the load node
  net_final <- load_node %>% 
    inner_join(
      gen_node %>% 
        select(modelyear, node_benefit_investment, generationMWh) %>% 
        rename(net_gen = generationMWh),
      by = c("modelyear", "node_benefit_investment")
    ) %>% 
    mutate(
      generationMWh_new = 0,
      loadMWh_new = loadMWh - net_gen,
      generation_benefit_new = coalesce((generationMWh_new / generationMWh) * generation_benefit, 0),
      demand_benefit_new = (loadMWh_new / loadMWh) * demand_benefit
    ) %>% 
    # Reconcile changes
    mutate(
      generationMWh = generationMWh_new,
      loadMWh = loadMWh_new,
      generation_benefit = generation_benefit_new,
      demand_benefit = demand_benefit_new
    ) %>% 
    # Drop '_new' and 'net_' columns
    select(-contains("_new"), -contains("net_")) %>% 
    # Union with the generation node and zero out generation benefit
    union_all(
      gen_node %>% 
        mutate(generation_benefit = 0)
    )
  
  return(net_final)
  
}

# Net two generation nodes off one load node (added for March adjustments)
net_2gen_against_1load <- function(gen_node1, gen_customer1,
                                   gen_node2, gen_customer2,
                                   load_node, load_customer){
  
  # Filter to generation node 1
  gen_node1 <- vSPD_traditional_wide %>% 
    filter(node == gen_node1, transpower_customer == gen_customer1) 
  
  # Filter to generation node 2
  gen_node2 <- vSPD_traditional_wide %>% 
    filter(node == gen_node2, transpower_customer == gen_customer2) 
  
  # Filter to load node
  load_node <- vSPD_traditional_wide %>% 
    filter(node == load_node, transpower_customer == load_customer) 
  
  # Then use the generation nodes to net the load node
  net_final <- load_node %>% 
    left_join(
      gen_node1 %>% 
        select(modelyear, node_benefit_investment, generationMWh) %>% 
        rename(net_gen1 = generationMWh),
      by = c("modelyear", "node_benefit_investment")
    ) %>% 
    left_join(
      gen_node2 %>% 
        select(modelyear, node_benefit_investment, generationMWh) %>% 
        rename(net_gen2 = generationMWh),
      by = c("modelyear", "node_benefit_investment")
    ) %>%
    replace_na(list(net_gen1 = 0, net_gen2 = 0)) %>% 
    mutate(
      generationMWh_new = 0,
      loadMWh_new = loadMWh - net_gen1 - net_gen2,
      generation_benefit_new = coalesce((generationMWh_new / generationMWh) * generation_benefit, 0),
      demand_benefit_new = (loadMWh_new / loadMWh) * demand_benefit
    ) %>% 
    # Reconcile changes
    mutate(
      generationMWh = generationMWh_new,
      loadMWh = loadMWh_new,
      generation_benefit = generation_benefit_new,
      demand_benefit = demand_benefit_new
    ) %>% 
    # Drop '_new' and 'net_' columns
    select(-contains("_new"), -contains("net_")) %>% 
    # Union with the generation nodes and zero out generation benefit
    union_all(
      gen_node1 %>% 
        mutate(generation_benefit = 0)
    ) %>% 
    union_all(
      gen_node2 %>% 
        mutate(generation_benefit = 0)
    )
  
  return(net_final)
  
}

# Multiple netting (with 3 nodes)
multiple_netting_3node <- function(from_node, to_node1, to_node2, customer){
  
  # First, use generation at "from_node" to offset the netted load at "to_node1"
  to_node1_net <- vSPD_traditional_wide %>% 
    filter(node == to_node1, transpower_customer == customer) %>%
    inner_join(
      vSPD_traditional_wide %>% 
        filter(node == from_node, transpower_customer == customer) %>% 
        select(modelyear, node_benefit_investment, generationMWh) %>% 
        rename(net_gen = generationMWh),
      by = c("modelyear", "node_benefit_investment")
    ) %>% 
    mutate(
      net_load = pmax(0, loadMWh - generationMWh - net_gen),
      extra_gen = pmax(0, generationMWh - loadMWh + net_gen)
    ) 
  
  # Then use extra generation to offset load at to_node_2
  to_node2_net <- vSPD_traditional_wide %>% 
    filter(node == to_node2, transpower_customer == customer) %>%
    inner_join(to_node1_net %>% 
                 select(modelyear, node_benefit_investment, extra_gen),
               by = c("modelyear", "node_benefit_investment")
    ) %>% 
    mutate(
      net_load = pmax(0, loadMWh - extra_gen)
    ) 
  
  to_node1_final <- to_node1_net %>% 
    mutate(
      generationMWh_new = 0,
      loadMWh_new = net_load,
      generation_benefit_new = coalesce((generationMWh_new / generationMWh) * generation_benefit, 0),
      demand_benefit_new = (loadMWh_new / loadMWh) * demand_benefit
    ) %>% 
    # Reconcile changes
    mutate(
      generationMWh = generationMWh_new,
      loadMWh = loadMWh_new,
      generation_benefit = generation_benefit_new,
      demand_benefit = demand_benefit_new
    ) %>% 
    # Drop '_new' and 'net_' columns
    select(-contains("_new"), -contains("net_"), -contains("extra_"))
  
  to_node2_final <- to_node2_net %>% 
    mutate(
      generationMWh_new = 0,
      loadMWh_new = net_load,
      generation_benefit_new = coalesce((generationMWh_new / generationMWh) * generation_benefit, 0),
      demand_benefit_new = (loadMWh_new / loadMWh) * demand_benefit
    ) %>% 
    # Reconcile changes
    mutate(
      generationMWh = generationMWh_new,
      loadMWh = loadMWh_new,
      generation_benefit = generation_benefit_new,
      demand_benefit = demand_benefit_new
    ) %>% 
    # Drop '_new' and 'net_' columns
    select(-contains("_new"), -contains("net_"), -contains("extra_")) 
  
  # Union results and output
  res <- to_node1_final %>% 
    union_all(
      to_node2_final
    ) %>% 
    # Union with the "from" node and zero out generation benefit
    union_all(
      vSPD_traditional_wide %>% 
        filter(node == from_node, transpower_customer == customer) %>% 
        mutate(generation_benefit = 0)
    )
  
  return(res)
  
}

# Multiple netting (with 4 nodes)
multiple_netting_4node <- function(from_node1, from_node2, to_node1, to_node2, customer){
  
  # First, use generation at "from_node1" to offset the netted load at "to_node1"
  to_node1_net <- vSPD_traditional_wide %>% 
    filter(node == to_node1, transpower_customer == customer) %>%
    inner_join(
      vSPD_traditional_wide %>% 
        filter(node == from_node1, transpower_customer == customer) %>% 
        select(modelyear, node_benefit_investment, generationMWh) %>% 
        rename(net_gen = generationMWh),
      by = c("modelyear", "node_benefit_investment")
    ) %>% 
    mutate(
      net_load = pmax(0, loadMWh - generationMWh - net_gen),
      extra_gen = pmax(0, generationMWh - loadMWh + net_gen)
    ) 
  
  # Then use extra generation and the generation at "from_node2" to offset load at HAM0111
  to_node2_net <- vSPD_traditional_wide %>% 
    filter(node == to_node2, transpower_customer == customer) %>%
    inner_join(
      vSPD_traditional_wide %>% 
        filter(node == from_node2, transpower_customer == customer) %>% 
        select(modelyear, node_benefit_investment, generationMWh) %>% 
        rename(net_gen = generationMWh),
      by = c("modelyear", "node_benefit_investment")
    ) %>%
    inner_join(to_node1_net %>% 
                 select(modelyear, node_benefit_investment, extra_gen),
               by = c("modelyear", "node_benefit_investment")
    ) %>% 
    mutate(
      net_load = pmax(0, loadMWh - extra_gen - net_gen)
    ) 
  
  to_node1_final <- to_node1_net %>% 
    mutate(
      generationMWh_new = 0,
      loadMWh_new = net_load,
      generation_benefit_new = coalesce((generationMWh_new / generationMWh) * generation_benefit, 0),
      demand_benefit_new = (loadMWh_new / loadMWh) * demand_benefit
    ) %>% 
    # Reconcile changes
    mutate(
      generationMWh = generationMWh_new,
      loadMWh = loadMWh_new,
      generation_benefit = generation_benefit_new,
      demand_benefit = demand_benefit_new
    ) %>% 
    # Drop '_new' and 'net_' columns
    select(-contains("_new"), -contains("net_"), -contains("extra_"))
  
  to_node2_final <- to_node2_net %>% 
    mutate(
      generationMWh_new = 0,
      loadMWh_new = net_load,
      generation_benefit_new = coalesce((generationMWh_new / generationMWh) * generation_benefit, 0),
      demand_benefit_new = (loadMWh_new / loadMWh) * demand_benefit
    ) %>% 
    # Reconcile changes
    mutate(
      generationMWh = generationMWh_new,
      loadMWh = loadMWh_new,
      generation_benefit = generation_benefit_new,
      demand_benefit = demand_benefit_new
    ) %>% 
    # Drop '_new' and 'net_' columns
    select(-contains("_new"), -contains("net_"), -contains("extra_"))
  
  # Union results and output
  res <- to_node1_final %>% 
    union_all(
      to_node2_final
    ) %>% 
    # Union with the "from" nodes and zero out generation benefit
    union_all(
      vSPD_traditional_wide %>% 
        filter(node == from_node1, transpower_customer == customer) %>% 
        mutate(generation_benefit = 0)
    ) %>%
    union_all(
      vSPD_traditional_wide %>% 
        filter(node == from_node2, transpower_customer == customer) %>% 
        mutate(generation_benefit = 0)
    )
  
  return(res)
  
}

# Multiple generating nodes
multiple_gen_nodes <- function(gen_node_vector, load_node, customer){
  
  # First, filter to generation nodes and sum generation
  gen_nodes <- vSPD_traditional_wide %>% 
    filter(node %in% gen_node_vector, transpower_customer == customer) 
  
  gen_nodes_sum <- gen_nodes %>% 
    group_by(modelyear, node_benefit_investment, transpower_customer) %>% 
    summarise(
      generationMWh = sum(generationMWh)
    ) %>% 
    ungroup()
  
  # Then, filter to load node
  load_node <- vSPD_traditional_wide %>% 
    filter(node == load_node, transpower_customer == customer) 
  
  # Then use the generation nodes to net the load node
  net_final <- load_node %>% 
    inner_join(
      gen_nodes_sum %>% 
        select(modelyear, node_benefit_investment, generationMWh) %>% 
        rename(net_gen = generationMWh),
      by = c("modelyear", "node_benefit_investment")
    ) %>% 
    mutate(
      generationMWh_new = 0,
      loadMWh_new = loadMWh - net_gen - generationMWh,
      generation_benefit_new = coalesce((generationMWh_new / generationMWh) * generation_benefit, 0),
      demand_benefit_new = (loadMWh_new / loadMWh) * demand_benefit
    ) %>% 
    # Reconcile changes
    mutate(
      generationMWh = generationMWh_new,
      loadMWh = loadMWh_new,
      generation_benefit = generation_benefit_new,
      demand_benefit = demand_benefit_new
    ) %>% 
    # Drop '_new' and 'net_' columns
    select(-contains("_new"), -contains("net_")) %>% 
    # Union with the generation node and zero out generation benefit
    union_all(
      gen_nodes %>% 
        mutate(generation_benefit = 0)
    )
  
  return(net_final)
  
}

# Special cases (that don't lend easily to generic functions)

## Netting Hawera and Stratford

hawea_stratford_net <- function(){
  
  # First, sum generation at HWA1101 PTA1/2/3
  HWA1101_PTA_gen_sum <- vSPD_traditional_wide %>% 
    filter(node %in% c("HWA1101 PTA1", "HWA1101 PTA2", "HWA1101 PTA3"), 
           transpower_customer == "TrustPower") %>% 
    group_by(modelyear, node_benefit_investment) %>% 
    summarise(generationMWh = sum(generationMWh)) %>% 
    ungroup()
  
  # Then use the generation at PTA and at HWA1102 WAA1 to net load at HWA0331
  HWA0331_net_load <- vSPD_traditional_wide %>% 
    filter(node == "HWA0331", transpower_customer == "Powerco") %>% 
    inner_join(
      HWA1101_PTA_gen_sum %>% 
                 rename(net_gen_PTA = generationMWh),
               by = c("modelyear", "node_benefit_investment")
    ) %>% 
    inner_join(
      vSPD_traditional_wide %>% 
        filter(
          node == "HWA1102 WAA1", transpower_customer == "Whareroa Cogen. Ltd") %>% 
                 select(modelyear, node_benefit_investment, generationMWh) %>% 
                 rename(net_gen_WAA1 = generationMWh),
               by = c("modelyear", "node_benefit_investment")
    ) %>% 
    mutate(
      net_load = pmax(0, loadMWh - net_gen_PTA - net_gen_WAA1),
      extra_gen = pmax(0, net_gen_PTA + net_gen_WAA1 - loadMWh)
    ) 
  
  # Then use the extra generation at HWA0331 to offset SFD0331
  SFD0331_net_load <- vSPD_traditional_wide %>% 
    filter(node == "SFD0331", transpower_customer == "Powerco") %>% 
    inner_join(HWA0331_net_load %>% 
                 select(modelyear, node_benefit_investment, extra_gen),
               by = c("modelyear", "node_benefit_investment")
    ) %>% 
    mutate(
      net_load = pmax(0, loadMWh - extra_gen)
    )
  
  # Create final datasets
  HWA0331_load_final <- HWA0331_net_load %>% 
    mutate(
      generationMWh_new = 0,
      loadMWh_new = net_load,
      generation_benefit_new = coalesce((generationMWh_new / generationMWh) * generation_benefit, 0),
      demand_benefit_new = (loadMWh_new / loadMWh) * demand_benefit
    ) %>% 
    # Reconcile changes
    mutate(
      generationMWh = generationMWh_new,
      loadMWh = loadMWh_new,
      generation_benefit = generation_benefit_new,
      demand_benefit = demand_benefit_new
    ) %>% 
    # Drop '_new' and 'net_' columns
    select(-contains("_new"), -contains("net_"), -contains("extra_"))
  
  SFD0331_load_final <- SFD0331_net_load %>% 
    mutate(
      generationMWh_new = 0,
      loadMWh_new = net_load,
      generation_benefit_new = coalesce((generationMWh_new / generationMWh) * generation_benefit, 0),
      demand_benefit_new = (loadMWh_new / loadMWh) * demand_benefit
    ) %>% 
    # Reconcile changes
    mutate(
      generationMWh = generationMWh_new,
      loadMWh = loadMWh_new,
      generation_benefit = generation_benefit_new,
      demand_benefit = demand_benefit_new
    ) %>% 
    # Drop '_new' and 'net_' columns
    select(-contains("_new"), -contains("net_"), -contains("extra_"))
  
  # Union results and output
  res <- HWA0331_load_final %>% 
    union_all(
      SFD0331_load_final
    ) %>% 
    # Union with the generation nodes and zero out generation benefit
    union_all(
      vSPD_traditional_wide %>% 
        filter(node %in% c("HWA1101 PTA1", "HWA1101 PTA2", "HWA1101 PTA3"), 
               transpower_customer == "TrustPower") %>% 
        mutate(generation_benefit = 0)
    ) %>%
    union_all(
      vSPD_traditional_wide %>% 
        filter(node == "HWA1102 WAA1", transpower_customer == "Whareroa Cogen. Ltd") %>% 
        mutate(generation_benefit = 0)
    )
  
  return(res)
  
}

## Netting Kawerau
kawerau_net <- function(){
  
  # First, work out the total load at KAW0112, KAW0112 DLS0, KAW0113,
  # KAW0113 DLS1 and KAW1101 (and also set net load to 0)
  KAW_load <- vSPD_traditional_wide %>% 
    filter(node %in% c("KAW0112", "KAW0112 DLS0", "KAW0113", "KAW0113 DLS1", 'KAW1101'), 
           transpower_customer == "Norske Skog") %>% 
    mutate(net_load = 0)
  
  KAW_load_sum <- KAW_load %>% 
    group_by(modelyear, node_benefit_investment) %>% 
    summarise(loadMWh = sum(loadMWh)) %>% 
    ungroup()
  
  # Subtract the load sum from generation at KAW1101 KAG0 (and add on the generation at KAW0112 ONU0)
  KAG0_net_gen <- vSPD_traditional_wide %>% 
    filter(node == "KAW1101 KAG0", transpower_customer == "Norske Skog") %>% 
    inner_join(KAW_load_sum %>% 
                 select(modelyear, node_benefit_investment, loadMWh) %>% 
                 rename(extra_load = loadMWh),
               by = c("modelyear", "node_benefit_investment")
    ) %>% 
    inner_join(
      vSPD_traditional_wide %>% 
        filter(node == "KAW0112 ONU0", transpower_customer == "Norske Skog") %>% 
        select(modelyear, node_benefit_investment, generationMWh) %>% 
        rename(extra_gen = generationMWh),
      by = c("modelyear", "node_benefit_investment")
    ) %>% 
    mutate(
      net_gen = pmax(0, generationMWh + extra_gen - extra_load)
    )
  
  # Create final datasets
  KAW_load_final <- KAW_load %>% 
    mutate(
      generationMWh_new = 0,
      loadMWh_new = net_load,
      generation_benefit_new = coalesce((generationMWh_new / generationMWh) * generation_benefit, 0),
      demand_benefit_new = coalesce((loadMWh_new / loadMWh) * demand_benefit, 0)
    ) %>% 
    # Reconcile changes
    mutate(
      generationMWh = generationMWh_new,
      loadMWh = loadMWh_new,
      generation_benefit = generation_benefit_new,
      demand_benefit = demand_benefit_new
    ) %>% 
    # Drop '_new' and 'net_' columns
    select(-contains("_new"), -contains("net_"), -contains("extra_"))
  
  KAG0_gen_final <- KAG0_net_gen %>% 
    mutate(
      generationMWh_new = net_gen,
      loadMWh_new = loadMWh,
      generation_benefit_new = coalesce((generationMWh_new / generationMWh) * generation_benefit, 0),
      demand_benefit_new = coalesce((loadMWh_new / loadMWh) * demand_benefit, 0)
    ) %>% 
    # Reconcile changes
    mutate(
      generationMWh = generationMWh_new,
      loadMWh = loadMWh_new,
      generation_benefit = generation_benefit_new,
      demand_benefit = demand_benefit_new
    ) %>% 
    # Drop '_new' and 'net_' columns
    select(-contains("_new"), -contains("net_"), -contains("extra_"))
  
  # Union results and output
  res <- KAW_load_final %>% 
    union_all(
      KAG0_gen_final
    ) %>%
    # Union with KAW0112 ONU0 zero out generation benefit
    union_all(
      vSPD_traditional_wide %>% 
        filter(node == "KAW0112 ONU0", transpower_customer == "Norske Skog") %>% 
        mutate(generation_benefit = 0)
    )
  
  return(res)
  
}


# Bind all modified rows in to single dataframe
final_net_vSPD_adjust <- bind_rows(
  
  net_off_itself(nodes_to_net = c("DOB0331", "LTN0331", "NMA0331", "ONG0331")),
  
  net_itself_then_another(from_node = "ABY0111", from_customer = "Alpine Energy", 
                          to_node = "TIM0111", to_customer = "Alpine Energy"),
  net_itself_then_another(from_node = "WIL0331", from_customer = "Wellington Electricity",
                          to_node = "TKR0331", to_customer = "Wellington Electricity"),
  
  generation_nets_load(gen_node = "ASB0661 HBK0", gen_customer = "Electricity Ashburton",
                       load_node = "ASB0661", load_customer = "Electricity Ashburton"),
  generation_nets_load(gen_node = "GLN0332 GLN0", gen_customer = "NZ Steel",
                       load_node = "GLN0331", load_customer = "NZ Steel"),
  generation_nets_load(gen_node = "TGA0331 KMI0", gen_customer = "Powerco",
                       load_node = "TGA0331", load_customer = "Powerco"),
  # generation_nets_load(gen_node = "STK0661 COB0", gen_customer = "Network Tasman",
  #                      load_node = "STK0331", load_customer = "Network Tasman"),
  generation_nets_load(gen_node = "NSY0331 PAE0", gen_customer = "OtagoNet JV",
                       load_node = "NSY0331", load_customer = "OtagoNet JV"),
  
  # Adjustment 3: net MAT1101 ANI0 against EDG0331
  if(adj3_flag == TRUE){
    
    net_2gen_against_1load(
      gen_node1 = "KAW0111",
      gen_customer1 = "Horizon Energy",
      gen_node2 = "MAT1101 ANI0",
      gen_customer2 = "Southern Generation",
      load_node = "EDG0331",
      load_customer = "Horizon Energy"
    )
    
  } else {
    
    generation_nets_load(
      gen_node = "KAW0111",
      gen_customer = "Horizon Energy",
      load_node = "EDG0331",
      load_customer = "Horizon Energy"
    )
    
  },
  
  # Adjustment 17: Net Cobb generation against Network Tasman POC: STK0331
  if(adj17_flag == TRUE){
    
    net_2gen_against_1load(
      gen_node1 = "STK0661 COB0",
      gen_customer1 = "Network Tasman",
      gen_node2 = "COB0661 COB0",
      gen_customer2 = "TrustPower",
      load_node = "STK0331",
      load_customer = "Network Tasman"
    )
    
  } else {
    
    generation_nets_load(
      gen_node = "STK0661 COB0",
      gen_customer = "Network Tasman",
      load_node = "STK0331",
      load_customer = "Network Tasman"
    )
    
  },
  
  
  
  # generation_nets_load(gen_node = "KAW0111", gen_customer = "Horizon Energy",
  #                      load_node = "EDG0331", load_customer = "Horizon Energy"),
  
  generation_nets_load(gen_node = "CYD0331", gen_customer = "Aurora Energy",
                       load_node = "CML0331", load_customer = "Aurora Energy"),
  
  multiple_netting_3node(from_node = "TWH0331 TRC1", to_node1 = "TWH0331", 
                         to_node2 = "HAM0111", customer = "WEL Networks"),
  multiple_netting_3node(from_node = "KUM0661 KUM0", to_node1 = "KUM0661", 
                         to_node2 = "GYM0661", customer = "Westpower"),
  multiple_netting_3node(from_node = "ROT1101 WHE0", to_node1 = "OWH0111", 
                         to_node2 = "ROT0111", customer = "Unison Networks"),
  
  
  multiple_netting_4node(from_node1 = "WRK0331 RKA0", from_node2 = "WRK0331 TAA0", 
                         to_node1 = "ROT0331", to_node2 = "WRK0331", customer = "Unison Networks"),
  
  multiple_gen_nodes(gen_node_vector = c("BWK1101 WPI0", "HWB0331 WPI0"), load_node = "HWB0331", 
                     customer = "Aurora Energy"),
  multiple_gen_nodes(gen_node_vector = c("KIN0112", "KIN0112 KIN0"), load_node = "KIN0111", 
                     customer = "Powerco"),
  
  hawea_stratford_net(),
  
  kawerau_net()
) 
