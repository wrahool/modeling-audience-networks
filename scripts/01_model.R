library(tidyverse)
library(igraph)

setwd("C:/Users/Subhayan/Documents/Work/modeling-audience-networks/")

##################################################
# model paramters

# n1: number of websites in the universe
# n2: number of members of the audiences
# n3: number of websites each person visits
# n4: number ot types of websites / people

# alpha: measure of randomness
# each person visits a total of n3 websites
# which websites they visit depends on a tuning parameter alpha (which controls their randomness)
# when alpha is 0 they can only visit websites whose outlet_id == their own p_id
# when alpha is 1 they can visit any website
# when alpha is 0.5, half of the websites they visit can be any website, the other half have to be restricted to those whose outlet_id == p_id


##################################################

get_simulated_network <- function(n1, n2, n3, n4, alpha, use_unique = TRUE) {
  outlet_ids <- 1:n1
  p_ids <- 1:n2
  types <- LETTERS[1:n4]
  
  outlets_tbl <- tibble(
    outlet_id = outlet_ids,
    outlet_name = paste("O", outlet_ids, sep = "_"),
    outlet_type = rep(types, each = n1/n4)
  )
  
  audience_tbl <- tibble(
    p_id = p_ids,
    p_name = paste("P", p_ids, sep = ""),
    p_type = rep(types, each = n2/n4)
  )
  
  
  audience_el <- NULL
  
  # loop over each person
  for(p in 1:1000) {
    
    # print(p)
    
    # when alpha is 0 all of their choices are selective
    # when alpha is 1 all of their choices are random
    random_choices_allowed <- round(alpha * n3)
    selective_choices_allowed <- round(n3 - random_choices_allowed)
    
  
    selective_chosen_outlets <- outlets_tbl %>%               # only from
      filter(outlet_type == audience_tbl$p_type[p]) %>%       # those outlets where outlet_type == p_type
      pull(outlet_id) %>%                                     #
      sample(selective_choices_allowed, replace = TRUE)       # sample the number of selective outlets allowed
    
    random_chosen_outlets <- outlets_tbl %>%                  # from
      pull(outlet_id) %>%                                     # all outlets in the universe
      sample(random_choices_allowed, replace = TRUE)          # sample the number of random outlets allowed
    
    all_chosen_outlets <- c(selective_chosen_outlets,
                            random_chosen_outlets)
    
    if (use_unique == TRUE)
      all_chosen_outlets <- unique(all_chosen_outlets)
    
    # build the edge-list for the audience network
    audience_el <- audience_el %>%
      rbind(
        tibble(
          p_name = paste("P", rep(p, length(all_chosen_outlets)), sep = "_"),             # one column is the p_id
          outlet_name = paste("O", all_chosen_outlets, sep = "_") # second column is the outlet_id
        )
      )
  }
  
  outlet_reach <- audience_el %>%
    pull(outlet_name) %>%
    table() %>%
    as_tibble() %>%
    rename(uv = n) %>%
    select(outlet_name = 1, everything())
    
  audience_g <- graph_from_data_frame(audience_el, directed = F)
  V(audience_g)$type <- substr(V(audience_g)$name, 1, 1) == "O"
  
  projection_graphs <- bipartite_projection(audience_g, multiplicity = TRUE)
  
  outlet_projection <- projection_graphs$proj2
  
  V(outlet_projection)$type <- V(outlet_projection)$name %>%
                                  lapply(FUN = function(x) { 
                                          outlets_tbl %>%
                                          filter(outlet_name == x) %>% 
                                          pull(outlet_type)
                                        }
                                    ) %>%
                                  unlist()
  
  # colrs <- c("gray50", "tomato", "gold", "purple", "cyan")
  V(outlet_projection)$color <- ifelse(V(outlet_projection)$type == "A", "gray50",
                                       ifelse(V(outlet_projection)$type == "B", "tomato",
                                              ifelse(V(outlet_projection)$type == "C", "gold",
                                                     ifelse(V(outlet_projection)$type == "D", "olivedrab4",
                                                            "cyan"))))
  
  
  outlet_projection_sl <- outlet_projection
  outlet_projection_sl[from=V(outlet_projection_sl), to=V(outlet_projection_sl)] = 1
  for(v in V(outlet_projection_sl)$name) {
    E(outlet_projection_sl)[v %--% v]$weight <- outlet_reach %>% 
      filter(outlet_name == v) %>%
      pull(uv)
  }
  
  return(outlet_projection_sl)
  
}

n1 <- 500 # number of websites in the universe
n2 <- 1000 # number of members of the audiences
n3 <- 100 # number of websites each person visits
n4 <- 5 # number ot types of websites / people

# each person visits a total of n3 websites
# which websites they visit depends on a tuning parameter alpha (which controls their randomness)
# when alpha is 0 they can only visit websites whose outlet_id == their own p_id
# when alpha is 1 they can visit any website
# when alpha is 0.5, half of the websites they visit can be any website, the other half have to be restricted to those whose outlet_id == p_id

set.seed(42)
simulation_tbl <- NULL

for(i in 1:50) {
  count <- 1
  for(alpha in seq(0.5, 1, by = 0.01)) {
    message(alpha)
    g_sl <- get_simulated_network(n1, n2, n3, n4, alpha, use_unique = TRUE)
    g <- simplify(g_sl, remove.loops = TRUE)
    
    simulation_tbl <- tibble(alpha = alpha,
           n_comm_sl = length(walktrap.community(g_sl)),
           n_comm = length(walktrap.community(g))
     ) %>% rbind(simulation_tbl)
    
    if(count %% 10 == 0) {
      write_csv(simulation_tbl, paste0("data/", i, "_master_simulation_results", count, ".csv"))
      simulation_tbl <- NULL
    }
    count <- count + 1
  }
}

# write_csv(simulation_tbl, "data/simulation_results5to8.csv")

# temp <- simulation_tbl %>%
#   filter(!n_comm  %in% c(500),
#          !n_comm_sl %in% c(500))
# 
# ggplot(simulation_tbl) +
#   geom_line(aes(x=alpha, y=n_comm_sl), color = "red") +
#   geom_line(aes(x=alpha, y=n_comm), color = "blue")
