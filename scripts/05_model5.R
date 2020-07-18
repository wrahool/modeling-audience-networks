library(tidyverse)
library(igraph)
library(aricode)
library(poweRlaw)

# model 5

# the only difference between model 3 and model 4 is the calculation of prediction accuracy
# instead of doing a naive accuracy calculation, in this I'm using
# NMI or the Normalized Mutual Information

# in model 5, there's a new parameter: the exponent of the power law
# outlets' repute follow a power law and audience's choices depends on the outlet's reputation

################################################################
# model parameters

# n1: number of websites in the universe
# n2: number of members of the audiences
# n4: number of types of websites / people

# rho:
# each person visits a total of n3 websites
# which websites they visit depends on a tuning parameter rho (which controls their randomness)
# when rho is 0 they can only visit websites whose outlet_id == their own p_id
# when rho is 1 they can visit any website
# when rho is 0.5, half of the websites they visit can be any website, the other half have to be restricted to those whose outlet_id == p_id

################################################################

sample_atleast_once <- function(x, n){
  
  # Only consider unique items
  if(length(unique(x)) > n){
    stop("Not enough unique items in input to give at least one of each")
  }
  
  # Get values for vector - force each item in at least once
  # then randomly select values to get the remaining.
  vals <- c(unique(x),
            sample(unique(x), n - length(unique(x)), 
                   replace = TRUE))
  
  # Now shuffle them
  sample(vals)
}


get_simulated_network <- function(n1, n2, n3, n4, a, rho, stop_debug = FALSE) {
  
  outlet_ids <- 1:n1
  p_ids <- 1:n2
  types <- LETTERS[1:n4]
  
  outlet_rep <-  rpldis(n1, 1, alpha = a) # power law distribution
  outlet_rep_normalized <- outlet_rep / sum(outlet_rep)
  
  outlets_tbl <- tibble(
    outlet_id = outlet_ids,
    outlet_name = paste("O", outlet_ids, sep = "_"),
    outlet_type = sample_atleast_once(types, n1), # at least one website of each type
    outlet_repute = outlet_rep_normalized
  )
  
  audience_tbl <- tibble(
    p_id = p_ids,
    p_name = paste("P", p_ids, sep = ""),
    p_type = sample_atleast_once(types, n2)       # at least one audience member of each type
  )
  
  audience_el <- NULL
  
  # loop over each person
  for(p in 1:n2) {
    
    n3 <- sample(1:n1, 1)
    
    # when rho is 0 all of their choices are selective
    # when rho is 1 all of their choices are random
    random_choices_allowed <- round(rho * n3)
    selective_choices_allowed <- n3 - random_choices_allowed
    
    # print(n3)
    # print(random_choices_allowed)
    # print(selective_choices_allowed)
    
    selective_outlets_pool <- outlets_tbl %>%
      filter(outlet_type == audience_tbl$p_type[p]) %>%
      select(outlet_id, outlet_repute)
    
    if(nrow(selective_outlets_pool) == 1) {                  # this is to prevent the bug where 1 type gets 1 outlet
      selective_outlets_pool <- selective_outlets_pool %>%
        rbind(selective_outlets_pool)
    }
    
    selective_chosen_outlets <- selective_outlets_pool %>%          # from
      pull(outlet_id) %>%                                           # all outlets in the selective outlets pool
      sample(selective_choices_allowed, replace = TRUE,             # randomly sample with prob = outlet repute (R auto-normalizes the probabilities of the subset)
             prob = selective_outlets_pool$outlet_repute)
    
    random_chosen_outlets <- outlets_tbl %>%                        # from
      pull(outlet_id) %>%                                           # all outlets in the universe
      sample(random_choices_allowed, replace = TRUE,                # randomly sample with prob = outlet_repute
             prob = outlets_tbl$outlet_repute)
    
    all_chosen_outlets <- c(selective_chosen_outlets,
                            random_chosen_outlets)
    
    # build the edge-list for the audience network
    audience_el <- audience_el %>%
      rbind(
        tibble(
          p_name = paste("P", rep(p, n3), sep = "_"),             # one column is the p_id
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
  
  return(list(outlet_projection, outlet_projection_sl, outlets_tbl))
}

################################################################
# function to calculate mode
mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

################################################################
# 
# assign, to outlets in each cluster, the modal type as its predicted type
get_prediction_accuracy <- function(c, outlet_types) {
  
  pred_tbl <- NULL
  for(i in 1:length(c)) {
    
    predicted_type_i <- outlet_types %>%
      filter(outlet_name %in% c[[i]]) %>%
      pull(outlet_type) %>%
      mode() %>%
      rep(length(c[[i]]))
    
    pred_tbl <- tibble(
      outlet_name = c[[i]],
      pred_type = predicted_type_i
    ) %>%
      rbind(pred_tbl)
  }
  
  pred_tbl <- outlet_types %>%
    merge(pred_tbl)
  
  pred_tbl %>%
    filter(outlet_type == pred_type) %>%
    nrow %>%
    `/` (nrow(pred_tbl))
  
}

# function to calculate the NMI for community structure c
get_NMI <- function(c, outlet_types) {
  
  if(!is.na(c)) {
    confusion_tbl <- outlet_types %>%
      merge(tibble(
        outlet_name = c$names,
        pred_type = c$membership
      ))
    
    NMI_score <- NMI(confusion_tbl$outlet_type,
                     confusion_tbl$pred_type)
  } else
    NMI_score <- NA
  
  return(NMI_score)
  
}

run_simulation <- function(n1, n2, n3, n4, pl_exp, rho, N) {
  res_tbl <- NULL
  all_o_ds <- NULL
  for(i in 1:N) {
    message(paste0("rho : ", rho, " Run : ", i))
    
    
    test <- get_simulated_network(n1, n2, n3, n4, a, rho, stop_debug = FALSE)
    
    # the next line is if you need to debug at a particular iteration (value of i)
    # test <- get_simulated_network(n1, n2, n3, n4, a, rho, stop_debug = ifelse(i == 24, TRUE, FALSE))
    
    g <- test[[1]]
    g_sl <- test[[2]]
    o_tbl <- test[[3]]
  
    plot(g)
    
    # save(g_sl, file = paste0("simulated_network_data_100/rho_", rho, "/", n1, "_", n2, "_", i, ".RData"))
    
    c_wt <- tryCatch(
      cluster_walktrap(g),
      error = function(e) {
        return(NA)
      })
    
    c_wt2 <- tryCatch(
      cluster_walktrap(g_sl),
      error = function(e) {
        return(NA)
      })
    
    c_l <- tryCatch(
      cluster_louvain(g),
      error = function(e) {
        return(NA)
      })
    
    c_l2 <- tryCatch(
      cluster_louvain(g_sl),
      error = function(e) {
        return(NA)
      })
    
    c_fg <- tryCatch(
      cluster_fast_greedy(g),
      error = function(e) {
        return(NA)
      })
    
    c_fg2 <- tryCatch(
      cluster_fast_greedy(g_sl),
      error = function(e) {
        return(NA)
      })
    
    c_eb <- tryCatch(
      cluster_edge_betweenness(g),
      error = function(e) {
        return(NA)
      })
    
    c_eb2 <- tryCatch(
      cluster_edge_betweenness(g_sl),
      error = function(e) {
        return(NA)
      })
    
    c_im <- tryCatch(
      cluster_infomap(g),
      error = function(e) {
        return(NA)
      })
    
    c_im2 <- tryCatch(
      cluster_infomap(g_sl),
      error = function(e) {
        return(NA)
      })
    
    c_lp <- tryCatch(
      cluster_label_prop(g),
      error = function(e) {
        return(NA)
      })
    
    c_lp2 <- tryCatch(
      cluster_label_prop(g_sl),
      error = function(e) {
        return(NA)
      })
    
    c_le <- tryCatch(
      cluster_leading_eigen(g),
      error = function(e) {
        return(NA)
      })
    
    c_le2 <- tryCatch(
      cluster_leading_eigen(g_sl),
      error = function(e) {
        return(NA)
      })
    
    c_sl <- tryCatch(
      cluster_spinglass(g),
      error = function(e) {
        return(NA)
      })
    
    c_sl2 <- tryCatch(
      cluster_spinglass(g_sl),
      error = function(e) {
        return(NA)
      }
    )
    
    # c_o <- cluster_optimal(g)
    # c_o2 <- cluster_optimal(g_sl)
    
    all_cs <- list(c_wt, c_wt2,
                   c_l, c_l2,
                   c_fg, c_fg2,
                   c_eb, c_eb2,
                   c_im, c_im2,
                   c_lp, c_lp2,
                   c_le, c_le2,
                   c_sl, c_sl2
    )
    
    cd_used <- c(
      "wt", "wt2",
      "l", "l2",
      "fg", "fg2",
      "eb", "eb2",
      "im", "im2",
      "lp", "lp2",
      "le", "le2",
      "sl", "sl2"
    )
    
    NMI_scores <- sapply(all_cs, FUN = function(x) {
      get_NMI(x, o_tbl)
    })
    
    res_tbl <- tibble(
      run = i,
      rho = rho,
      method = cd_used,
      NMI_scores = NMI_scores
    ) %>%
      rbind(res_tbl)
  }
  
  return(res_tbl)
}


# n1 = number of websites in the universe
# n2 = number of members of the audiences
# n3 = number of websites each person visits
# n4 = number of types of websites / people
# a = power law exponent

# n_simulations, N = the number of simulations

n_simulations = 100
from_rho = 0.4
to_rho = 0.4
a = 1.5
for(r in seq(from = from_rho, to = to_rho, by = 0.1)) {
  set.seed(1009)
  simulation_results <- run_simulation(n1 = 50, n2 = 100, n4 = 5, pl_exp = a, rho = r, N=n_simulations)
  # write_csv(simulation_results, paste0("results/CLOUD_NMI_PL_N_", n_simulations, "_rho_", r, "_alpha_", a, ".csv"))
}
