library(tidyverse)
library(igraph)
library(aricode)

# the only difference between model 3 and model 4 is the calculation of prediction accuracy
# instead of doing a naive accuracy calculation, in this I'm using
# NMI or the Normalized Mutual Information

################################################################
# model paramters

# n1: number of websites in the universe
# n2: number of members of the audiences
# n4: number of types of websites / people

# alpha:
# each person visits a total of n3 websites
# which websites they visit depends on a tuning parameter alpha (which controls their randomness)
# when alpha is 0 they can only visit websites whose outlet_id == their own p_id
# when alpha is 1 they can visit any website
# when alpha is 0.5, half of the websites they visit can be any website, the other half have to be restricted to those whose outlet_id == p_id

################################################################

get_simulated_network <- function(n1, n2, n3, n4, alpha) {
  
  # this model no longer uses n3. Instead each person visits a different number of websites
  # if n3 is missing choose a random number from 2 to half the number of outlets in the universe
  # if(missing(n3)) {
  #   n3 <- sample(seq(from = 2,
  #                    to = ifelse((n1/2)%%2 == 0,
  #                                n1/2,
  #                                (n1/2)-1),
  #                    by = 2),
  #                1)
  # }
  
  outlet_ids <- 1:n1
  p_ids <- 1:n2
  types <- LETTERS[1:n4]
  
  outlets_tbl <- tibble(
    outlet_id = outlet_ids,
    outlet_name = paste("O", outlet_ids, sep = "_"),
    # outlet_type = rep(types, each = n1/n4)
    outlet_type = sample(types, size = n1, replace = TRUE)
  )
  
  audience_tbl <- tibble(
    p_id = p_ids,
    p_name = paste("P", p_ids, sep = ""),
    # p_type = rep(types, each = n2/n4)
    p_type = sample(types, size = n2, replace = TRUE)
  )
  
  audience_el <- NULL
  
  # loop over each person
  for(p in 1:n2) {
    
    n3 <- sample(1:n1, 1)
    
    # when alpha is 0 all of their choices are selective
    # when alpha is 1 all of their choices are random
    random_choices_allowed <- round(alpha * n3)
    selective_choices_allowed <- n3 - random_choices_allowed
    
    # print(n3)
    # print(random_choices_allowed)
    # print(selective_choices_allowed)
    
    if(outlets_tbl %>%
       filter(outlet_type == audience_tbl$p_type[p]) %>%
       pull(outlet_id) %>%
       length() == 1) {
      
      selective_chosen_outlets <- outlets_tbl %>%               # only from
        filter(outlet_type == audience_tbl$p_type[p]) %>%       # those outlets where outlet_type == p_type
        pull(outlet_id) %>%                                     #
        rep(2) %>%
        sample(selective_choices_allowed, replace = TRUE)
      
    } else {
      
      selective_chosen_outlets <- outlets_tbl %>%               # only from
        filter(outlet_type == audience_tbl$p_type[p]) %>%       # those outlets where outlet_type == p_type
        pull(outlet_id) %>%                                     #
        sample(selective_choices_allowed, replace = TRUE)       # sample the number of selective outlets allowed
    }
    
    random_chosen_outlets <- outlets_tbl %>%                  # from
      pull(outlet_id) %>%                                     # all outlets in the universe
      sample(random_choices_allowed, replace = TRUE)          # sample the number of random outlets allowed
    
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

run_simulation <- function(n1, n2, n3, n4, alpha, N) {
  res_tbl <- NULL
  all_o_ds <- NULL
  for(i in 1:N) {
    message(paste0("alpha : ", alpha, " Run : ", i))
    test <- get_simulated_network(n1, n2, n3, n4, alpha)
    
    g <- test[[1]]
    g_sl <- test[[2]]
    o_tbl <- test[[3]]
    
    # save(g_sl, file = paste0("simulated_network_data_100/alpha_", alpha, "/", n1, "_", n2, "_", i, ".RData"))
    
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
      alpha = alpha,
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

# n_simulations, N = the number of simulations

n_simulations = 100
from_alpha = 0
to_alpha = 1
for(a in seq(from = from_alpha, to = to_alpha, by = 0.1)) {
  set.seed(1009)
  simulation_results <- run_simulation(n1 = 50, n2 = 100, n4 = 5, alpha = a, N=n_simulations)
  write_csv(simulation_results, paste0("results/CLOUD_NMI_N_", n_simulations, "_alpha_", a, ".csv"))
}
