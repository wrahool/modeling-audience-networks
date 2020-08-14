library(tidyverse)
library(igraph)
library(aricode)
library(poweRlaw)
library(fGarch)
library(rjson)

# model 7

# the only difference between model 3 and model 4 is the calculation of prediction accuracy
# instead of doing a naive accuracy calculation, in this I'm using
# NMI or the Normalized Mutual Information

# in model 5, there's a new parameter: the exponent of the power law
# outlets' repute follow a power law and audience's choices depends on the outlet's reputation

# in model 6, the number of outlets each person visits is not randomly chosen from 1:n2
# instead they follow a positively (right) skewed bell curve

# you get accuracy < 1 even with rho = 0 iff some person(s) visit(s) only 1 specific website
# in this case, this happens in simulation number 53 where person # 84 visits only outlet # 47
# and no one else visits that outlet

# model 7 is identical to model 6. The only difference in this script is the calculation of 
# mixing parameter mx for each simulated network and tallying the values of mx with values of rho

# to debug this script, you need to first identify which simulation you want to debug
# and then pass stop_debug = TRUE for that simulation in the get_simulated_network function

################################################################
# model parameters

# n1: number of websites in the universe
# n2: number of members of the audiences
# n3: number of types of websites / people

# rho:
# each person visits a total of n4 websites
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


get_simulated_network <- function(n1, n2, n4, n3, a, rho, sk, stop_debug = FALSE) {
  
  outlet_ids <- 1:n1
  p_ids <- 1:n2
  types <- LETTERS[1:n3]
  
  if(a > 1) {
    outlet_rep <-  rpldis(n1, 1, alpha = a) # power law distribution
    outlet_rep_normalized <- outlet_rep / sum(outlet_rep)
  } else {
    outlet_rep <- rep(1, n1)
    outlet_rep_normalized <- outlet_rep / sum(outlet_rep)
  }
  
  outlets_tbl <- tibble(
    outlet_id = outlet_ids,
    outlet_name = paste("O", outlet_ids, sep = "_"),
    outlet_type = sample_atleast_once(types, n1), # at least one website of each type
    outlet_repute = outlet_rep_normalized
  )
  
  all_n4 <- rsnorm(n = n2, mean = 0, sd = 1, xi = sk)
  all_n4_scaled <- round(((all_n4 - min(all_n4))/(max(all_n4)-min(all_n4))*(n1-1) + 1))
  
  audience_tbl <- tibble(
    p_id = p_ids,
    p_name = paste("P", p_ids, sep = ""),
    p_type = sample_atleast_once(types, n2),       # at least one audience member of each type
    # p_n4 = sample(1:n1, n2, replace = TRUE)
    p_n4 = all_n4_scaled
  )
  
  audience_el <- NULL
  
  # if(stop_debug) {
  #   View(outlets_tbl)
  #   View(audience_tbl)
  # }
  
  # loop over each person
  for(p in 1:n2) {
    
    n4 <- audience_tbl$p_n4[p]
    
    # when rho is 0 all of their choices are selective
    # when rho is 1 all of their choices are random
    random_choices_allowed <- round(rho * n4)
    selective_choices_allowed <- n4 - random_choices_allowed
    
    selective_outlets_pool <- outlets_tbl %>%
      dplyr::filter(outlet_type == audience_tbl$p_type[p]) %>%
      select(outlet_id, outlet_repute)
    
    # if (stop_debug) {
    #   print(selective_outlets_pool)
    # }
    
    if(nrow(selective_outlets_pool) == 1) {                  # this is to prevent the bug where 1 type gets 1 outlet
      selective_outlets_pool <- selective_outlets_pool %>%
        rbind(selective_outlets_pool)
    }
    
    selective_chosen_outlets <- selective_outlets_pool %>%          # from
      pull(outlet_id) %>%                                           # all outlets in the selective outlets pool
      sample(selective_choices_allowed, replace = TRUE,             # randomly sample with prob = outlet repute (R auto-normalizes the probabilities of the subset)
             prob = selective_outlets_pool$outlet_repute)
# 
#     if (stop_debug) {
#       print("Person Number:")
#       print(p)
#       print("Visisted # of websites:")
#       print(n4)
#       print("These are the websites:")
#       print(selective_chosen_outlets)
#       print("----------------------------------------")
#       
#       fileConn<-file("output.txt")
#       writeLines(c(p," : ", n4, " : ", selective_chosen_outlets), fileConn)
#       close(fileConn)
# 
#     }

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
          p_name = paste("P", rep(p, n4), sep = "_"),             # one column is the p_id
          outlet_name = paste("O", all_chosen_outlets, sep = "_") # second column is the outlet_id
        )
      )
  }
  
  outlet_reach <- audience_el %>%
    pull(outlet_name) %>%
    table() %>%
    as_tibble() %>%
    dplyr::rename(uv = n) %>%
    select(outlet_name = 1, everything())
  
  audience_g <- graph_from_data_frame(audience_el, directed = F)
  V(audience_g)$type <- substr(V(audience_g)$name, 1, 1) == "O"
  
  projection_graphs <- bipartite_projection(audience_g, multiplicity = TRUE)
  outlet_projection <- projection_graphs$proj2
  
  V(outlet_projection)$type <- V(outlet_projection)$name %>%
    lapply(FUN = function(x) { 
      outlets_tbl %>%
        dplyr::filter(outlet_name == x) %>% 
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
      dplyr::filter(outlet_name == v) %>%
      pull(uv)
  }
  
  # if(stop_debug) {
  #   plot(outlet_projection)
  # }
  
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
      dplyr::filter(outlet_name %in% c[[i]]) %>%
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
    dplyr::filter(outlet_type == pred_type) %>%
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

# function to calculate the input mixing parameter of a graph ig,
# using outlet_types
  
get_mixing_parameter <- function(ig, outlet_types) {
    
    edge_ends <- data.frame(ends(ig, E(ig))) %>% 
      as_tibble()
    
    o_tbl <- outlet_types %>%
      select(outlet_name, outlet_type)
    
    c <- edge_ends %>%
      inner_join(o_tbl, by = c("X1" = "outlet_name")) %>%
      rename("X1_type" = "outlet_type") %>%
      inner_join(o_tbl, by = c("X2" = "outlet_name")) %>%
      rename("X2_type" = "outlet_type") %>%
      mutate(edgetype = ifelse(X1_type == X2_type, "in", "out")) %>%
      dplyr::filter(edgetype == "out") %>%
      nrow() %>%
      `/` (nrow(edge_ends))
    
    return(c)
}

run_simulation <- function(n1, n2, n4, n3, pl_exp, rho, sk, N) {
  res_tbl <- NULL
  rho_mxps <- NULL
  
  i <- 1
  while(i <= N) {
    message(paste0("rho : ", rho, " Run : ", i))
    
    # without debug
    test <- get_simulated_network(n1, n2, n4, n3, a, rho, sk, stop_debug = FALSE)
    
    # the next line is if you need to debug at a particular iteration (value of i)
    # test <- get_simulated_network(n1, n2, n4, n3, a, rho, sk, stop_debug = ifelse(i == 61, TRUE, FALSE))
    
    g <- test[[1]]
    g_sl <- test[[2]]
    o_tbl <- test[[3]]
    
    if(length(V(g)) <= 1) {
      message("Rerun...")
      next
    }
    # save(list(g, g_sl, o_tbl), file = paste0("network_data/mixing_parameter_rdata/rho_", rho, "_", n1, "_", n2, "_", i, ".RData"))
    
    rho_mxps <- tibble(
      curr_rho = rho,
      mxp1 = get_mixing_parameter(g, o_tbl),
      mxp2 = get_mixing_parameter(g_sl, o_tbl)
    ) %>%
      rbind(rho_mxps)
    
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
    
    # how to identify which simulation you need to investigate
    # if(NMI_scores[1] != 1) {
    #   print(i)
    #   print(length(cluster_walktrap(g)))
    # }
    
    res_tbl <- tibble(
      run = i,
      rho = rho,
      method = cd_used,
      NMI_scores = NMI_scores
    ) %>%
      rbind(res_tbl)
    
    i <- i+1
  }
  
  return(list(res_tbl, rho_mxps))
}


# n1 = number of websites in the universe
# n2 = number of members of the audiences
# n3 = number of types of websites / people
# a = power law exponent
# b = skewness of distribution of n4

# n_simulations, N = the number of simulations

model_params <- fromJSON(file = "params/model_params.json")

n_outlets <- model_params$n_outlets
n_audience <- model_params$n_audiences
n_types <- model_params$n_types
n_simulations <- model_params$n_simulations
from_rho <- model_params$from_rho
to_rho <- model_params$to_rho
rho_inc <- model_params$rho_inc
a = model_params$a
b = model_params$b

for(r in seq(from = from_rho, to = to_rho, by = rho_inc)) {
  
  # set the same seed for a specific rho so that errors within each rho can be easily replicated
  set.seed(108)
  
  simulation_results <- run_simulation(n1 = n_outlets,
                                       n2 = n_audience,
                                       n3 = n_types,
                                       pl_exp = a,
                                       rho = r,
                                       sk = b,
                                       N = n_simulations)
  
  write_csv(simulation_results[[1]], paste0("results/NMI_n1_", n_outlets,
                                                        "_n2_", n_audience,
                                                        "_n3_", n_types,
                                                        "_alpha_", a,
                                                        "_sk_", b,
                                                        "_N_", n_simulations,
                                                        "_rho_", r,".csv"))
  
  write_csv(simulation_results[[2]], paste0("results/MXP_n1_", n_outlets,
                                                        "_n2_", n_audience,
                                                        "_n3_", n_types,
                                                        "_alpha_", a,
                                                        "_sk_", b,
                                                        "_N_", n_simulations,
                                                        "_rho_", r,".csv"))
}
