library(tidyverse)
library(igraph)

##################################################
# model paramters

n1 <- 50 # number of websites in the universe
n2 <- 1000 # number of members of the audiences
n3 <- 10 # number of websites each person visits
n4 <- 5 # number ot types of websites / people

##################################################

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

# each person visits a total of n3 websites
# which websites they visit depends on a tuning parameter tau (which controls their randomness)
# when tau is 0 they can only visit websites whose outlet_id == their own p_id
# when tau is 1 they can visit any website
# when tau is 0.5, half of the websites they visit can be any website, the other half have to be restricted to those whose outlet_id == p_id

tau <- 0.01
audience_el <- NULL

# loop over each person
for(p in 1:1000) {
  
  # when tau is 0 all of their choices are selective
  # when tau is 1 all of their choices are random
  selective_choices_allowed <- (1-tau) * n3
  random_choices_allowed <- tau * n3
  

  selective_chosen_outlets <- outlets_tbl %>%               # only from
    filter(outlet_type == audience_tbl$p_type[p]) %>%       # those outlets where outlet_type == p_type
    pull(outlet_id) %>%                                     #
    sample(selective_choices_allowed, replace = TRUE)       # sample the number of selective outlets allowed
  
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

audience_g <- graph_from_data_frame(audience_el, directed = F)
V(audience_g)$type <- substr(V(audience_g)$name, 1, 1) == "O"

projection_graphs <- bipartite_projection(audience_g, multiplicity = TRUE)

outlet_projection <- projection_graphs$proj2

V(outlet_projection)$type <- unlist(
                                lapply(V(outlet_projection)$name,
                                  FUN = function(x) { 
                                    outlets_tbl %>%
                                      filter(outlet_name == x) %>% 
                                      pull(outlet_type)
                                    }
                                  )
                              )
  

# colrs <- c("gray50", "tomato", "gold", "purple", "cyan")
V(outlet_projection)$color <- ifelse(V(outlet_projection)$type == "A", "gray50",
                                     ifelse(V(outlet_projection)$type == "B", "tomato",
                                            ifelse(V(outlet_projection)$type == "C", "gold",
                                                   ifelse(V(outlet_projection)$type == "D", "olivedrab4",
                                                          "cyan"))))

plot.igraph(outlet_projection)

