# investigate  how eb2 and lp2 give 100% accuracy even when alpha = 1

# done: they are overfitting, producing 50 communities.

########################################################################
## next
# why are some algos producing < 1 accuracy when alpha == 0?

# check for eb

for(f in list.files("simulated_network_data/alpha_0/")) {
  load(paste0("simulated_network_data/alpha_0/", f))
  c_eb <- cluster_edge_betweenness(g_sl)
  if (length(c_eb) != 5) print(f) # 973
}


# this is happening when (in the oft chance, one type, ie A, B, C or D, has only one outlet)
# so say there is only one outlet in type B (say outlet 23)
# so when a person of type B has to choose say, 6 outlets of type B (alpha = 0)
# the sampling equation translates into
# sample(23, 6, replace - TRUE)
# instead of sampling 23 6 times, the sample function samples 6 times with replacement
# from 1 to 23


load("network_data/debug.RData")

library(igraph)
