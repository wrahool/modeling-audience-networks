library(tidyverse)

######################################################
# plotting results for 100 simulations per alpha

simulation_results = read_csv("data/simulation_results_100_per_alpha_0_to_1.csv")

# ggplot(simulation_results) +
#   geom_boxplot(aes(x=method, y=accuracies)) +
#   theme_bw()

ggplot(simulation_results) +
  geom_boxplot(aes(x=as_factor(alpha), y=accuracies)) +
  facet_wrap(.~method, nrow = 7, ncol = 2) +
  xlab("alpha") +
  theme_bw()

ggsave("plots/simulation_results_100_per_alpha_0_to_1.pdf")

# without sl and sl2
simulation_results_bkup <- simulation_results %>%
  filter(!method %in% c("sl", "sl2"))

ggplot(simulation_results_bkup) +
  geom_boxplot(aes(x=as_factor(alpha), y=accuracies)) +
  facet_wrap(.~method, nrow = 7, ncol = 2) +
  theme_bw()

#################################################################

# plotting results for 1000 simulations per alpha

reqd_file_indices <- list.files("data") %>%
  grep(pattern = "CLOUD_N_1000")

reqd_files <- list.files("data")[reqd_file_indices] 

sim_results <- NULL

for(file in reqd_files) {
  sim_result_file <- read_csv(paste0("data/", file))
  sim_results <- sim_result_file %>%
    rbind(sim_results)
}

ggplot(sim_results) +
  geom_boxplot(aes(x=as_factor(alpha), y=accuracies)) +
  facet_wrap(.~method, nrow = 7, ncol = 2) +
  xlab("alpha") +
  theme_bw()

ggsave("plots/simulation_results_1000_per_alpha_0_to_1.pdf")

# without sl and sl2
sim_results_bkup <- sim_results %>%
  filter(!method %in% c("sl", "sl2"))

ggplot(sim_results_bkup) +
  geom_boxplot(aes(x=as_factor(alpha), y=accuracies)) +
  facet_wrap(.~method, nrow = 7, ncol = 2) +
  theme_bw()

# next steps:
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