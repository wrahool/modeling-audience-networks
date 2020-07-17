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

#################################################################

# plotting accuracy + overfitting for 100 simulations per alpha
# instead of boxplot



