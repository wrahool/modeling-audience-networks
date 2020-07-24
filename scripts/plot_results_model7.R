library(tidyverse)

set.seed(108)

nmi_file_indices <- list.files("results/") %>%
  startsWith(prefix = "NMI")

nmi_results <- NULL
for(file in list.files("results")[nmi_file_indices]) {
  nmi_result <- read_csv(paste0("results/", file))
  nmi_results <- nmi_results %>%
    rbind(nmi_result)
}

ggplot(nmi_results) +
  geom_boxplot(aes(x=as_factor(rho), y=NMI_scores)) +
  geom_hline(aes(yintercept=0.5), color = "#FF0000")+
  facet_wrap(.~method, nrow = 8, ncol = 2) +
  xlab("randomizing parameter") +
  ylab("normalized mutual information score") +
  theme_bw()

mxp_file_indices <- list.files("results/") %>%
  startsWith(prefix = "MXP")

mxp_results <- NULL
for(file in list.files("results/")[mxp_file_indices]) {
  mxp_result <- read_csv(paste0("results/", file))
  mxp_results <- mxp_results %>%
    rbind(mxp_result)
}

mxp_results_long <- mxp_results %>%
  gather(key = mxp_type, value = mxp_value, mxp1:mxp2)

ggplot(mxp_results_long) +
  geom_boxplot(aes(x=as.factor(curr_rho), y=mxp_value, fill=mxp_type)) +
  xlab("randomizing paramter") +
  ylab("mixing parameter") +
  theme_bw()

cor.test(mxp_results$curr_rho,
         mxp_results$mxp1, method = "kendall")

mxp_results %>%
  group_by(curr_rho) %>%
  rename(rho = curr_rho) %>%
  summarize(mean_mxp1 = mean(mxp1),
            mean_mxp2 = mean(mxp2)) %>%
  cor(method = "kendall") # very high rank correlation between rho and mean mxp

mxp_results %>%
  group_by(curr_rho) %>%
  rename(rho = curr_rho) %>%
  summarize(median_mxp1 = median(mxp1),
            median_mxp2 = median(mxp2)) %>%
  cor(method = "kendall") # very high rank correlation between rho and median mxp

# summary(lm(MXP_results$mxp1 ~ MXP_results$curr_rho))
