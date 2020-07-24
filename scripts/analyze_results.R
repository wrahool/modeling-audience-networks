library(tidyverse)

set.seed(108)

analyze_results <- function(n1, n2, n3, alpha, sk) {
  nmi_file_indices <- list.files("results/") %>%
    startsWith(prefix = paste("NMI_n1", n1,
                              "n2", n2,
                              "n3", n3,
                              "alpha", alpha,
                              "sk", sk, sep = "_"))
  
  nmi_files <- list.files("results/")[nmi_file_indices]
  
  nmi_results <- NULL
  for(file in nmi_files) {
    nmi_result <- read_csv(paste0("results/", file))
    nmi_results <- nmi_results %>%
      rbind(nmi_result)
  }
  
  nmi_plot <- ggplot(nmi_results) +
    geom_boxplot(aes(x=as_factor(rho), y=NMI_scores)) +
    geom_hline(aes(yintercept=0.5), color = "#FF0000")+
    facet_wrap(.~method, nrow = 8, ncol = 2) +
    xlab("randomizing parameter") +
    ylab("normalized mutual information score") +
    theme_bw()
  
  mxp_file_indices <- list.files("results/") %>%
    startsWith(prefix = paste("MXP_n1", n1,
                              "n2", n2,
                              "n3", n3,
                              "alpha", alpha,
                              "sk", sk, sep = "_"))
  
  mxp_files <- list.files("results/")[mxp_file_indices]
  
  mxp_results <- NULL
  for(file in mxp_files) {
    mxp_result <- read_csv(paste0("results/", file))
    mxp_results <- mxp_results %>%
      rbind(mxp_result)
  }
  
  mxp_results_long <- mxp_results %>%
    gather(key = mxp_type, value = mxp_value, mxp1:mxp2)
  
  mxp_plot <- ggplot(mxp_results_long) +
    geom_boxplot(aes(x=as.factor(curr_rho), y=mxp_value, fill=mxp_type)) +
    xlab("randomizing paramter") +
    ylab("mixing parameter") +
    theme_bw()
  
  overall_kendall <- cor.test(mxp_results$curr_rho,
           mxp_results$mxp1, method = "kendall")
  
  mean_kendall <- mxp_results %>%
    group_by(curr_rho) %>%
    rename(rho = curr_rho) %>%
    summarize(mean_mxp1 = mean(mxp1),
              mean_mxp2 = mean(mxp2)) %>%
    cor(method = "kendall") # very high rank correlation between rho and mean mxp
  
  median_kendall <- mxp_results %>%
    group_by(curr_rho) %>%
    rename(rho = curr_rho) %>%
    summarize(median_mxp1 = median(mxp1),
              median_mxp2 = median(mxp2)) %>%
    cor(method = "kendall") # very high rank correlation between rho and median mxp
  
  return(list(nmi_plot, mxp_plot, overall_kendall, mean_kendall, median_kendall))
}

res <- analyze_results(n1 = 50, n2 = 100, n3 = 5, alpha = 1.5, sk = 3)
