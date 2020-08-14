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
  
  methods <- unique(gsub(pattern = "2", "", unique(nmi_results$method)))
  
  default_better_tbl <- NULL
  for(r in unique(nmi_results$rho)) {
    for(m in methods) {
      
      rho_m_nmi_results <- nmi_results %>%
        dplyr::filter(method %in% c(m, paste0(m, "2")), rho == r)
      
      # ggplot(rho_m_nmi_results) +
      #   geom_boxplot(aes(x=method, y=NMI_scores)) +
      #   theme_bw()
      
      rho_m_nmi_results_wide <- rho_m_nmi_results %>%
        spread(key = method, value = NMI_scores)
    
      wilcox_result_p <- tryCatch(
        wilcox.test(rho_m_nmi_results_wide[[m]],
                    rho_m_nmi_results_wide[[paste0(m, 2)]],
                    paired = TRUE, alternative = "l")$p.value,
        error = function(e) {
          return(NA)
      })
    
      # wilcox_result_p <- wilcox_result$p.value
      
      default_better_tbl <- default_better_tbl %>%
        rbind(tibble(
          method = m,
          rho = r,
          default_worse_p = wilcox_result_p
        ))
    }
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
  
  return(list(nmi_plot, mxp_plot, overall_kendall, mean_kendall, median_kendall, default_better_tbl))
}

res <- analyze_results(n1 = 100, n2 = 1000, n3 = 5, alpha = 3, sk = 3)

# ggplot(res[[6]]) +
#   geom_point(aes(y=default_worse_p < 0.05, x=rho)) +
#   facet_wrap(.~method, nrow = 4) +
#   theme_bw()
