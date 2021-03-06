library(tidyverse)
library(stringr)

set.seed(108)

analyze_results <- function(n1, n2, n3, sk, alpha, opt, allNMI, N) {
  
  nmi_file_indices <- list.files("results/") %>%
    startsWith(prefix = paste("NMI_n1", n1,
                              "n2", n2,
                              "n3", n3,
                              "alpha", alpha,
                              "sk", sk, 
                              "optimal", opt,
                              "allNMI", allNMI, 
                              "N", N, sep = "_"))
  
  nmi_files <- list.files("results/")[nmi_file_indices]
  
  nmi_results <- NULL
  for(file in nmi_files) {
    nmi_result <- read_csv(paste0("results/", file))
    nmi_results <- nmi_results %>%
      rbind(nmi_result)
  }
  
  methods <- unique(gsub(pattern = "2", "", unique(nmi_results$method)))
  
  message("The following metrics are available for this set of parameters:")
  
  available_metrics <- names(nmi_results)[-c(1,2,3)]
  available_metrics <- available_metrics[available_metrics != "scaling_factors"]
  
  i <- 1
  for(a_m in available_metrics) {
    message(paste0(i, ": ", a_m))
    i <- i+1
  }
  
  metric_index <- readline(prompt="Enter the number corresponding to the metric you wish to use : ")
  metric_to_use <- available_metrics[as.numeric(metric_index)]

  use_scaling_factor <- readline(prompt = "Use scaling factor? 1 for Yes, 0 for No : ")
  use_scaling_factor <- as.logical(as.numeric(use_scaling_factor))
  
  message(paste0("Using ", metric_to_use))
  
  if (use_scaling_factor) {
    message("Using scaling factor ... ")
    
    nmi_results <- nmi_results %>%
      select(run, rho, method, metric_to_use, scaling_factors) %>%
      rename("metric" = 4) %>%
      mutate(metric = ifelse(is.nan(metric), 0, metric)) %>%
      mutate(metric = metric * scaling_factors) %>%
      select(-scaling_factors)
  }
  else {
    message("Not using scaling factor ...")
    
    nmi_results <- nmi_results %>%
      select(run, rho, method, metric_to_use) %>%
      rename("metric" = 4) %>%
      mutate(metric = ifelse(is.nan(metric), NA, metric))
    
  } 
  
  default_better_tbl <- NULL
  for(r in unique(nmi_results$rho)) {
    for(m in methods) {
      
      rho_m_nmi_results <- nmi_results %>%
        dplyr::filter(method %in% c(m, paste0(m, "2")), rho == r)
      
      rho_m_nmi_results_wide <- rho_m_nmi_results %>%
        spread(key = method, value = metric)
      
      wilcox_result_p <- tryCatch(
        wilcox.test(rho_m_nmi_results_wide[[m]],
                    rho_m_nmi_results_wide[[paste0(m, 2)]],
                    paired = TRUE, alternative = "l")$p.value,
        error = function(e) {
          return(NA)
        })
      
      default_better_tbl <- default_better_tbl %>%
        rbind(tibble(
          method = m,
          rho = r,
          default_worse_p = wilcox_result_p
        ))
    }
  }
  
  nmi_mean_sd <- nmi_results %>%
    group_by(method, rho) %>%
    summarize(meanNMI = mean(metric),
              sdNMI = sd(metric)) %>%
    ungroup() %>%
    mutate(lower_bound = meanNMI - sdNMI,
           upper_bound = meanNMI + sdNMI) %>%
    mutate(type = str_sub(method, -1)) %>%
    mutate(type = as_factor(ifelse(type %in% letters, 1, 2))) %>%
    mutate(method = gsub("2", "", method)) %>%
    mutate(type = ifelse(type == 1, "baseline", "augmented")) %>%
    rename(network = type) %>%
    select(method, network, rho, lower_bound, meanNMI, upper_bound)
  
  method <- nmi_mean_sd %>%
    pull(method) %>%
    unique()
  
  method_labels <- c("Edge Betweeneness", "Fast Greedy", "Infomap","Multilevel", 
                     "Leading Eigenvector", "Label Propagation","Spin-Glass", "WalkTrap")
  
  names(method_labels) <- method
  
  nmi_ribbonplot <- ggplot(nmi_mean_sd,
                           aes(x = rho,
                               color = network,
                               fill = network)) +
    geom_line(aes(x=rho,
                  y=meanNMI)) +
    geom_ribbon(aes(ymin = lower_bound,
                    ymax = upper_bound),
                alpha = .3,
                linetype = 0) +
    geom_hline(aes(yintercept = 0.5),
               color = "#FF0000",
               linetype = "dashed") +
    facet_wrap(~method,
               labeller = labeller(method = method_labels),
               nrow = 2,
               ncol = 4) +
    xlab(expression(rho)) +
    ylab("NMI") +
    theme_bw() +
    scale_x_continuous(breaks = seq(from = 0, to = 1, by = 0.2)) +
    scale_y_continuous(breaks = seq(from = 0, to = 1, by = 0.2)) +
    theme(
      strip.background = element_rect(
        color="black", fill="black", size=1.5, linetype="solid"
      ),
      strip.text.x = element_text(
        size = 12, color = "white"
      ),
      panel.grid.major.x = element_line(colour="gray90",size = rel(0.5)),
      panel.grid.minor.x = element_line(colour="gray95",size = rel(0.5)),
      panel.grid.major.y = element_line(colour="gray90",size = rel(0.5)),
      panel.grid.minor.y = element_line(colour="gray95",size = rel(0.5)),
      axis.text=element_text(size=7),
      legend.position = "bottom"
    )
  
  nmi_boxplot <- ggplot(nmi_results) +
    geom_boxplot(aes(x = as_factor(rho),
                     y = metric)) +
    geom_hline(aes(yintercept = 0.5),
               color = "#FF0000")+
    facet_wrap(.~method,
               nrow = 8,
               ncol = 2) +
    xlab("randomizing parameter") +
    ylab("normalized mutual information score") +
    theme_bw()
  
  return(list(nmi_ribbonplot, nmi_boxplot, default_better_tbl))
}

res <- analyze_results(n1 = 100, n2 = 1000, n3 = 5, sk = 3, alpha = 3, allNMI = TRUE, N = 100, opt = FALSE)

res[[1]]

# ggsave(res[[1]], filename = "plots/SNMI.eps", device=cairo_ps)
