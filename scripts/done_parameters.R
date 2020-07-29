library(tidyverse)

all_results <- grep(pattern = "NMI.*.csv", perl = TRUE, list.files("results/"), value = TRUE)

all_results <- unique(gsub(pattern = "_N_100_rho_.*.csv", replacement = "", x = all_results))

x <- all_results[1]

all_params_matrix <- do.call(rbind,
                             lapply(all_results,
                                    FUN = function(x) {
                                      unlist(strsplit(x, "_"))[c(3,5,7,9,11)]
                                      }))
  

colnames(all_params_matrix) <- unlist(strsplit(all_results[1], "_"))[c(2,4,6,8,10)]

all_params_tbl <- as_tibble(all_params_matrix)
