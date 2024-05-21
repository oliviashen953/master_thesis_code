library(readr)
library(dplyr)


assign_percentiles <- function(data, rank_method_name) {

  na_rows <- data %>% 
    filter(rank_method == rank_method_name, is.na(percentile)) %>%
    arrange(desc(average_pheno))
  

  percentiles <- c("top_5_percent", "top_10_percent", "top_20_percent", "top_30_percent",
                   "top_50_percent", "top_75_percent", "top_95_percent")
  na_rows$percentile <- factor(percentiles, levels = percentiles)
  

  data <- data %>% 
    filter(!(rank_method == rank_method_name & is.na(percentile))) %>%
    bind_rows(na_rows) %>%
    arrange(rank_method, percentile)
  
  return(data)
}


for (group_id in 1:20) {
  file_name <- sprintf("R1r2_new_BMI_group%d_averages_pheno_by_percentile.csv", group_id)
  
  if (file.exists(file_name)) {
    data <- read_csv(file_name)

    data <- assign_percentiles(data, "real_ols")
    data <- assign_percentiles(data, "real_african_ols")
    

    write.csv(data, file_name, row.names = FALSE)
  } else {
    message(sprintf("File %s not found, skipping...", file_name))
  }
}
