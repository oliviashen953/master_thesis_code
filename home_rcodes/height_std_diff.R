library(readr)
library(dplyr)


data <- read_csv("updated_race_R1_new_Height_group6_with_ranks.csv")


rank_columns <- names(data)[grepl("_rank$", names(data))]
rank_columns

results_df <- data.frame(rank_method = character(),
                         mean_top_5 = numeric(),
                         mean_bottom_95 = numeric(),
                         standardized_difference = numeric(),
                         relative_risk = numeric(),
                         stringsAsFactors = FALSE)

for (rank_col in rank_columns) {
  top_5_data <- data %>%
    arrange(get(rank_col)) %>%
    slice(1:ceiling(0.05 * n()))
  

  bottom_95_data <- data %>%
    arrange(desc(get(rank_col))) %>%
    slice(1:ceiling(0.95 * n()))
  

  mean_top_5 <- mean(top_5_data$pheno, na.rm = TRUE)
  mean_bottom_95 <- mean(bottom_95_data$pheno, na.rm = TRUE)
  

  SE_top_5 <- sd(top_5_data$pheno, na.rm = TRUE) / sqrt(nrow(top_5_data))
  SE_bottom_95 <- sd(bottom_95_data$pheno, na.rm = TRUE) / sqrt(nrow(bottom_95_data))
  

  pooled_SE <- sqrt((SE_top_5^2 + SE_bottom_95^2) / 2)
  

  standardized_difference <- (mean_top_5 - mean_bottom_95) / pooled_SE
  

  relative_risk <- mean_top_5 / mean_bottom_95
  
  
  results_df <- rbind(results_df, data.frame(rank_method = rank_col,
                                             mean_top_5 = mean_top_5,
                                             mean_bottom_95 = mean_bottom_95,
                                             standardized_difference = standardized_difference,
                                             relative_risk = relative_risk))
}


print(results_df)


write_csv(results_df, "Height_group6_standardized_differences_and_rr_height.csv")
