library(readr)
library(dplyr)

library(readr)
AGE_R3_new_Height_group1_with_ranks <- read_csv("AGE_R3_new_Height_group1_with_ranks.csv")
View(AGE_R3_new_Height_group1_with_ranks)


for (group_id in 1:20) {

  file_name <- sprintf("AGE_R3_new_Height_group%d_with_ranks.csv", group_id)
  

  if (file.exists(file_name)) {

    data <- read_csv(file_name)
    

    rank_columns <- c(names(data)[grepl("_rank$", names(data))],"RA_average_rank", "RA_median_rank", "supervise3000_ols_rank_no")
    
    

    results_df <- data.frame(rank_method = character(),
                             mean_top_5 = numeric(),
                             mean_bottom_95 = numeric(),
                             standardized_difference = numeric(),
                             relative_risk = numeric(),
                             stringsAsFactors = FALSE)
    
    for (rank_col in rank_columns) {

      top_5_data <- data %>%
        arrange(get(rank_col)) %>%
        slice_head(prop = 0.05)
      bottom_95_data <- data %>%
        arrange(get(rank_col)) %>%
        slice_tail(prop = 0.95)
      

      mean_top_5 <- mean(top_5_data$pheno, na.rm = TRUE)
      mean_bottom_95 <- mean(bottom_95_data$pheno, na.rm = TRUE)
      
     
      SE_top_5 <- sd(top_5_data$pheno) / sqrt(nrow(top_5_data))
      SE_bottom_95 <- sd(bottom_95_data$pheno) / sqrt(nrow(bottom_95_data))
  
      
      pooled_SE <- sqrt((SE_top_5^2 + SE_bottom_95^2) / 2)
      standardized_difference <- (mean_top_5 - mean_bottom_95) / pooled_SE
      
    
      relative_risk <- mean_top_5 / mean_bottom_95

      results_df <- rbind(results_df, data.frame(rank_method = rank_col,
                                                 mean_top_5 = mean_top_5,
                                                 mean_bottom_95 = mean_bottom_95,
                                                 standardized_difference = standardized_difference,
                                                 relative_risk = relative_risk))
    }
    

    output_file_name <- sprintf("Height_group%d_standardized_differences_and_rr.csv", group_id)
    write_csv(results_df, output_file_name)
  } else {
    warning(paste("File not found:", file_name))
  }
}


















library(readr)
library(dplyr)

results_list <- list()


for (group_id in 1:20) {
  file_name <- sprintf("Height_group%d_standardized_differences_and_rr.csv", group_id)
  

  if (file.exists(file_name)) {
    results_list[[group_id]] <- read_csv(file_name)
  } else {
    warning(paste("File not found:", file_name))
  }
}

combined_results <- bind_rows(results_list)


summary_results <- combined_results %>%
  group_by(rank_method) %>%
  summarize(
    mean_standardized_difference = mean(standardized_difference, na.rm = TRUE),
    mean_relative_risk = mean(relative_risk, na.rm = TRUE)
  ) %>%
  ungroup()


print(summary_results)


write_csv(summary_results, "Height_summary_standardized_differences_and_rr_across_groups.csv")

