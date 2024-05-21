library(readr)
library(dplyr)


for (group_id in 1:20) {
  file_name <- sprintf("updated_AGE_R3_new_Height_group%d_with_RA_ranks.csv", group_id)
  
  if (file.exists(file_name)) {

    data <- read_csv(file_name)
    

    rank_columns <- c(names(data)[grepl("_rank$", names(data))], "real_ols_rank_no", "real_african_ols_rank_no")
    

    results_df <- data.frame(rank_method = character(),
                             group_percentile = character(),
                             mean_top_group = numeric(),
                             mean_bottom_group = numeric(),
                             standardized_difference = numeric(),
                             relative_risk = numeric(),
                             stringsAsFactors = FALSE)
    
    for (rank_col in rank_columns) {

      for (top_percent in c(5, 10, 20)) {
        bottom_percent <- 100 - top_percent
        
      
        top_data <- data %>%
          arrange(get(rank_col)) %>%
          slice_head(prop = top_percent / 100)
        bottom_data <- data %>%
          arrange(get(rank_col)) %>%
          slice_tail(prop = bottom_percent / 100)
        
        mean_top <- mean(top_data$pheno, na.rm = TRUE)
        mean_bottom <- mean(bottom_data$pheno, na.rm = TRUE)
        
        
        SE_top <- sd(top_data$pheno) / sqrt(nrow(top_data))
        SE_bottom <- sd(bottom_data$pheno) / sqrt(nrow(bottom_data))
        

        pooled_SE <- sqrt((SE_top^2 + SE_bottom^2) / 2)
        standardized_difference <- (mean_top - mean_bottom) / pooled_SE
        

        relative_risk <- mean_top / mean_bottom
        

        results_df <- rbind(results_df, data.frame(rank_method = rank_col,
                                                   group_percentile = paste0("Top ", top_percent, "%"),
                                                   mean_top_group = mean_top,
                                                   mean_bottom_group = mean_bottom,
                                                   standardized_difference = standardized_difference,
                                                   relative_risk = relative_risk))
      }
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
  group_by(rank_method, group_percentile) %>%
  summarize(
    mean_standardized_difference = mean(standardized_difference, na.rm = TRUE),
    mean_relative_risk = mean(relative_risk, na.rm = TRUE),
    .groups = 'drop'
  )


print(summary_results)


write_csv(summary_results, "Height_summary_standardized_differences_and_rr_across_groups.csv")
