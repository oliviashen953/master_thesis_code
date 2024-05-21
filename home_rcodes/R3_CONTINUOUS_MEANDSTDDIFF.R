library(readr)
library(dplyr)


for (group_id in 1:20) {

  file_name <- sprintf("AGE_R3_new_Height_group%d_with_ranks.csv", group_id)
  

  if (file.exists(file_name)) {

    data <- read_csv(file_name)
    

    rank_columns <- c(names(data)[grepl("_rank$", names(data))], "RA_average_rank", "RA_median_rank", "supervise3000_ols_rank_no")
    

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
    

    output_file_name <- sprintf("R3_Height_group%d_standardized_differences_and_rr.csv", group_id)
    write_csv(results_df, output_file_name)
  } else {
    warning(paste("File not found:", file_name))
  }
}

















library(readr)
library(dplyr)

# Initialize an empty list to store the results data frames
results_list <- list()

# Loop over each group file and read the data
for (group_id in 1:20) {
  file_name <- sprintf("R3_Height_group%d_standardized_differences_and_rr.csv", group_id)
  
  # Check if the file exists
  if (file.exists(file_name)) {
    results_list[[group_id]] <- read_csv(file_name)
  } else {
    warning(paste("File not found:", file_name))
  }
}

# Combine all individual group data frames into one data frame
combined_results <- bind_rows(results_list)

# Calculate the mean standardized difference and mean relative risk for each rank_method and group_percentile
summary_results <- combined_results %>%
  group_by(rank_method, group_percentile) %>%
  summarize(
    mean_standardized_difference = mean(standardized_difference, na.rm = TRUE),
    mean_relative_risk = mean(relative_risk, na.rm = TRUE),
    .groups = 'drop'  # This ensures the result is ungrouped after summarization
  )

# View the summary results
print(summary_results)

# Save the summary to a new CSV file
write_csv(summary_results, "R3_Height_summary_standardized_differences_and_rr_across_groups.csv")
