library(readr)
library(dplyr)

calculate_or_rr <- function(data, rank_col, prop) {

  top_data <- data %>%
    arrange(get(rank_col)) %>%
    slice_head(prop = prop)
  bottom_data <- data %>%
    arrange(get(rank_col)) %>%
    slice_tail(prop = 1 - prop)
  
  top_cases <- sum(top_data$pheno == 1, na.rm = TRUE)
  top_controls <- sum(top_data$pheno == 0, na.rm = TRUE)
  bottom_cases <- sum(bottom_data$pheno == 1, na.rm = TRUE)
  bottom_controls <- sum(bottom_data$pheno == 0, na.rm = TRUE)
  
  # odds ratio (OR) and relative risk (RR)
  odds_top <- top_cases / top_controls
  odds_bottom <- bottom_cases / bottom_controls
  or <- odds_top / odds_bottom
  
  risk_top <- top_cases / (top_cases + top_controls)
  risk_bottom <- bottom_cases / (bottom_cases + bottom_controls)
  rr <- risk_top / risk_bottom
  

  percentile <- paste0(round(prop * 100), "%")
  return(data.frame(rank_method = rank_col, percentile = percentile, odds_ratio = or, relative_risk = rr))
}

for (group_id in 1:20) {
  file_name <- sprintf("EUR_R5_new_T2D_group%d_with_ranks.csv", group_id)
  

  if (file.exists(file_name)) {

    data <- read_csv(file_name)
    

    rank_columns <- c(names(data)[grepl("_rank$", names(data))],  "RA_average_rank", "RA_median_rank", "supervise3000_ols_rank_no")
    

    results_df <- data.frame(rank_method = character(),
                             percentile = character(),
                             odds_ratio = numeric(),
                             relative_risk = numeric(),
                             stringsAsFactors = FALSE)
    

    for (rank_col in rank_columns) {
      for (prop in c(0.05, 0.10, 0.20)) { # Top 5%, 10%, 20%
        results_df <- rbind(results_df, calculate_or_rr(data, rank_col, prop))
      }
    }
    
    output_file_name <- sprintf("R5_T2D_group%d_odds_ratio_and_rr.csv", group_id)
    write_csv(results_df, output_file_name)
  } else {
    warning(paste("File not found:", file_name))
  }
}







results_list <- list()


for (group_id in 1:20) {
  file_name <- sprintf("R5_T2D_group%d_odds_ratio_and_rr.csv", group_id)
  if (file.exists(file_name)) {
    results_list[[group_id]] <- read_csv(file_name)
  }
}

# all results into one dataframe
combined_results <- bind_rows(results_list)

# the mean OR and RR across all groups for each rank method and percentile
summary_results <- combined_results %>%
  group_by(rank_method, percentile) %>%
  summarize(
    mean_odds_ratio = mean(odds_ratio, na.rm = TRUE),
    mean_relative_risk = mean(relative_risk, na.rm = TRUE)
  ) %>%
  ungroup()


write_csv(summary_results, "R5_T2D_summary_odds_ratio_and_rr_across_groups.csv")
