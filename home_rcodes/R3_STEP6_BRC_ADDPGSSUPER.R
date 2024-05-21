library(readr)
R3_AGE_Sample3000_new_supervised_BrC_model_coefficients <- readRDS("~/R3_AGE_Sample3000_new_supervised_BrC_model_coefficients.rds")

new_supervised_BrC_model_coefficients <- R3_AGE_Sample3000_new_supervised_BrC_model_coefficients
valid_coefficients <- new_supervised_BrC_model_coefficients[!is.na(new_supervised_BrC_model_coefficients)]
valid_coefficients <- valid_coefficients[-1] 

for(group in 1:20) {
  

  file_name <- sprintf("AGE_R3_new_BrC_group%d_with_ranks.csv", group)
  

  group_data <- read_csv(file_name)
  

  valid_pgs_columns <- names(valid_coefficients)
  valid_pgs_columns <- valid_pgs_columns[valid_pgs_columns %in% colnames(group_data)]
  

  group_data$supervise3000_ols_score_no_intercept <- as.matrix(group_data[valid_pgs_columns]) %*% valid_coefficients
  

  group_data$supervise3000_ols_rank_no <- rank(-group_data$supervise3000_ols_score_no_intercept, ties.method = "average")
  

  output_file_name <- sprintf("AGE_R3_new_BrC_group%d_with_ranks.csv", group)
  write.csv(group_data, output_file_name, row.names = FALSE)
  

  message(sprintf("Completed processing for group %d", group))
}
