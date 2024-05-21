R3_AGE_Sample3000_new_supervised_BMI_model_coefficients <- readRDS("~/R3_AGE_Sample3000_new_supervised_BMI_model_coefficients.rds")
new_supervised_BMI_model_coefficients <- R3_AGE_Sample3000_new_supervised_BMI_model_coefficients
library(readr)
new_supervised_BMI_model_coefficients



library(readr)


for(group in 1:20) {
  

  new_supervised_BMI_model_coefficients <- R3_AGE_Sample3000_new_supervised_BMI_model_coefficients
  coefficients  <- new_supervised_BMI_model_coefficients
  

  pgs_columns <- names(coefficients)[-1] 
  

  file_name <- sprintf("AGE_R3_new_BMI_group%d_with_ranks.csv", group)
  group_data <- read_csv(file_name)
  
  
  group_data$supervise3000_ols_score_no_intercept <- as.matrix(group_data[pgs_columns]) %*% coefficients[-1]
  
  group_data$supervise3000_ols_score_intercept <- as.matrix(cbind(1, group_data[pgs_columns])) %*% coefficients
  

  last_col_no_intercept <- ncol(group_data) - 1 
  colnames(group_data)[last_col_no_intercept] <- 'supervise3000_ols_score_no_intercept'
  

  last_col_intercept <- ncol(group_data)
  colnames(group_data)[last_col_intercept] <- 'supervise3000_ols_score_intercept'
  

  group_data$supervise3000_ols_rank_no <- rank(-group_data$supervise3000_ols_score_no_intercept, ties.method = "average")
  group_data$supervise3000_ols_rank_intercept <- rank(-group_data$supervise3000_ols_score_intercept, ties.method = "average")
  

  output_file_name <- sprintf("TEST_AGE_R3_new_BMI_group%d_with_ranks.csv", group)
  write.csv(group_data, output_file_name, row.names = FALSE)
}
