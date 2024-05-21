library(readr)

library(readr)

process_BMI_group <- function(group_number) {

  file_name <- sprintf("AGE_R3_BMI_group%d.csv", group_number)
  BMI_group <- read_csv(file_name)
  

  score_columns <- grep("PGS", names(BMI_group), value = TRUE)
  for(col_name in score_columns) {
    rank_col_name <- paste0(col_name, "_rank")
    BMI_group[[rank_col_name]] <- rank(-BMI_group[[col_name]], ties.method = "average")
  }
  

  ordered_indices <- order(BMI_group$pheno, decreasing = TRUE)
  BMI_group$true_rank <- order(ordered_indices)
  

  norms <- c("ams", "bcs", "mu", "logs", "sbs")
  for(norm in norms) {
    norm_data <- read_csv(sprintf("skew-nuclear/matlab/R3_new_%s_BMI_group%d.csv", norm, group_number), col_names = FALSE)
    colnames(norm_data)[1] <- paste0(norm, "_score")
    BMI_group <- cbind(BMI_group, norm_data)
    

    rank_col_name <- paste0(norm, "_rank")
    BMI_group[[rank_col_name]] <- rank(-BMI_group[[paste0(norm, "_score")]], ties.method = "average")
  }
  

  mc4_data <- read_csv(sprintf("new_R3_mc4_BMI_group%d.csv", group_number))
  mct_data <- read_csv(sprintf("new_R3_mct_BMI_group%d.csv", group_number))
  BMI_group$mc4_rank <- mc4_data$mc4_rank
  BMI_group$mct_rank <- mct_data$mct_rank
  
 
  write.csv(BMI_group, sprintf("AGE_R3_new_BMI_group%d_with_ranks.csv", group_number), row.names = FALSE)
  cat(sprintf("Processed and saved group %d\n", group_number))
}

for(i in 1:20) {
  process_BMI_group(i)
}
