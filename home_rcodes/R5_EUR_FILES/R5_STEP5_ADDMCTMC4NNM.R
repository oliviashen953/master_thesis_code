library(readr)

setwd("/n/home00/jiaxinshen")

library(readr)


process_T2D_group <- function(group_number) {

  file_name <- sprintf("EUR_R5_T2D_group%d.csv", group_number)
  T2D_group <- read_csv(file_name)
  

  score_columns <- grep("PGS", names(T2D_group), value = TRUE)
  for(col_name in score_columns) {
    rank_col_name <- paste0(col_name, "_rank")
    T2D_group[[rank_col_name]] <- rank(-T2D_group[[col_name]], ties.method = "average")
  }
  

  ordered_indices <- order(T2D_group$pheno, decreasing = TRUE)
  T2D_group$true_rank <- order(ordered_indices)
  

  norms <- c("ams", "bcs", "mu", "logs", "sbs")
  for(norm in norms) {
    norm_data <- read_csv(sprintf("skew-nuclear/matlab/R5_new_%s_T2D_group%d.csv", norm, group_number), col_names = FALSE)
    colnames(norm_data)[1] <- paste0(norm, "_score")
    T2D_group <- cbind(T2D_group, norm_data)
    

    rank_col_name <- paste0(norm, "_rank")
    T2D_group[[rank_col_name]] <- rank(-T2D_group[[paste0(norm, "_score")]], ties.method = "average")
  }
  

  mc4_data <- read_csv(sprintf("new_R5_mc4_T2D_group%d.csv", group_number))
  mct_data <- read_csv(sprintf("new_R5_mct_T2D_group%d.csv", group_number))
  T2D_group$mc4_rank <- mc4_data$mc4_rank
  T2D_group$mct_rank <- mct_data$mct_rank
  

  write.csv(T2D_group, sprintf("EUR_R5_new_T2D_group%d_with_ranks.csv", group_number), row.names = FALSE)
  cat(sprintf("Processed and saved group %d\n", group_number))
}


for(i in 1:20) {
  process_T2D_group(i)
}
