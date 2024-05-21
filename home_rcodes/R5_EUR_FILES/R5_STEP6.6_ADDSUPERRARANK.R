library(dplyr)
library(readr)


add_rank_columns <- function(df, pgs_pattern, ra_columns) {

  pgs_columns <- grep(pgs_pattern, names(df), value = TRUE)
  pgs_columns <- pgs_columns[!grepl("_rank$", pgs_columns)]
  

  df$pgs_average_rank <- rowMeans(df[pgs_columns], na.rm = TRUE)
  df$pgs_median_rank <- apply(df[pgs_columns], 1, median, na.rm = TRUE)
  

  df$pgs_average_rank <- rank(-df$pgs_average_rank, ties.method = "average")
  df$pgs_median_rank <- rank(-df$pgs_median_rank, ties.method = "average")
  

  df$RA_average_rank <- rowMeans(df[ra_columns], na.rm = TRUE)
  df$RA_median_rank <- apply(df[ra_columns], 1, median, na.rm = TRUE)
  

  df$RA_average_rank <- rank(df$RA_average_rank, ties.method = "average")
  df$RA_median_rank <- rank(df$RA_median_rank, ties.method = "average")
  
  df
}


process_file <- function(filename) {

  df <- read_csv(filename)
  

  ra_columns <- c("ams_rank", "bcs_rank", "logs_rank", "sbs_rank", "mct_rank", "mc4_rank")
  

  df <- add_rank_columns(df, "^PGS00", ra_columns)
  

  write_csv(df, filename)
  cat("Processed and saved:", filename, "\n")
}


patterns <- c("T2D", "BMI", "Height", "BrC")


for (pattern in patterns) {
  for (i in 1:20) {
    filename <- sprintf("EUR_R5_new_%s_group%d_with_ranks.csv", pattern, i)
    if (file.exists(filename)) {
      process_file(filename)
    } else {
      cat("File does not exist:", filename, "\n")
    }
  }
}
