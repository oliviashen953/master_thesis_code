##### LOAD DATASET #######
new_Height_group1_with_ranks <- read_csv("new_Height_group1_with_ranks.csv")

TIE_new_Height_group1_with_rank <- new_Height_group1_with_ranks

pattern <- "^PGS00[0-9]{4}_rank$"
selected_data <- TIE_new_Height_group1_with_rank %>%
  select(id, matches(pattern))

########################
#######################
library(dplyr)
library(readr)
library(tidyr)

process_and_save_transposed_data <- function(group_number) {

  file_name <- sprintf("new_Height_group%d_with_ranks.csv", group_number)
  

  data <- read_csv(file_name)
  

  score_columns <- grep("PGS00[0-9]{4}_rank", names(data), value = TRUE)
  for (col_name in score_columns) {

    ranks <- rank(-data[[col_name]], ties.method = "average")
    total_rows <- nrow(data)
    top_5_percent_limit <- ceiling(0.05 * total_rows)  

    tie_rank_col_name <- sub("_rank", "_tie_rank", col_name)
    data[[tie_rank_col_name]] <- ifelse(ranks <= top_5_percent_limit, ranks,
                                        151 + (ceiling((ranks - top_5_percent_limit) / (0.05 * total_rows)) - 1))
  }

  output_file_name <- sprintf("tiered_rank_Height_group%d_with_ranks.csv", group_number)
  write_csv(data, output_file_name)
}


for (i in 1:8) {
  process_and_save_transposed_data(i)
}
