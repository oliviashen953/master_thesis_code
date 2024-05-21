new_T2D_group1_with_ranks <- read_csv("new_T2D_group1_with_ranks.csv")
new_T2D_group2_with_ranks <- read_csv("new_T2D_group2_with_ranks.csv")
new_T2D_group3_with_ranks <- read_csv("new_T2D_group3_with_ranks.csv")
new_T2D_group4_with_ranks <- read_csv("new_T2D_group4_with_ranks.csv")
new_T2D_group5_with_ranks <- read_csv("new_T2D_group5_with_ranks.csv")
new_T2D_group6_with_ranks <- read_csv("new_T2D_group6_with_ranks.csv")
new_T2D_group7_with_ranks <- read_csv("new_T2D_group7_with_ranks.csv")
new_T2D_group8_with_ranks <- read_csv("new_T2D_group8_with_ranks.csv")
new_T2D_group9_with_ranks <- read_csv("new_T2D_group9_with_ranks.csv")
new_T2D_group10_with_ranks <- read_csv("new_T2D_group10_with_ranks.csv")


library(dplyr)
library(readr)
library(tidyr)

process_and_save_transposed_data <- function(group_number) {
  file_name <- paste("new_T2D_group", group_number, "_with_ranks.csv", sep="")
  
  data <- read_csv(file_name)
  
  pattern <- "^PGS00[0-9]{4}_rank$"
  selected_data <- data %>%
    select(id, matches(pattern))
  

  long_data <- selected_data %>%
    pivot_longer(cols = -id, names_to = "variable", values_to = "value")
  

  wide_data <- long_data %>%
    pivot_wider(names_from = id, values_from = value, names_prefix = "SUBJECTID_")
  
  output_file_name <- paste("PGS_RANKS_transposed_T2D_group", group_number, "_with_ranks.csv", sep="")
  write_csv(wide_data, output_file_name)
}

for (i in 1:10) {
  process_and_save_transposed_data(i)
}

