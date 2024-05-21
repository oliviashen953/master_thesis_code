library(dplyr)
library(readr)

add_tiered_ranking_and_clean <- function(file_path, num_groups) {
  for (i in 1:num_groups) {

    file_name <- paste0(file_path, "_group", i, ".csv")

    group_data <- read_csv(file_name)

    pgs_columns <- grep("PGS00[0-9]{4}", names(group_data), value = TRUE)
    

    for (pgs_col in pgs_columns) {
      rank_col_name <- paste0(pgs_col, "_TIERANK")
      group_data[[rank_col_name]] <- rank(-group_data[[pgs_col]], ties.method = "average")
      

      group_data[[rank_col_name]] <- case_when(
        group_data[[rank_col_name]] <= 150 ~ group_data[[rank_col_name]],
        group_data[[rank_col_name]] > 150 & group_data[[rank_col_name]] <= 300 ~ 151,
        group_data[[rank_col_name]] > 300 & group_data[[rank_col_name]] <= 450 ~ 152,
        group_data[[rank_col_name]] > 450 & group_data[[rank_col_name]] <= 600 ~ 153,
        group_data[[rank_col_name]] > 600 & group_data[[rank_col_name]] <= 750 ~ 154,
        group_data[[rank_col_name]] > 750 & group_data[[rank_col_name]] <= 900 ~ 155,
        group_data[[rank_col_name]] > 900 & group_data[[rank_col_name]] <= 1050 ~ 156,
        group_data[[rank_col_name]] > 1050 & group_data[[rank_col_name]] <= 1200 ~ 157,
        group_data[[rank_col_name]] > 1200 & group_data[[rank_col_name]] <= 1350 ~ 158,
        group_data[[rank_col_name]] > 1350 & group_data[[rank_col_name]] <= 1500 ~ 159,
        group_data[[rank_col_name]] > 1500 & group_data[[rank_col_name]] <= 1650 ~ 160,
        group_data[[rank_col_name]] > 1650 & group_data[[rank_col_name]] <= 1800 ~ 161,
        group_data[[rank_col_name]] > 1800 & group_data[[rank_col_name]] <= 1950 ~ 162,
        group_data[[rank_col_name]] > 1950 & group_data[[rank_col_name]] <= 2100 ~ 163,
        group_data[[rank_col_name]] > 2100 & group_data[[rank_col_name]] <= 2250 ~ 164,
        group_data[[rank_col_name]] > 2250 & group_data[[rank_col_name]] <= 2400 ~ 165,
        group_data[[rank_col_name]] > 2400 & group_data[[rank_col_name]] <= 2550 ~ 166,
        group_data[[rank_col_name]] > 2550 & group_data[[rank_col_name]] <= 2700 ~ 167,
        group_data[[rank_col_name]] > 2700 & group_data[[rank_col_name]] <= 2850 ~ 168,
        TRUE ~ 169
      )
    }
    

    group_data <- group_data %>% select(-one_of(pgs_columns))
    

    write.csv(group_data, paste0(file_path, "_group", i, "_TIERANK.csv"), row.names = FALSE)
  }
  
  return(invisible(NULL))  
}


traits <- c("T2D", "BMI", "BrC", "Height")
file_prefix <- "TIE_R6_"


for (trait in traits) {
  file_path <- paste0(file_prefix, trait)
  add_tiered_ranking_and_clean(file_path, 20)
}
