
traits <- c("Height", "BMI", "T2D", "BrC")
num_groups <- 20


file_directory <- "" 


rank_per_row <- function(df) {
  t(apply(df, 1, function(row) rank(-row, ties.method = "average"))) 
}



for (trait in traits) {
  for (group_num in 1:num_groups) {
    input_file_name <- paste0("transposed_AGE_R3_", trait, "_group", group_num, ".csv")
    output_file_name <- paste0("RANK_transposed_AGE_R3_", trait, "_group", group_num, ".csv")
    

    input_full_path <- paste0(file_directory, input_file_name)
    output_full_path <- paste0(file_directory, output_file_name)
    

    transposed_data <- read.csv(input_full_path, row.names = NULL, header = TRUE)
    

    ranked_data <- rank_per_row(transposed_data)
    

    write.csv(ranked_data, output_full_path, row.names = FALSE, col.names = TRUE)
  }
}


########## move RANK_transopsed files to mc4// mct #####
# Note: Make sure that the destination directory exists before running the script.