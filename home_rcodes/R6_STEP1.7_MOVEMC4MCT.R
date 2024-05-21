
traits <- c("Height", "BMI", "T2D", "BrC")
num_groups <- 20


file_directory <- ""


destination_directory <- "mc4-tmp/MC4-1/test_datasets/"


if (!dir.exists(destination_directory)) {
  dir.create(destination_directory, recursive = TRUE)
}


for (trait in traits) {

  for (group_num in 1:num_groups) {

    file_name <- paste0("RANK_transposed_TIE_R6_", trait, "_group", group_num, ".csv")
    

    input_full_path <- paste0(file_directory, file_name)
    output_full_path <- paste0(destination_directory, file_name)
    
    file.rename(input_full_path, output_full_path)
  }
}
