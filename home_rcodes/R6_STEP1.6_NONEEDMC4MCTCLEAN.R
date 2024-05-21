library(readr)
TIE_R6_T2D_group1 <- read_csv("TIE_R6_T2D_group1.csv")
View(TIE_R6_T2D_group1)


traits <- c("Height", "BMI", "T2D", "BrC")
num_groups <- 20


file_directory <- ""



for (trait in traits) {

  for (group_num in 1:num_groups) {
  
    input_file_name <- paste0("transposed_TIE_R6_", trait, "_group", group_num, ".csv")
    output_file_name <- paste0("RANK_transposed_TIE_R6_", trait, "_group", group_num, ".csv")
    

    input_full_path <- paste0(file_directory, input_file_name)
    output_full_path <- paste0(file_directory, output_file_name)
    

    transposed_data <- read.csv(input_full_path, row.names = NULL, header = TRUE)
  
    
    write.csv(transposed_data, output_full_path, row.names = FALSE, col.names = TRUE)
  }
}
 
