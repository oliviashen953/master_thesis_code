##################################################################################
##################################################################################
################ keep row name into Subject_ID_000000 ######################
##################################################################################
##################################################################################

traits <- c("Height", "BMI", "T2D", "BrC")
num_groups <- 20


file_directory <- ""


for (trait in traits) {

  for (group_num in 1:num_groups) {

    file_name <- paste0("TIE_R6_", trait, "_group", group_num, "_TIERANK.csv")
    
    full_path <- paste0(file_directory, file_name)
    

    data <- read.csv(full_path)
    

    
    filtered_columns <- c('id', grep("^PGS00[0-9]{4}_TIERANK$", names(data), value = TRUE))
    data_filtered <- data[, filtered_columns]
    

    transposed_data <- as.data.frame(t(data_filtered[,-1])) 
    

    colnames(transposed_data) <- paste("Subject_id:", data_filtered$id)
    

    output_file_name <- paste0("transposed_TIE_R6_", trait, "_group", group_num, ".csv")
    output_full_path <- paste0(file_directory, output_file_name)
    write.csv(transposed_data, output_full_path, row.names = FALSE, quote = FALSE)
  }
}
