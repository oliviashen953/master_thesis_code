
files <- list.files()


rename_files <- function(prefix, files) {
  for (file in files) {
    if (grepl(paste0("^TEST_", prefix), file)) {

      new_file_name <- gsub("TEST_", "", file)

      file.rename(from = file, to = new_file_name)
      cat("Renamed", file, "to", new_file_name, "\n")
    }
  }
}


rename_files("AGE_R3_new_BMI_group", files)


rename_files("AGE_R3_new_Height_group", files)
