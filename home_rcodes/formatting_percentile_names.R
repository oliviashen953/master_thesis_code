library(readr)
library(dplyr)


file_paths <- c("SORTED_Summary_BMI_averages_pheno_by_percentile.csv",
                "SORTED_Summary_Height_averages_pheno_by_percentile.csv")


standardize_percentile <- function(data) {
  data %>%
    mutate(percentile = str_replace_all(percentile, "[[:space:]_]", "_")) %>%
    mutate(percentile = str_to_lower(percentile))
}


for (file_path in file_paths) {

  if (file.exists(file_path)) {

    data <- read_csv(file_path)
    

    standardized_data <- standardize_percentile(data)
    

    write_csv(standardized_data, file_path)
  } else {
    warning(paste("File not found:", file_path))
  }
}
