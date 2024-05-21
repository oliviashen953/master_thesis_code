############################################
############################################
# supervised size: 3000 from previous 20%
############################################
############################################
library(readr)
library(dplyr)

####### IMPORT SUPERVISED DATASETS #######
supervised_Height_whole_new <- read_csv("new_emergeIII/supervised_Height_whole_new.csv")

supervised_BMI_whole_new <- read_csv("new_emergeIII/supervised_BMI_whole_new.csv")

supervised_BrC_whole_new <- read_csv("new_emergeIII/supervised_BrC_whole_new.csv")
supervised_T2D_whole_new <- read_csv("new_emergeIII/supervised_T2D_whole_new.csv")


sample_and_save <- function(dataset, filename) {
  sampled_data <- dataset %>%
    sample_n(3000) 
  

  new_file_name <- paste0("sampled3000_", filename, ".csv")
  

  write_csv(sampled_data, new_file_name)
}


sample_and_save(supervised_Height_whole_new, "supervised_Height_whole_new")
sample_and_save(supervised_BMI_whole_new, "supervised_BMI_whole_new")
sample_and_save(supervised_BrC_whole_new, "supervised_BrC_whole_new")
sample_and_save(supervised_T2D_whole_new, "supervised_T2D_whole_new")
