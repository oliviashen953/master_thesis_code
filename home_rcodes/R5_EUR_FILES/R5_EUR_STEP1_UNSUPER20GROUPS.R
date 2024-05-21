###### import dataset ######
library(readr)
Type_2_Diabetes_EUR_unsupervised_data <- read_csv("Type 2 Diabetes_EUR_unsupervised_data.csv")
View(Type_2_Diabetes_EUR_unsupervised_data)

unsupervised_T2D_whole_new <- read_csv("Type 2 Diabetes_EUR_unsupervised_data.csv")
unsupervised_BrC_whole_new <- read_csv("Breast Cancer_EUR_unsupervised_data.csv")
unsupervised_Height_whole_new <- read_csv("Height_EUR_unsupervised_data.csv")
unsupervised_BMI_whole_new <- read_csv("BMI_EUR_unsupervised_data.csv")

#############################################################################
#############################################################################
######## VERSION 2-A: RANDOM NO REP WITHIN BUT CAN REPLICATE BETEEN GROUPS ###
############################################################################
############################################################################
set.seed(12345) 


create_and_save_random_groups <- function(dataset, num_groups, group_size, file_prefix) {
 
  group_indices <- vector("list", num_groups)
  
  for (i in 1:num_groups) {
   
    group_indices[[i]] <- sample(nrow(dataset), group_size, replace = FALSE)
  }
  

  for (i in 1:num_groups) {
    current_group <- dataset[group_indices[[i]], ]
    file_name <- paste0(file_prefix, "_group", i, ".csv")
    write.csv(current_group, file_name, row.names = FALSE)
  }
  
  return(invisible(NULL)) 
}


num_groups <- 20
group_size <- 3000



create_and_save_random_groups(unsupervised_T2D_whole_new, num_groups, group_size, "EUR_R5_T2D")
create_and_save_random_groups(unsupervised_BrC_whole_new, num_groups, group_size, "EUR_R5_BrC")
create_and_save_random_groups(unsupervised_Height_whole_new, num_groups, group_size, "EUR_R5_Height")
create_and_save_random_groups(unsupervised_BMI_whole_new, num_groups, group_size, "EUR_R5_BMI")



