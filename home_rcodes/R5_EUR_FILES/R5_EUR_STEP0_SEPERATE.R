library(dplyr)

set.seed(123) 

sample_and_split_data <- function(dataset, trait_name) {

  eur_individuals <- dataset %>%
    filter(race == "eur")
  
  # if(nrow(eur_individuals) >= 3000) {
     supervised_sample <- sample_n(eur_individuals, 3000)
    cat("Sampled 3000 individuals of EUR race for supervised training in", trait_name, "\n")
  # } else {
  #   supervised_sample <- eur_individuals
  #   cat("Not enough EUR individuals available in", trait_name, ". Only", nrow(supervised_sample), "available.\n")
  # }
  

  unsupervised_data <- setdiff(dataset, supervised_sample)
  cat("Remaining data set aside for unsupervised training in", trait_name, "contains", nrow(unsupervised_data), "individuals.\n")
  

  supervised_filename <- paste0(trait_name, "_EUR_supervised_sample.csv")
  unsupervised_filename <- paste0(trait_name, "_EUR_unsupervised_data.csv")
  
  write.csv(supervised_sample, supervised_filename, row.names = FALSE)
  write.csv(unsupervised_data, unsupervised_filename, row.names = FALSE)
}


updated_eMERGEIII_pheno_covar_T2D <- read.csv("~/new_emergeIII/newEMERGE_WithRace/updated_eMERGEIII_pheno_covar_T2D.txt")
updated_eMERGEIII_pheno_covar_BrC <- read.csv("~/new_emergeIII/newEMERGE_WithRace/updated_eMERGEIII_pheno_covar_BrC.txt")
updated_eMERGEIII_pheno_covar_BMI <- read.csv("~/new_emergeIII/newEMERGE_WithRace/updated_eMERGEIII_pheno_covar_BMI.txt")
updated_eMERGEIII_pheno_covar_Height <- read.csv("~/new_emergeIII/newEMERGE_WithRace/updated_eMERGEIII_pheno_covar_Height.txt")
updated_eMERGEIII_pheno_covar_Height

sample_and_split_data(updated_eMERGEIII_pheno_covar_Height, "Height")
sample_and_split_data(updated_eMERGEIII_pheno_covar_BMI, "BMI")
sample_and_split_data(updated_eMERGEIII_pheno_covar_BrC, "Breast Cancer")
sample_and_split_data(updated_eMERGEIII_pheno_covar_T2D, "Type 2 Diabetes")
supervised_sample
