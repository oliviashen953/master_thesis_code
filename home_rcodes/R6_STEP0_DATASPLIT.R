eMERGEIII_pheno_covar_Height <- read.csv("~/new_emergeIII/eMERGEIII_pheno_covar_Height.txt", sep="")
eMERGEIII_pheno_covar_BMI <- read.csv("~/new_emergeIII/eMERGEIII_pheno_covar_BMI.txt", sep="")
eMERGEIII_pheno_covar_BrC <- read.csv("~/new_emergeIII/eMERGEIII_pheno_covar_BrC.txt", sep="")
eMERGEIII_pheno_covar_T2D <- read.csv("~/new_emergeIII/eMERGEIII_pheno_covar_T2D.txt", sep="")



library(dplyr)

set.seed(123)  


sample_and_split_data <- function(dataset, trait_name) {

  if(nrow(dataset) >= 3000) {
    supervised_sample <- sample_n(dataset, 3000)
    cat("Sampled 3000 individuals for supervised training in", trait_name, "\n")
  } else {
    supervised_sample <- dataset
    cat("Not enough individuals available in", trait_name, ". Only", nrow(supervised_sample), "available.\n")
  }
  

  unsupervised_data <- setdiff(dataset, supervised_sample)
  cat("Remaining data set aside for unsupervised training in", trait_name, "contains", nrow(unsupervised_data), "individuals.\n")
  

  supervised_filename <- paste0(trait_name, "_TIE_supervised_sample.csv")
  unsupervised_filename <- paste0(trait_name, "_TIE_unsupervised_data.csv")
  
  write.csv(supervised_sample, supervised_filename, row.names = FALSE)
  write.csv(unsupervised_data, unsupervised_filename, row.names = FALSE)
}



eMERGEIII_pheno_covar_Height <- read.csv("~/new_emergeIII/eMERGEIII_pheno_covar_Height.txt", sep="")
eMERGEIII_pheno_covar_BMI <- read.csv("~/new_emergeIII/eMERGEIII_pheno_covar_BMI.txt", sep="")
eMERGEIII_pheno_covar_BrC <- read.csv("~/new_emergeIII/eMERGEIII_pheno_covar_BrC.txt", sep="")
eMERGEIII_pheno_covar_T2D <- read.csv("~/new_emergeIII/eMERGEIII_pheno_covar_T2D.txt", sep="")

sample_and_split_data(eMERGEIII_pheno_covar_Height, "Height")
sample_and_split_data(eMERGEIII_pheno_covar_BMI, "BMI")
sample_and_split_data(eMERGEIII_pheno_covar_BrC, "Breast Cancer")
sample_and_split_data(eMERGEIII_pheno_covar_T2D, "Type 2 Diabetes")
supervised_sample
