eMERGEIII_pheno_covar_Height <- read.csv("~/new_emergeIII/eMERGEIII_pheno_covar_Height.txt", sep="")
eMERGEIII_pheno_covar_BMI <- read.csv("~/new_emergeIII/eMERGEIII_pheno_covar_BMI.txt", sep="")
eMERGEIII_pheno_covar_BrC <- read.csv("~/new_emergeIII/eMERGEIII_pheno_covar_BrC.txt", sep="")
eMERGEIII_pheno_covar_T2D <- read.csv("~/new_emergeIII/eMERGEIII_pheno_covar_T2D.txt", sep="")


library(dplyr)


count_older_than_65 <- function(dataset, dataset_name) {
  count <- dataset %>%
    filter(age > 65) %>%
    nrow()
  cat(dataset_name, "has", count, "individuals over the age of 65.\n")
}

count_older_than_65(eMERGEIII_pheno_covar_Height, "Height")
count_older_than_65(eMERGEIII_pheno_covar_BMI, "BMI")
count_older_than_65(eMERGEIII_pheno_covar_BrC, "Breast Cancer")
count_older_than_65(eMERGEIII_pheno_covar_T2D, "Type 2 Diabetes")

set.seed(123)  # Set seed for reproducibility


sample_and_split_data <- function(dataset, trait_name) {
  eligible_older_individuals <- dataset %>%
    filter(age > 65)

  if(nrow(eligible_older_individuals) >= 3000) {
    supervised_sample <- sample_n(eligible_older_individuals, 3000)
    cat("Sampled 3000 individuals over 65 years of age for supervised training in", trait_name, "\n")
  } else {
    supervised_sample <- eligible_older_individuals
    cat("Not enough individuals over 65 available in", trait_name, ". Only", nrow(supervised_sample), "available.\n")
  }
  
  unsupervised_data <- setdiff(dataset, supervised_sample)
  cat("Remaining data set aside for unsupervised training in", trait_name, "contains", nrow(unsupervised_data), "individuals.\n")
  
  write.csv(supervised_sample, paste0(trait_name, "_65_supervised_sample.csv"), row.names = FALSE)
  write.csv(unsupervised_data, paste0(trait_name, "_65_unsupervised_data.csv"), row.names = FALSE)
}


sample_and_split_data(eMERGEIII_pheno_covar_Height, "Height")
sample_and_split_data(eMERGEIII_pheno_covar_BMI, "BMI")
sample_and_split_data(eMERGEIII_pheno_covar_BrC, "Breast Cancer")
sample_and_split_data(eMERGEIII_pheno_covar_T2D, "Type 2 Diabetes")
