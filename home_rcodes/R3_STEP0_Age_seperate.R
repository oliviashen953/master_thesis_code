


set.seed(123)  


sample_and_split_data <- function(dataset, trait_name) {
  oldest_sample <- NULL  
  
  if(nrow(dataset) >= 3000) {
    oldest_sample <- dataset %>%
      arrange(desc(age)) %>%
      slice_head(n = 3000)
    cat("Sampled 3000 oldest individuals for supervised training in", trait_name, "\n")
  } else {
    cat("Not enough individuals available in", trait_name, ". Only", nrow(dataset), "available.\n")
  }
  

  unsupervised_data <- setdiff(dataset, oldest_sample)
  cat("Remaining data set aside for unsupervised training in", trait_name, "contains", nrow(unsupervised_data), "individuals.\n")

  write.csv(oldest_sample, paste0(trait_name, "_oldest_sample.csv"), row.names = FALSE)
  write.csv(unsupervised_data, paste0(trait_name, "_NAoldest_unsupervised_data.csv"), row.names = FALSE)
}


sample_and_split_data(eMERGEIII_pheno_covar_Height, "Height")
sample_and_split_data(eMERGEIII_pheno_covar_BMI, "BMI")
sample_and_split_data(eMERGEIII_pheno_covar_BrC, "Breast Cancer")
sample_and_split_data(eMERGEIII_pheno_covar_T2D, "Type 2 Diabetes")
