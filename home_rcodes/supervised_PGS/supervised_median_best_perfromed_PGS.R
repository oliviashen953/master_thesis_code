######################
# load Data set
#####################

BMI_african_sample  


pgs_columns <- grep("PGS", names(BMI_african_sample ), value = TRUE)


percentiles <- c(5, 10, 20, 30, 50, 75, 95)


results <- list()


for (pct in percentiles) {
 
  median_pheno <- numeric(length(pgs_columns))
  names(median_pheno) <- pgs_columns
  
  for(pgs in pgs_columns) {
   
    threshold <- quantile(BMI_african_sample [[pgs]], (100-pct)/100, na.rm = TRUE)
    

    top_pct_subjects <- BMI_african_sample [BMI_african_sample [[pgs]] > threshold, ]
    

    median_pheno[pgs] <- median(top_pct_subjects$pheno, na.rm = TRUE)
  }
  

  results[[paste(pct, "%", sep = "")]] <- median_pheno
}


for (pct in names(results)) {

  median_pheno <- results[[pct]]
  
  
  best_model <- which.max(median_pheno)
  best_model_name <- names(best_model)
  best_pheno_value <- median_pheno[best_model]
  
  cat("supervised 3 thousands Height: For the top", pct, ", the best-performing PGS model is:", best_model_name, 
      "with a median 'pheno' value of:", best_pheno_value, "\n\n")
}

