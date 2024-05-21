#####################################################################################################################
#####################################################################################################################
# For binary phenotypes like Breast Cancer /T2D
#####################################################################################################################

pgs_columns <- grep("PGS", names(T2D_65_supervised_sample), value = TRUE)


percentiles <- c(5, 10, 20, 30, 50, 75, 95)


results <- list()

for (pct in percentiles) {
  
  proportion_pheno <- numeric(length(pgs_columns))
  names(proportion_pheno) <- pgs_columns
 
  for(pgs in pgs_columns) {

    threshold <- quantile(T2D_65_supervised_sample [[pgs]], (100-pct)/100, na.rm = TRUE)
    
   
    top_pct_subjects <- T2D_65_supervised_sample [T2D_65_supervised_sample [[pgs]] > threshold, ]
    

    proportion_pheno[pgs] <- mean(top_pct_subjects$pheno, na.rm = TRUE)
  }
  

  results[[paste(pct, "%", sep = "")]] <- proportion_pheno
}


for (pct in names(results)) {
  write.csv(results[[pct]], paste0("proportion_pheno_top_", pct, "_PGS_models.csv"), row.names = TRUE)
}


for (pct in names(results)) {

  best_model_index <- which.max(results[[pct]])
  best_model_name <- names(best_model_index)
  best_proportion_value <- results[[pct]][best_model_name]
  
  cat("t2d SUPERVISED TWENTY: For the top", pct, ", the best-performing PGS model is:", best_model_name, 
      "with a proportion of 'pheno = 1' of:", best_proportion_value, "\n\n")
}
