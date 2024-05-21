######################
# load Data set
#####################


BMI_EUR_sample 



pgs_columns <- grep("PGS", names(BMI_EUR_sample), value = TRUE)
pgs_columns

percentiles <- c(5, 10, 20, 30, 50, 75, 95)

results <- list()


for (pct in percentiles) {

  avg_pheno <- numeric(length(pgs_columns))
  names(avg_pheno) <- pgs_columns
  

  for(pgs in pgs_columns) {

    threshold <- quantile(BMI_EUR_sample[[pgs]], (100-pct)/100, na.rm = TRUE)

    top_pct_subjects <- BMI_EUR_sample[BMI_EUR_sample[[pgs]] > threshold, ]
    

    avg_pheno[pgs] <- mean(top_pct_subjects$pheno, na.rm = TRUE)
  }
  

  results[[paste(pct, "%", sep = "")]] <- avg_pheno
}


for (pct in names(results)) {
  cat("Average 'pheno' values for the top", pct, "of each PGS model:\n")
  print(results[[pct]])
  cat("\n")
}

for (pct in names(results)) {

  avg_pheno <- results[[pct]]
  

  best_model <- which.max(avg_pheno)
  best_model_name <- names(best_model)
  best_pheno_value <- avg_pheno[best_model]
  

  cat("supervised 20% BMI: For the top", pct, ", the best-performing PGS model is:", best_model_name, 
      "with an average 'pheno' value of:", best_pheno_value, "\n\n")
}

