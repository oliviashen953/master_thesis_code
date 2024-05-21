######################
# load Data set
#####################

BMI_african_sample 

#####################################################################################################################
#####################################################################################################################


pgs_columns <- grep("PGS", names(BMI_african_sample), value = TRUE)


percentiles <- c(5, 10, 20, 30, 50, 75, 95)

results <- list()


for (pct in percentiles) {

  avg_pheno <- numeric(length(pgs_columns))
  names(avg_pheno) <- pgs_columns
  

  for(pgs in pgs_columns) {

    threshold <- quantile(BMI_african_sample[[pgs]], (100-pct)/100, na.rm = TRUE)
    

    top_pct_subjects <- BMI_african_sample[BMI_african_sample[[pgs]] > threshold, ]
    

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















##################################################################
# Step 1: For Each PGS Model, Select Its Top 5% Highest Values
#################################################################

pgs_columns <- grep("PGS", names(BMI_african_sample), value = TRUE)


avg_pheno_for_top_5_percent <- numeric(length(pgs_columns))
names(avg_pheno_for_top_5_percent) <- pgs_columns


for(pgs in pgs_columns) {

  threshold <- quantile(BMI_african_sample[[pgs]], 0.95, na.rm = TRUE)
  
  top_5_percent_subjects <- BMI_african_sample[BMI_african_sample[[pgs]] > threshold, ]
  

  avg_pheno_for_top_5_percent[pgs] <- mean(top_5_percent_subjects$pheno, na.rm = TRUE)
}


##################################################################
# Step 2: Identify the Best-Performing PGS Model
#################################################################
best_model_index <- which.max(avg_pheno_for_top_5_percent)
best_model_name <- names(best_model_index)
best_model_avg_pheno <- avg_pheno_for_top_5_percent[best_model_name]
avg_pheno_for_top_5_percent

cat("Best-performing PGS model:", best_model_name, "\nAverage 'pheno' value for its top 5%:", best_model_avg_pheno, "\n")










