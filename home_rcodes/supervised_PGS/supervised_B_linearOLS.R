#################################
# B. Linear Combination: OLS 
#################################
library(readr)
#Height_oldest_sample <- read_csv("new_emergeIII/Height_oldest_sample.csv")
View(Height_oldest_sample)


#####################
#####################
#Step 1: Train the Linear Model
#####################
#####################

pgs_columns <- grep("PGS", names(Height_oldest_sample), value = TRUE)
formula <- paste("pheno ~", paste(pgs_columns, collapse=" + "))

# linear model
model <- lm(formula, data = Height_oldest_sample)


#####################
#####################
#Step 2: Create a New Score Using the Model's Coefficients
#####################
#####################
coefficients <- model$coefficients[-1]
coefficients
##### SAVE COEFFICIENTS#######

saveRDS(model$coefficients, "R1_African_Sample3000_new_supervised_BrC_model_coefficients.rds")

saveRDS(model, "R1_African_Sample3000_new_supervised_BrC_OLS_fitted_linear_model.rds")





