#################################
# B. Linear Combination: OLS 
#################################
library(readr)
#Type_2_Diabetes_oldest_sample <- read_csv("new_emergeIII/Type_2_Diabetes_oldest_sample.csv")
Type_2_Diabetes_oldest_sample


#####################
#####################
#Step 1: Train the Linear Model
#####################
#####################

pgs_columns <- grep("PGS", names(Type_2_Diabetes_oldest_sample), value = TRUE)
formula <- paste("pheno ~", paste(pgs_columns, collapse=" + "))

# Train the linear model
model <- lm(formula, data = Type_2_Diabetes_oldest_sample)


#####################
#####################
#Step 2: Create a New Score Using the Model's Coefficients
#####################
#####################


coefficients <- model$coefficients[-1]
coefficients

##### SAVE COEFFICIENTS#######

saveRDS(model$coefficients, "R3_AGE_Sample3000_new_supervised_T2D_model_coefficients.rds")

saveRDS(model, "R3_AGE_Sample3000_new_supervised_T2D_OLS_fitted_linear_model.rds")





