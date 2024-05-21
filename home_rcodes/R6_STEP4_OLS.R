#################################
# B. Linear Combination: OLS 
#################################
library(readr)
BMI_TIE_sample <- read_csv("BMI_TIE_supervised_sample.csv")
BMI_TIE_sample

#####################
#####################
#Step 1: Train the Linear Model
#####################
#####################

pgs_columns <- grep("PGS", names(BMI_TIE_sample), value = TRUE)
formula <- paste("pheno ~", paste(pgs_columns, collapse=" + "))
pgs_columns
# train linear model
model <- lm(formula, data = BMI_TIE_sample)


#####################
#####################
#Step 2: Create a New Score Using the Model's Coefficients
#####################
#####################


coefficients <- model$coefficients[-1]
coefficients

##### SAVE COEFFICIENTS#######
saveRDS(model$coefficients, "R6_TIE_Sample3000_new_supervised_BMI_model_coefficients.rds")
saveRDS(model, "R6_TIE_Sample3000_new_supervised_BMI_OLS_fitted_linear_model.rds")
