#################################
# B. Linear Combination: OLS 
#################################
library(readr)
T2D_EUR_sample <- read_csv("Type 2 Diabetes_EUR_supervised_sample.csv")
T2D_EUR_sample

#####################
#####################
#Step 1: Train the Linear Model
#####################
#####################

pgs_columns <- grep("PGS", names(T2D_EUR_sample), value = TRUE)
formula <- paste("pheno ~", paste(pgs_columns, collapse=" + "))
pgs_columns

model <- lm(formula, data = T2D_EUR_sample)


#####################
#####################
#Step 2: Create a New Score Using the Model's Coefficients
#####################
#####################

coefficients <- model$coefficients[-1]
coefficients

saveRDS(model$coefficients, "R5_EUR_Sample3000_new_supervised_T2D_model_coefficients.rds")
saveRDS(model, "R5_EUR_Sample3000_new_supervised_T2D_OLS_fitted_linear_model.rds")
