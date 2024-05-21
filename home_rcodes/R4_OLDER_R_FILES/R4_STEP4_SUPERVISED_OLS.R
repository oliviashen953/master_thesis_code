#################################
# B. Linear Combination: OLS 
#################################
library(readr)
Height_65_sample <- read_csv("Height_65_supervised_sample.csv")
Height_65_sample

#####################
#####################
#Step 1: Train the Linear Model
#####################
#####################


pgs_columns <- grep("PGS", names(Height_65_sample), value = TRUE)
formula <- paste("pheno ~", paste(pgs_columns, collapse=" + "))

model <- lm(formula, data = Height_65_sample)


#####################
#####################
#Step 2: Create a New Score Using the Model's Coefficients
#####################
#####################

# Exclude the intercept from the coefficients
coefficients <- model$coefficients[-1]
coefficients
##### SAVE COEFFICIENTS#######

saveRDS(model$coefficients, "R4_AGE_Sample3000_new_supervised_Height_model_coefficients.rds")

saveRDS(model, "R4_AGE_Sample3000_new_supervised_Height_OLS_fitted_linear_model.rds")
