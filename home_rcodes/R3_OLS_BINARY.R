library(readr)
library(dplyr)


Breast_Cancer_oldest_sample <- read_csv("Breast_Cancer_oldest_sample.csv")


pgs_columns <- grep("^PGS", names(Breast_Cancer_oldest_sample), value = TRUE)


Breast_Cancer_oldest_sample$average_PGS <- rowMeans(Breast_Cancer_oldest_sample[pgs_columns], na.rm = TRUE)


head(Breast_Cancer_oldest_sample)



formula <- as.formula(paste("pheno ~", paste(pgs_columns, collapse = " + ")))


ols_model <- lm(formula, data = Breast_Cancer_oldest_sample)


summary(ols_model)


coefficients <- coef(ols_model)


saveRDS(ols_model$coefficients, "R3_AGE_Sample3000_new_supervised_BrC_model_coefficients.rds")

saveRDS(ols_model, "R3_AGE_Sample3000_new_supervised_BrC_OLS_fitted_linear_model.rds")

