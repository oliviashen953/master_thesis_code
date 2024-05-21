


library(readr)

Breast_Cancer_oldest_sample <- read_csv("Breast_Cancer_oldest_sample.csv")
Breast_Cancer_oldest_sample


Breast_Cancer_oldest_sample$pheno <- factor(Breast_Cancer_oldest_sample$pheno, levels = c(0, 1))


pgs_columns <- grep("^PGS", names(Breast_Cancer_oldest_sample), value = TRUE)
formula <- as.formula(paste("pheno ~", paste(pgs_columns, collapse=" + ")))


model <- glm(formula, data = Breast_Cancer_oldest_sample, family = binomial)


summary(model)


