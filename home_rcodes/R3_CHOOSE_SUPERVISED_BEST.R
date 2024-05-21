####################################################
################ B. NEW OLS ########################
####################################################
library(readr)
Breast_Cancer_oldest_sample <- read_csv("Breast_Cancer_oldest_sample.csv")
View(Breast_Cancer_oldest_sample)
library(readr)
Breast_Cancer_oldest_sample <- read_csv("Breast_Cancer_oldest_sample.csv")
View(Breast_Cancer_oldest_sample)
library(readr)
Breast_Cancer_oldest_sample <- read_csv("Type 2 Diabetes_oldest_sample.csv")
View(Breast_Cancer_oldest_sample)
library(readr)
Breast_Cancer_oldest_sample <- read_csv("Breast Cancer_oldest_sample.csv")
View(Breast_Cancer_oldest_sample)



pgs_columns <- grep("PGS", names(Breast_Cancer_oldest_sample), value = TRUE) 

model_summaries <- data.frame(model = character(), r_squared = numeric(), stringsAsFactors = FALSE)

library(broom)

for(pgs in pgs_columns) {
  model <- lm(as.formula(paste("pheno ~", pgs)), data = Breast_Cancer_oldest_sample)
  summary_model <- glance(model)
  model_summaries <- rbind(model_summaries, data.frame(model = pgs, r_squared = summary_model$r.squared))
}


best_model <- model_summaries[which.max(model_summaries$r_squared), ]
best_model
################# B RESULTS: SAVE IN CSV// R ###########################


write.csv(model_summaries, "R3_Height_PGS_olsModel_summaries.csv", row.names = FALSE)


saveRDS(best_model, "R3_Height_new_supervised_BRC_best_PGS_model.rds")


write.csv(best_model, "R3_Height_new_supervised_BRC_best_PGS_model.csv", row.names = FALSE)


model_list <- list()


for(pgs in pgs_columns) {
  model <- lm(as.formula(paste("pheno ~", pgs)), data = Breast_Cancer_oldest_sample)
  model_list[[pgs]] <- model
}

saveRDS(model_list, "sampled3000_new_supervised_T2D_PGS_models_list.rds")
loaded_model_list <- readRDS("sampled3000_new_supervised_T2D_PGS_models_list.rds")

