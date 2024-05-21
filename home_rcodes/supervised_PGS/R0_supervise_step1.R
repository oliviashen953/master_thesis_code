# ####### IMPORT SUPERVISED DATASETS #######
# library(readr)
# supervised_Height_whole_new <- read_csv("new_emergeIII/supervised_Height_whole_new.csv")
# View(supervised_Height_whole_new)
# 
# library(readr)
# supervised_BMI_whole_new <- read_csv("new_emergeIII/supervised_BMI_whole_new.csv")
# View(supervised_BMI_whole_new)
# 
# library(readr)
# supervised_BrC_whole_new <- read_csv("new_emergeIII/supervised_BrC_whole_new.csv")
# View(supervised_BrC_whole_new)
# 
# library(readr)
# supervised_T2D_whole_new <- read_csv("new_emergeIII/supervised_T2D_whole_new.csv")
# View(supervised_T2D_whole_new)
library(readr)
sampled3000_supervised_Height_whole_new <- read_csv("sampled3000_supervised_Height_whole_new.csv")
sampled3000_supervised_BMI_whole_new <- read_csv("sampled3000_supervised_BMI_whole_new.csv")
sampled3000_supervised_BrC_whole_new <- read_csv("sampled3000_supervised_BrC_whole_new.csv")
sampled3000_supervised_T2D_whole_new <- read_csv("sampled3000_supervised_T2D_whole_new.csv")

####################################################
#####################################################
######################################################
#####################################################
############## HEIGHT SUPERVISED 20% ################
####################################################
#####################################################
######################################################
#####################################################
####################################################


####################################################
#####################################################
############ BEST PERFORMED PGS #####################
#####################################################
####################################################
library(dplyr)
library(broom)

####################################################
############### A. OLD SLECTED #####################
####################################################

####################################################
################ B. NEW OLS ########################
####################################################
# B. Simple Linear Regression for Each PGS Model
pgs_columns <- grep("PGS", names(supervised_Height_whole_new), value = TRUE) 

model_summaries <- data.frame(model = character(), r_squared = numeric(), stringsAsFactors = FALSE)

for(pgs in pgs_columns) {
  model <- lm(as.formula(paste("pheno ~", pgs)), data = supervised_Height_whole_new)
  summary_model <- glance(model)
  model_summaries <- rbind(model_summaries, data.frame(model = pgs, r_squared = summary_model$r.squared))
}

best_model <- model_summaries[which.max(model_summaries$r_squared), ]

################# B RESULTS: SAVE IN CSV// R ###########################


write.csv(model_summaries, "new_supervised_Height_PGS_olsModel_summaries.csv", row.names = FALSE)


saveRDS(best_model, "new_supervised_Height_best_PGS_model.rds")


write.csv(best_model, "new_supervised_Height_best_PGS_model.csv", row.names = FALSE)


model_list <- list()

for(pgs in pgs_columns) {
  model <- lm(as.formula(paste("pheno ~", pgs)), data = supervised_Height_whole_new)
  model_list[[pgs]] <- model
}


saveRDS(model_list, "new_supervised_Height_PGS_models_list.rds")
loaded_model_list <- readRDS("new_supervised_Height_PGS_models_list.rds")


####################################################
########## C. NEW SIMPLE AVG #######################
####################################################
supervised_Height_whole_new$avg_PGS <- rowMeans(supervised_Height_whole_new[,pgs_columns])

####################################################
############ D. NEW SIMPLE MEDIAN ##################
####################################################

supervised_Height_whole_new$median_PGS <- apply(supervised_Height_whole_new[,pgs_columns], 1, median)
