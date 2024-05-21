####################################################
################ B. NEW OLS ########################
####################################################



T2D_EUR_supervised_sample <- read_csv("Type 2 Diabetes_EUR_supervised_sample.csv")
pgs_columns <- grep("PGS", names(T2D_EUR_supervised_sample), value = TRUE) 

pgs_columns

model_summaries <- data.frame(model = character(), r_squared = numeric(), stringsAsFactors = FALSE)

library(broom)

for(pgs in pgs_columns) {
  model <- lm(as.formula(paste("pheno ~", pgs)), data = T2D_EUR_supervised_sample)
  summary_model <- glance(model)
  model_summaries <- rbind(model_summaries, data.frame(model = pgs, r_squared = summary_model$r.squared))
}


best_model <- model_summaries[which.max(model_summaries$r_squared), ]
best_model
