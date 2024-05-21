
updated_eMERGEIII_pheno_covar_T2D <- read.csv("~/new_emergeIII/newEMERGE_WithRace/updated_eMERGEIII_pheno_covar_T2D.txt")
updated_eMERGEIII_pheno_covar_BrC <- read.csv("~/new_emergeIII/newEMERGE_WithRace/updated_eMERGEIII_pheno_covar_BrC.txt")
updated_eMERGEIII_pheno_covar_BMI <- read.csv("~/new_emergeIII/newEMERGE_WithRace/updated_eMERGEIII_pheno_covar_BMI.txt")
updated_eMERGEIII_pheno_covar_Height <- read.csv("~/new_emergeIII/newEMERGE_WithRace/updated_eMERGEIII_pheno_covar_Height.txt")




race_counts <- table(updated_eMERGEIII_pheno_covar_T2D$race)

print(race_counts)


race_counts <- table(updated_eMERGEIII_pheno_covar_BrC$race)

print(race_counts)

race_counts <- table(updated_eMERGEIII_pheno_covar_BMI$race)


print(race_counts)

race_counts <- table(updated_eMERGEIII_pheno_covar_Height$race)


print(race_counts)
