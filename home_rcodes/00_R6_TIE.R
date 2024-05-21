# eMERGEIII_pheno_covar_Height <- read.csv("~/new_emergeIII/eMERGEIII_pheno_covar_Height.txt", sep="")
# eMERGEIII_pheno_covar_BMI <- read.csv("~/new_emergeIII/eMERGEIII_pheno_covar_BMI.txt", sep="")
# eMERGEIII_pheno_covar_BrC <- read.csv("~/new_emergeIII/eMERGEIII_pheno_covar_BrC.txt", sep="")
# eMERGEIII_pheno_covar_T2D <- read.csv("~/new_emergeIII/eMERGEIII_pheno_covar_T2D.txt", sep="")



add_tiered_ranking <- function(file_path, num_groups) {
  for (i in 1:num_groups) {

    file_name <- paste0(file_path, "_group", i, ".csv")

    group_data <- read_csv(file_name)
    

    pgs_columns <- grep("PGS00[0-9]{4}", names(group_data), value = TRUE)
    

    for (pgs_col in pgs_columns) {

      group_data <- group_data %>%
        mutate(!!paste0(pgs_col, "_TIERANK") := rank(-get(pgs_col), ties.method = "average")) %>%

        mutate(!!paste0(pgs_col, "_TIERANK") := case_when(
          between(!!paste0(pgs_col, "_TIERANK"), 1, 150) ~ !!paste0(pgs_col, "_TIERANK"),
          between(!!paste0(pgs_col, "_TIERANK"), 151, 300) ~ 151,
          between(!!paste0(pgs_col, "_TIERANK"), 301, 450) ~ 152,
          between(!!paste0(pgs_col, "_TIERANK"), 451, 600) ~ 153,
          between(!!paste0(pgs_col, "_TIERANK"), 601, 750) ~ 154,
          between(!!paste0(pgs_col, "_TIERANK"), 751, 900) ~ 155,
          between(!!paste0(pgs_col, "_TIERANK"), 901, 1050) ~ 156,
          between(!!paste0(pgs_col, "_TIERANK"), 1051, 1200) ~ 157,
          between(!!paste0(pgs_col, "_TIERANK"), 1201, 1350) ~ 158,
          between(!!paste0(pgs_col, "_TIERANK"), 1351, 1500) ~ 159,
          between(!!paste0(pgs_col, "_TIERANK"), 1501, 1650) ~ 160,
          between(!!paste0(pgs_col, "_TIERANK"), 1651, 1800) ~ 161,
          between(!!paste0(pgs_col, "_TIERANK"), 1801, 1950) ~ 162,
          between(!!paste0(pgs_col, "_TIERANK"), 1951, 2100) ~ 163,
          between(!!paste0(pgs_col, "_TIERANK"), 2101, 2250) ~ 164,
          between(!!paste0(pgs_col, "_TIERANK"), 2251, 2400) ~ 165,
          between(!!paste0(pgs_col, "_TIERANK"), 2401, 2550) ~ 166,
          between(!!paste0(pgs_col, "_TIERANK"), 2551, 2700) ~ 167,
          between(!!paste0(pgs_col, "_TIERANK"), 2701, 2850) ~ 168,
          TRUE ~ 169
        ))
    }
    

    write.csv(group_data, paste0(file_path, "_group", i, "_TIERANK.csv"), row.names = FALSE)
  }
  
  return(invisible(NULL))  #
}




traits <- c("T2D", "BMI", "BrC", "Height")
file_prefix <- "TIE_R6_"


for (trait in traits) {
  file_path <- paste0(file_prefix, trait)
  add_tiered_ranking(file_path, 20)
}
