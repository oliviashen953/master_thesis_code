library(readr)
library(dplyr)

# Load the dataset
data <- read_csv("SORTED_Summary_Height_averages_pheno_by_percentile.csv")


average_performance <- data %>%
  group_by(rank_method) %>%
  summarise(avg_mean_average_pheno = mean(mean_average_pheno, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(avg_mean_average_pheno)) 

print(average_performance)


write.csv(average_performance, "R1R2_Height_Average_Performance_by_Rank_Method.csv", row.names = FALSE)


#######################################################################
library(readr)
library(dplyr)


data <- read_csv("SORTED_Summary_BMI_averages_pheno_by_percentile.csv")

average_performance <- data %>%
  group_by(rank_method) %>%
  summarise(avg_mean_average_pheno = mean(mean_average_pheno, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(avg_mean_average_pheno)) 

print(average_performance)


write.csv(average_performance, "R1R2_BMI_Average_Performance_by_Rank_Method.csv", row.names = FALSE)


#######################################################################
library(readr)
library(dplyr)


data <- read_csv("SORTED_Summary_BrC_percentage_pheno_1s.csv")

average_performance <- data %>%
  group_by(rank_method) %>%
  summarise(avg_mean_average_pheno = mean(mean_percentage_pheno_1s, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(avg_mean_average_pheno)) 


print(average_performance)


write.csv(average_performance, "R1R2_BrC_Average_Performance_by_Rank_Method.csv", row.names = FALSE)


#######################################################################
library(readr)
library(dplyr)


data <- read_csv("SORTED_Summary_T2D_percentage_pheno_1s.csv")


average_performance <- data %>%
  group_by(rank_method) %>%
  summarise(avg_mean_average_pheno = mean(mean_percentage_pheno_1s, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(avg_mean_average_pheno)) 


print(average_performance)


write.csv(average_performance, "R1R2_T2D_Average_Performance_by_Rank_Method.csv", row.names = FALSE)
