library(dplyr)


race_counts <- Height_random_sample %>%
  group_by(race) %>%
  summarise(count = n())


print(race_counts)


library(readr)
BMI_random_sample <- read_csv("R1_original_super_unsuper_datasets/BMI_random_sample.csv")
View(BMI_random_sample)

race_counts <- BMI_random_sample %>%
  group_by(race) %>%
  summarise(count = n())

print(race_counts)


library(readr)
Breast_Cancer_random_sample <- read_csv("R1_original_super_unsuper_datasets/Breast Cancer_random_sample.csv")
View(Breast_Cancer_random_sample)

race_counts <- Breast_Cancer_random_sample %>%
  group_by(race) %>%
  summarise(count = n())

print(race_counts)
