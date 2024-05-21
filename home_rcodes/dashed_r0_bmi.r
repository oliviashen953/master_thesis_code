library(readr)
library(dplyr)
library(ggplot2)
library(forcats)  

BMI_summary <- read_csv("BMI_summary_standardized_differences_and_rr_across_groups.csv")


ra_methods <- c("mc4_rank", "logs_rank", "RA_average_rank")
pgs_methods <- c("real_ols_rank_no", "PGS002313_rank", "pgs_average_rank")
all_methods <- c(ra_methods, pgs_methods)


filtered_methods <- BMI_summary %>%
  filter(rank_method %in% all_methods)


pgs_labels <- setNames(c(
  "MC4",
  "NNM",
  "AVG_RA_Rank",
  "Supervised_OLS",
  "Supervised_Best: PGS002313",
  "AVG_PRS"
), all_methods)


filtered_methods$rank_method <- factor(filtered_methods$rank_method, levels = all_methods, labels = pgs_labels)


filtered_methods$group_percentile <- fct_relevel(filtered_methods$group_percentile, "Top 5%", "Top 10%", "Top 20%")


method_colors <- c(
  "MC4" = "#E41A1C", # Red
  "NNM" = "#377EB8", # Blue
  "AVG_RA_Rank" = "#4DAF4A", # Green
  "Supervised_OLS" = "#FF7F00", # Orange
  "Supervised_Best: PGS002313" = "#984EA3", # Purple
  "AVG_PRS" = "#FFFF33" # Yellow
)


method_shapes <- setNames(c(19, 17, 15, 18, 16, 13), pgs_labels)


method_linetypes <- setNames(c("solid", "solid", "dashed", "solid", "solid", "dotted"), pgs_labels)


plot <- ggplot(filtered_methods, aes(x = group_percentile, y = mean_standardized_difference, group = rank_method,
                                     shape = rank_method, color = rank_method, linetype = rank_method)) +
  geom_point(size = 4) +  
  geom_line(size = 1.2) + 
  scale_color_manual(values = method_colors) +
  scale_shape_manual(values = method_shapes) +
  scale_linetype_manual(values = method_linetypes) +
  theme_bw(base_size = 16) +  
  labs(y = "Mean Standardized Difference (Standard Units)", x = "Group Percentile") +
  guides(color = guide_legend(override.aes = list(shape = c(19, 17, 15, 18, 16, 13))))


plot
ggsave("dashed_r0_BMI_OG_Top_Percentiles.png", plot = plot, width = 10, height = 8, dpi = 300)
