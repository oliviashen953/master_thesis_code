# ###########################
# ###########################
# # js_thesis -> graph_true.R
# 
# # RDS file: all result datasets
# # median_updated_percentile_result_list.rds
# ###########################
# ###########################
# readRDS("median_updated_percentile_result_list.rds")
# new_percentile_result_list <- readRDS("median_updated_percentile_result_list.rds")


library(ggplot2)
library(dplyr)
library(RColorBrewer)

library(forcats)

library(readr)
Sorted_Summary_BMI_averages_pheno_by_percentile <- read_csv("SORTED_Summary_BMI_averages_pheno_by_percentile.csv")
View(Sorted_Summary_BMI_averages_pheno_by_percentile)

BMI_TOTOAL_results <- Sorted_Summary_BMI_averages_pheno_by_percentile


library(dplyr)


pgs_benchmarks <- c("real_african_ols","pgs_average", "PGS000921")
ra_methods <- c("mc4")


all_methods <- c(pgs_benchmarks, ra_methods)


filtered_methods <- BMI_TOTOAL_results %>%
  filter(rank_method %in% all_methods)


pgs_labels <- c(
  "real_african_ols" = "supervised OLS 3000 training (supervise AFR3000)",
  "PGS000921" = "supervised best 3000 training: PGS000921 (supervise AFR3000)",
  "mc4" = "Best RA method: Markov Chains 4",
  "pgs_average" = "Average PRS"
)


filtered_methods$rank_method <- as.character(filtered_methods$rank_method)
filtered_methods$rank_method <- ifelse(filtered_methods$rank_method %in% names(pgs_labels),
                                       pgs_labels[filtered_methods$rank_method],
                                       filtered_methods$rank_method)


filtered_methods$rank_method <- factor(filtered_methods$rank_method, levels = unique(filtered_methods$rank_method))


levels(filtered_methods$percentile)
filtered_methods$percentile <- fct_relevel(filtered_methods$percentile,
                                           "top_5_percent", "top_10_percent", "top_20_percent")

library(ggplot2)
library(dplyr)
library(forcats) 


top_percentiles <- filtered_methods %>%
  filter(percentile %in% c("top_5_percent", "top_10_percent", "top_20_percent"))


colors <- ggplot2::hue_pal()(length(unique(top_percentiles$rank_method)))
colors <- brewer.pal(length(unique(filtered_methods$rank_method)), "Set1")

number_of_colors <- length(unique(top_percentiles$rank_method))
colors <- colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))(number_of_colors)


y_min <- min(top_percentiles$mean_average_pheno, na.rm = TRUE)
y_max <- max(top_percentiles$mean_average_pheno, na.rm = TRUE)


padding <- (y_max - y_min) * 0.1
y_min <- y_min - padding
y_max <- y_max + padding


ggplot(top_percentiles, aes(x = percentile, y = mean_average_pheno, group = rank_method,
                            shape = rank_method, color = rank_method)) +
  geom_point(size = 3) +
  geom_line() +
  scale_color_manual(values = colors) +
  theme_bw() +
  labs(y = "Average BMI (in cm)", x = "Top Catching Percentiles",
       title = "BMI: Comparison of Best RA Method with PGS Models for Top percentiles (supervise AFR3000)") +
  ylim(y_min, y_max) 


ggsave("R1_AFR_BMI_Top_Percentiles.png", width = 10, height = 8, dpi = 300)
