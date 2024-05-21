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
Sorted_Summary_T2D_averages_pheno_by_percentile <- read_csv("SORTED_Summary_T2D_percentage_pheno_1s.csv")
View(Sorted_Summary_T2D_averages_pheno_by_percentile)

T2D_TOTOAL_results <- Sorted_Summary_T2D_averages_pheno_by_percentile


library(dplyr)


pgs_benchmarks <- c("supervise3000_ols_rank_no","pgs_average_rank", "PGS002308_rank")
ra_methods <- c("ams_rank","mc4_rank")


all_methods <- c(pgs_benchmarks, ra_methods)


filtered_methods <- T2D_TOTOAL_results %>%
  filter(rank_method %in% all_methods)


pgs_labels <- c(
  "supervise3000_ols_rank_no" = "supervised OLS 3000 training",
  "PGS002308_rank" = "supervised best 3000 training: PGS002308_rank",
  "ams_rank" = "Best RA method: Nuclear Norm ams_rank",
  "pgs_average_rank" = "Average PRS",
  "mc4_rank" = "Best RA method On Average: Markov Chain 4"
)


filtered_methods$rank_method <- as.character(filtered_methods$rank_method)
filtered_methods$rank_method <- ifelse(filtered_methods$rank_method %in% names(pgs_labels),
                                       pgs_labels[filtered_methods$rank_method],
                                       filtered_methods$rank_method)


filtered_methods$rank_method <- factor(filtered_methods$rank_method, levels = unique(filtered_methods$rank_method))



levels(filtered_methods$percentile)

filtered_methods$percentile <- fct_relevel(filtered_methods$percentile,
                                           "Top_5_Percent", "Top_10_Percent", "Top_20_Percent")

library(ggplot2)
library(dplyr)
library(forcats)  
top_percentiles <- filtered_methods %>%
  filter(percentile %in% c("Top_5_Percent", "Top_10_Percent", "Top_20_Percent"))

colors <- ggplot2::hue_pal()(length(unique(top_percentiles$rank_method)))
colors <- brewer.pal(length(unique(filtered_methods$rank_method)), "Set1")

number_of_colors <- length(unique(top_percentiles$rank_method))
colors <- colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))(number_of_colors)


y_min <- min(top_percentiles$mean_percentage_pheno_1s, na.rm = TRUE)
y_max <- max(top_percentiles$mean_percentage_pheno_1s, na.rm = TRUE)

padding <- (y_max - y_min) * 0.1
y_min <- y_min - padding
y_max <- y_max + padding


ggplot(top_percentiles, aes(x = percentile, y = mean_percentage_pheno_1s, group = rank_method,
                            shape = rank_method, color = rank_method)) +
  geom_point(size = 3) +
  geom_line() +
  scale_color_manual(values = colors) +
  theme_bw() +
  labs(y = "Average Percentage of Pheno == 1", x = "Top Catching Percentiles",
       title = "T2D: Comparison of Best Two RA Methods with PGS Models for Top percentiles") +
  ylim(y_min, y_max) 

ggsave("MC4_ADD_R1_T2D_Top_Percentiles.png", width = 10, height = 8, dpi = 300)
