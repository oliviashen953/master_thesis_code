###########################
###########################
# js_thesis -> graph_true.R

# RDS file: all result datasets
# SUPERVISED3000_new_percentile_result_list.rds
###########################
###########################
readRDS("SUPERVISED3000_new_percentile_result_list.rds")
new_percentile_result_list <- readRDS("SUPERVISED3000_new_percentile_result_list.rds")
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(forcats)

T2D_group10_results <- new_percentile_result_list[["new_T2D_group10_percentiles_COMBINED_INCLUDED.csv"]]

library(dplyr)

pgs_benchmarks <- c("supervise3000_ols_rank_no", "pgs_avg_rank", "pgs_median_rank", "PGS002308_rank", "PGS000330_rank")
ra_methods <- c("ams_rank", "bcs_rank", "sbs_rank", "logs_rank", "mu_rank", "borda_rank", "mct_rank", "mc4_rank", "combined_average_rank", "combined_median_rank")


all_methods <- c(pgs_benchmarks, ra_methods)


filtered_methods <- T2D_group10_results %>%
  filter(rank_method %in% all_methods)


pgs_labels <- c(
  "supervise3000_ols_rank_no" = "supervised3000 OLS generated: supervise3000_ols",
  "PGS002308_rank" = "supervised3000 R^2 Highest: PGS002308_rank",
  "PGS000330_rank" = "supervised3000 AVG Highest: PGS000330_rank"
)


filtered_methods$rank_method <- as.character(filtered_methods$rank_method)
filtered_methods$rank_method <- ifelse(filtered_methods$rank_method %in% names(pgs_labels),
                                       pgs_labels[filtered_methods$rank_method],
                                       filtered_methods$rank_method)


filtered_methods$rank_method <- factor(filtered_methods$rank_method, levels = unique(filtered_methods$rank_method))



levels(filtered_methods$percentile)

filtered_methods$percentile <- fct_relevel(filtered_methods$percentile,
                                           "Top_5_Percent", "Top_10_Percent", "Top_20_Percent",
                                           "Top_30_Percent", "Top_50_Percent", "Top_75_Percent", "Top_95_Percent")
###################################
###################################
###################################
######## Top 5/10/20/30#############
###################################
###################################
library(ggplot2)
library(dplyr)
library(forcats)

Top_percentiles <- filtered_methods %>%
  filter(percentile %in% c("Top_5_Percent", "Top_10_Percent", "Top_20_Percent", "Top_30_Percent"))


colors <- ggplot2::hue_pal()(length(unique(Top_percentiles$rank_method)))
colors <- brewer.pal(length(unique(filtered_methods$rank_method)), "Set1")

number_of_colors <- length(unique(Top_percentiles$rank_method))
colors <- colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))(number_of_colors)


y_min <- min(Top_percentiles$percentage_pheno_1s, na.rm = TRUE)
y_max <- max(Top_percentiles$percentage_pheno_1s, na.rm = TRUE)


padding <- (y_max - y_min) * 0.1
y_min <- y_min - padding
y_max <- y_max + padding


ggplot(Top_percentiles, aes(x = percentile, y = percentage_pheno_1s, group = rank_method,
                            shape = rank_method, color = rank_method)) +
  geom_point(size = 3) +
  geom_line() +
  scale_color_manual(values = colors) +
  theme_bw() +
  labs(y = "percentile", x = "percentile",
       title = "T2D Group 1:Comparison of RA Methods for Top percentiles") +
  ylim(y_min, y_max) # Apply the focused y-axis limits


ggsave("Supervised3000_test_new_T2D_group10_Top_percentiles.png", width = 10, height  = 8, dpi = 300)





library(ggplot2)
library(dplyr)
library(forcats)


number_of_methods <- length(unique(filtered_methods$rank_method))
custom_colors <- colorRampPalette(brewer.pal(min(9, number_of_methods), "Set1"))(number_of_methods)


custom_shapes <- c(16, 17, 18, 15, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

while(length(custom_shapes) < number_of_methods){
  custom_shapes <- c(custom_shapes, 21:25)
}


names(custom_colors) <- levels(filtered_methods$rank_method)
names(custom_shapes) <- levels(filtered_methods$rank_method)

p <- ggplot(filtered_methods, aes(x = percentile, y = percentage_pheno_1s, group = rank_method)) +
  geom_line(aes(color = rank_method)) +
  geom_point(aes(shape = rank_method, color = rank_method), size = 3) +
  scale_shape_manual(values = custom_shapes) +
  scale_color_manual(values = custom_colors) +
  theme_minimal() +
  theme(legend.position = "right") +
  labs(title = "Supervised3000_T2D group10: Comparison of RA Methods for all Percentiles",
       x = "Percentile",
       y = "Pheno == 1's Percentage",
       color = "Rank Method",
       shape = "Rank Method")


print(p)


ggsave("Supervised3000_new_T2D_group10_ALL_percentiles.png", plot = p, width = 12, height = 8, dpi = 300)



######## 优化 : top 5--> top 20 ######

top_percentiles <- filtered_methods %>%
  filter(percentile %in% c("Top_5_Percent", "Top_10_Percent", "Top_20_Percent"))
library(ggplot2)


number_of_methods <- length(unique(top_percentiles$rank_method))
custom_colors <- colorRampPalette(brewer.pal(min(9, number_of_methods), "Set1"))(number_of_methods)

custom_shapes <- c(16, 17, 18, 15, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

while(length(custom_shapes) < number_of_methods){
  custom_shapes <- c(custom_shapes, 21:25)
}


names(custom_colors) <- levels(top_percentiles$rank_method)
names(custom_shapes) <- levels(top_percentiles$rank_method)


p <- ggplot(top_percentiles, aes(x = percentile, y = percentage_pheno_1s, group = rank_method)) +
  geom_line(aes(color = rank_method)) +
  geom_point(aes(shape = rank_method, color = rank_method), size = 3) +
  scale_shape_manual(values = custom_shapes) +
  scale_color_manual(values = custom_colors) +
  theme_minimal() +
  theme(legend.position = "right") +
  labs(title = "Supervised3000_T2D group10: Comparison of RA Methods for Top Percentiles",
       x = "Percentile",
       y = "Pheno == 1's Percentage",
       color = "Rank Method",
       shape = "Rank Method")


print(p)

ggsave("Supervised3000_new_T2D_group10_mu_included_TOP_percentiles.png", plot = p, width = 12, height = 8, dpi = 300)















######## 优化 EXCLUDE BAD PERFOMED RA & TOP 5-20 PERCENT ######
readRDS("SUPERVISED3000_new_percentile_result_list.rds")
new_percentile_result_list <- readRDS("SUPERVISED3000_new_percentile_result_list.rds")
library(ggplot2)
library(dplyr)
library(RColorBrewer)

library(forcats)

T2D_group10_results <- new_percentile_result_list[["new_T2D_group10_percentiles_COMBINED_INCLUDED.csv"]]


library(dplyr)


pgs_benchmarks <- c("supervise3000_ols_rank_no", "pgs_avg_rank", "pgs_median_rank", "PGS002308_rank", "PGS000330_rank")
ra_methods <- c("bcs_rank", "sbs_rank", "logs_rank", "borda_rank", "mct_rank", "mc4_rank", "combined_average_rank", "combined_median_rank")


all_methods <- c(pgs_benchmarks, ra_methods)


filtered_methods <- T2D_group10_results %>%
  filter(rank_method %in% all_methods)


pgs_labels <- c(
  "supervise3000_ols_rank_no" = "supervised3000 OLS generated: supervise3000_ols",
  "PGS002308_rank" = "supervised3000 R^2 Highest: PGS002308_rank",
  "PGS000330_rank" = "supervised3000 AVG Highest: PGS000330_rank"
)


filtered_methods$rank_method <- as.character(filtered_methods$rank_method)
filtered_methods$rank_method <- ifelse(filtered_methods$rank_method %in% names(pgs_labels),
                                       pgs_labels[filtered_methods$rank_method],
                                       filtered_methods$rank_method)

filtered_methods$percentile <- fct_relevel(filtered_methods$percentile,
                                           "Top_5_Percent", "Top_10_Percent", "Top_20_Percent")


top_percentiles <- filtered_methods %>%
  filter(percentile %in% c("Top_5_Percent", "Top_10_Percent", "Top_20_Percent"))

number_of_methods <- length(unique(top_percentiles$rank_method))


custom_colors <- colorRampPalette(brewer.pal(min(9, number_of_methods), "Set1"))(number_of_methods)
custom_shapes <- c(16, 17, 18, 15, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
while(length(custom_shapes) < number_of_methods){
  custom_shapes <- c(custom_shapes, 21:25)
}

names(custom_colors) <- levels(top_percentiles$rank_method)
names(custom_shapes) <- levels(top_percentiles$rank_method)


p <- ggplot(top_percentiles, aes(x = percentile, y = percentage_pheno_1s, group = rank_method)) +
  geom_line(aes(color = rank_method)) +
  geom_point(aes(shape = rank_method, color = rank_method), size = 3) +
  scale_shape_manual(values = custom_shapes) +
  scale_color_manual(values = custom_colors) +
  theme_minimal() +
  theme(legend.position = "right") +
  labs(title = "Supervised3000_T2D group10: Comparison of Top Performed RA Methods for Top Percentiles",
       x = "Percentile",
       y = "Pheno == 1's Percentage",
       color = "Rank Method",
       shape = "Rank Method")

print(p)
ggsave("Supervised3000_new_T2D_group10_BEST_TOP_percentiles.png", plot = p, width = 12, height = 8, dpi = 300)


