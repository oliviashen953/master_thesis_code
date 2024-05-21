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

BMI_group8_results <- new_percentile_result_list[["supervised3000_new_BMI_group8_averages_pheno_by_percentile.csv"]]
BMI_group8_results

library(dplyr)

pgs_benchmarks <- c("supervise3000_ols", "pgs_avg", "pgs_median", "PGS002313", "PGS002161")
ra_methods <- c("ams", "bcs", "sbs", "logs", "mu", "borda", "mct", "mc4", "combined_average", "combined_median")


all_methods <- c(pgs_benchmarks, ra_methods)


filtered_methods <- BMI_group8_results %>%
  filter(rank_method %in% all_methods)


pgs_labels <- c(
  "supervise3000_ols" = "supervised3000 OLS generated: supervise3000_ols",
  "PGS002161" = "supervised Best R^2/Avg: PGS002161",
  "PGS002313" = "supervised Best Median: PGS002313"
)


filtered_methods$rank_method <- as.character(filtered_methods$rank_method)
filtered_methods$rank_method <- ifelse(filtered_methods$rank_method %in% names(pgs_labels),
                                       pgs_labels[filtered_methods$rank_method],
                                       filtered_methods$rank_method)


filtered_methods$rank_method <- factor(filtered_methods$rank_method, levels = unique(filtered_methods$rank_method))



levels(filtered_methods$percentile)

filtered_methods$percentile <- fct_relevel(filtered_methods$percentile,
                                           "top_5 percent", "top_10 percent", "top_20 percent",
                                           "top_30 percent", "top_50 percent", "top_75 percent", "top_95 percent")

###################################
###################################
###################################
######## Top 5/10/20/30#############
###################################
###################################
library(ggplot2)
library(dplyr)
library(forcats)  

top_percentiles <- filtered_methods %>%
  filter(percentile %in% c("top_5 percent", "top_10 percent", "top_20 percent", "top_30 percent"))


colors <- ggplot2::hue_pal()(length(unique(top_percentiles$rank_method)))
colors <- brewer.pal(length(unique(filtered_methods$rank_method)), "Set1")

number_of_colors <- length(unique(top_percentiles$rank_method))
colors <- colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))(number_of_colors)


y_min <- min(top_percentiles$average_pheno, na.rm = TRUE)
y_max <- max(top_percentiles$average_pheno, na.rm = TRUE)


padding <- (y_max - y_min) * 0.1
y_min <- y_min - padding
y_max <- y_max + padding


ggplot(top_percentiles, aes(x = percentile, y = average_pheno, group = rank_method,
                            shape = rank_method, color = rank_method)) +
  geom_point(size = 3) +
  geom_line() +
  scale_color_manual(values = colors) +
  theme_bw() +
  labs(y = "percentile", x = "percentile",
       title = "BMI Group 1:Comparison of RA Methods for Top percentiles") +
  ylim(y_min, y_max) 


ggsave("supervised3000_test_new_BMI_group8_top_percentiles.png", width = 10, height = 8, dpi = 300)





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


p <- ggplot(filtered_methods, aes(x = percentile, y = average_pheno, group = rank_method)) +
  geom_line(aes(color = rank_method)) +
  geom_point(aes(shape = rank_method, color = rank_method), size = 3) +
  scale_shape_manual(values = custom_shapes) +
  scale_color_manual(values = custom_colors) +
  theme_minimal() +
  theme(legend.position = "right") +
  labs(title = "Supervised3000_BMI group8: Comparison of RA Methods for all Percentiles",
       x = "Percentile",
       y = "Average BMI",
       color = "Rank Method",
       shape = "Rank Method")


print(p)


ggsave("supervised3000_new_BMI_group8_ALL_percentiles.png", plot = p, width = 12, height = 8, dpi = 300)


######## 优化 : top 5--> top 20 ######
top_percentiles <- filtered_methods %>%
  filter(percentile %in% c("top_5 percent", "top_10 percent", "top_20 percent"))
library(ggplot2)


number_of_methods <- length(unique(top_percentiles$rank_method))
custom_colors <- colorRampPalette(brewer.pal(min(9, number_of_methods), "Set1"))(number_of_methods)


custom_shapes <- c(16, 17, 18, 15, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
while(length(custom_shapes) < number_of_methods){
  custom_shapes <- c(custom_shapes, 21:25)
}


names(custom_colors) <- levels(top_percentiles$rank_method)
names(custom_shapes) <- levels(top_percentiles$rank_method)

p <- ggplot(top_percentiles, aes(x = percentile, y = average_pheno, group = rank_method)) +
  geom_line(aes(color = rank_method)) +
  geom_point(aes(shape = rank_method, color = rank_method), size = 3) +
  scale_shape_manual(values = custom_shapes) +
  scale_color_manual(values = custom_colors) +
  theme_minimal() +
  theme(legend.position = "right") +
  labs(title = "Supervised3000_BMI group8: Bad RA included: Comparison of RA Methods for Top Percentiles",
       x = "Percentile",
       y = "Average BMI",
       color = "Rank Method",
       shape = "Rank Method")


print(p)

ggsave("Supervised3000_new_BMI_group8_mu_included_TOP_percentiles.png", plot = p, width = 12, height = 8, dpi = 300)











######## 优化 EXCLUDE BAD PERFOMED RA & TOP 5-20 PERCENT ######

readRDS("SUPERVISED3000_new_percentile_result_list.rds")
new_percentile_result_list <- readRDS("SUPERVISED3000_new_percentile_result_list.rds")
library(ggplot2)
library(dplyr)
library(RColorBrewer)

library(forcats)

BMI_group8_results <- new_percentile_result_list[["supervised3000_new_BMI_group8_averages_pheno_by_percentile.csv"]]
BMI_group8_results

library(dplyr)


pgs_benchmarks <- c("supervise3000_ols", "pgs_median", "PGS002313", "PGS002161")
ra_methods <- c("bcs", "sbs", "logs", "borda", "mct", "mc4", "combined_median")


all_methods <- c(pgs_benchmarks, ra_methods)

filtered_methods <- BMI_group8_results %>%
  filter(rank_method %in% all_methods)



pgs_labels <- c(
  "supervise3000_ols" = "supervised3000 OLS generated: supervise3000_ols",
  "PGS002161" = "supervised Best R^2/Avg: PGS000910",
  "PGS002313" = "supervised Best Median: PGS002161"
)



filtered_methods$rank_method <- as.character(filtered_methods$rank_method)
filtered_methods$rank_method <- ifelse(filtered_methods$rank_method %in% names(pgs_labels),
                                       pgs_labels[filtered_methods$rank_method],
                                       filtered_methods$rank_method)


filtered_methods$rank_method <- factor(filtered_methods$rank_method, levels = unique(filtered_methods$rank_method))



levels(filtered_methods$percentile)

filtered_methods$percentile <- fct_relevel(filtered_methods$percentile,
                                           "top_5 percent", "top_10 percent", "top_20 percent",
                                           "top_30 percent", "top_50 percent", "top_75 percent", "top_95 percent")




top_percentiles <- filtered_methods %>%
  filter(percentile %in% c("top_5 percent", "top_10 percent", "top_20 percent"))


library(ggplot2)


number_of_methods <- length(unique(top_percentiles$rank_method))
custom_colors <- colorRampPalette(brewer.pal(min(9, number_of_methods), "Set1"))(number_of_methods)


custom_shapes <- c(16, 17, 18, 15, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

while(length(custom_shapes) < number_of_methods){
  custom_shapes <- c(custom_shapes, 21:25)
}


names(custom_colors) <- levels(top_percentiles$rank_method)
names(custom_shapes) <- levels(top_percentiles$rank_method)


p <- ggplot(top_percentiles, aes(x = percentile, y = average_pheno, group = rank_method)) +
  geom_line(aes(color = rank_method)) +
  geom_point(aes(shape = rank_method, color = rank_method), size = 3) +
  scale_shape_manual(values = custom_shapes) +
  scale_color_manual(values = custom_colors) +
  theme_minimal() +
  theme(legend.position = "right") +
  labs(title = "Supervised3000_BMI group8: Comparison of Top Performed RA Methods for Top Percentiles",
       x = "Percentile",
       y = "Average Phenotype Value",
       color = "Rank Method",
       shape = "Rank Method")


print(p)

ggsave("Supervised3000_new_BMI_group8_BEST_TOP_percentiles.png", plot = p, width = 12, height = 8, dpi = 300)

