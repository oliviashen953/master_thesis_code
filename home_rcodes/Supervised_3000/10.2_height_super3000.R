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


Height_group8_results <- new_percentile_result_list[["supervised3000_new_Height_group8_averages_pheno_by_percentile.csv"]]


library(dplyr)


pgs_benchmarks <- c("supervise3000_ols", "pgs_avg", "pgs_median", "PGS002332")
ra_methods <- c("ams", "bcs", "sbs", "logs", "mu", "borda", "mct", "mc4", "combined_average", "combined_median")


all_methods <- c(pgs_benchmarks, ra_methods)


filtered_methods <- Height_group8_results %>%
  filter(rank_method %in% all_methods)


pgs_labels <- c(
  #"ols" = "supervised OLS generated: OLS",
  "supervise3000_ols" = "supervised3000 OLS generated: supervise3000_ols",
  "PGS002332" = "supervised3000: R^2, Median, Avg Highest: PGS002332"
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
  labs(title = "Height group8: Supvised3000_Comparison of RA Methods for Top Percentiles",
       x = "Percentile",
       y = "Average Height",
       color = "Rank Method",
       shape = "Rank Method")


print(p)
ggsave("Supervised3000_new_Height_group8_TOP_percentiles.png", plot = p, width = 12, height = 8, dpi = 300)



###################################
###################################
######## EXCLUDE BAD PEROMED########
###################################
###################################
###################################


pgs_benchmarks <- c("supervise3000_ols", "pgs_median", "PGS002332")
ra_methods <- c("bcs", "sbs", "logs", "borda", "mct", "mc4", "combined_median")


all_methods <- c(pgs_benchmarks, ra_methods)

filtered_methods <- Height_group8_results %>%
  filter(rank_method %in% all_methods)


pgs_labels <- c(
  #"ols" = "supervised OLS generated: OLS",
  "supervise3000_ols" = "supervised3000 OLS generated: supervise3000_ols",
  "PGS002332" = "supervised3000: R^2, Median, Avg Highest: PGS002332"
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
  labs(title = "Supervised3000: Height group8: Comparison of Top Performed RA Methods for Top Percentiles",
       x = "Percentile",
       y = "Average Height (in cm)",
       color = "Rank Method",
       shape = "Rank Method")


print(p)

ggsave("Supervised3000_Height_group8_BEST_TOP_percentiles.png", plot = p, width = 12, height = 8, dpi = 300)


