
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
  labs(title = "Supervised3000_BrC group12: Comparison of RA Methods for Top Percentiles",
       x = "Percentile",
       y = "Pheno == 1's Percentage",
       color = "Rank Method",
       shape = "Rank Method")


print(p)

ggsave("Supervised3000_new_BrC_group12_mu_included_TOP_percentiles.png", plot = p, width = 12, height = 8, dpi = 300)















######## 优化 EXCLUDE BAD PERFOMED RA & TOP 5-20 PERCENT ######

library(readrds)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(forcats)


new_percentile_result_list <- readRDS("SUPERVISED3000_new_percentile_result_list.rds")
BrC_group12_results <- new_percentile_result_list[["new_BrC_group12_percentiles_COMBINED_INCLUDED.csv"]]


pgs_benchmarks <- c("supervise3000_ols_rank_no", "pgs_median_rank", "PGS000214_rank", "PGS000497_rank")
ra_methods <- c("bcs_rank", "sbs_rank", "logs_rank", "borda_rank", "mct_rank", "mc4_rank", "combined_average_rank", "combined_median_rank")


all_methods <- c(pgs_benchmarks, ra_methods)


filtered_methods <- BrC_group12_results %>%
  filter(rank_method %in% all_methods)


pgs_labels <- c(
  "supervise3000_ols_rank_no" = "supervised3000 OLS generated: supervise3000_ols",
  "PGS000214_rank" = "supervised3000 R^2 Highest: PGS000214_rank",
  "PGS000497_rank" = "supervised3000 AVG Highest: PGS000497_rank"
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
  labs(title = "Supervised3000_BrC group12: Comparison of Top Performed RA Methods for Top Percentiles",
       x = "Percentile",
       y = "Pheno == 1's Percentage",
       color = "Rank Method",
       shape = "Rank Method")

print(p)
ggsave("Supervised3000_new_BrC_group12_BEST_TOP_percentiles.png", plot = p, width = 12, height = 8, dpi = 300)

