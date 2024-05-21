library(readr)
library(dplyr)
library(ggplot2)
library(forcats) 
library(RColorBrewer)


height_summary <- read_csv("Height_summary_standardized_differences_and_rr_across_groups.csv")


ra_methods <- c("mc4_rank", "logs_rank", "RA_median_rank")
pgs_methods <- c("real_ols_rank_no", "PGS001929_rank", "pgs_average_rank")
all_methods <- c(ra_methods, pgs_methods)


filtered_methods <- height_summary %>%
  filter(rank_method %in% all_methods)


pgs_labels <- c(
  "mc4_rank" = "RA method: MC4",
  "logs_rank" = "RA method: Nuclear Norm logs_rank",
  "RA_median_rank" = "RA Median Rank",
  "real_ols_rank_no" = "Supervised OLS Rank",
  "PGS001929_rank" = "PGS Model: PGS001929",
  "pgs_average_rank" = "Average PGS Rank"
)

filtered_methods$rank_method <- as.character(filtered_methods$rank_method)
filtered_methods$rank_method <- ifelse(filtered_methods$rank_method %in% names(pgs_labels),
                                       pgs_labels[filtered_methods$rank_method],
                                       filtered_methods$rank_method)


filtered_methods$rank_method <- factor(filtered_methods$rank_method, levels = unique(filtered_methods$rank_method))


filtered_methods$group_percentile <- fct_relevel(filtered_methods$group_percentile,
                                                 "Top 5%", "Top 10%", "Top 20%")


method_colors <- setNames(brewer.pal(min(8, length(all_methods)), "Dark2"), all_methods)


ggplot(filtered_methods, aes(x = group_percentile, y = mean_standardized_difference, group = rank_method,
                             shape = rank_method, color = rank_method)) +
  geom_point(size = 3) +
  geom_line() +
  scale_color_manual(values = method_colors) +
  theme_bw() +
  labs(y = "Mean Standardized Difference", x = "Group Percentile",
       title = "Comparison of Rank Methods Across Top Percentiles") +
  scale_shape_manual(values = seq(1, length(all_methods)))


ggsave("Comparison_of_Rank_Methods_Across_Top_Percentiles.png", width = 10, height = 8, dpi = 300)
