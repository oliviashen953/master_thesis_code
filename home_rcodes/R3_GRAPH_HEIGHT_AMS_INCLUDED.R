library(readr)
library(dplyr)
library(ggplot2)
library(forcats) 
height_summary <- read_csv("R3_Height_summary_standardized_differences_and_rr_across_groups.csv")


ra_methods <- c("mc4_rank", "logs_rank","ams_rank" ,"RA_average_rank")
pgs_methods <- c("supervise3000_ols_rank_no", "PGS001929_rank", "pgs_average_rank")
all_methods <- c(ra_methods, pgs_methods)


filtered_methods <- height_summary %>%
  filter(rank_method %in% all_methods)


pgs_labels <- setNames(c(
  "RA method: MC4",
  "RA method: Nuclear Norm logs_rank",
  "RA method: Nuclear Norm ams_rank",
  "RA Average Rank",
  "Supervised3000 OLS Rank",
  "Supervised3000 Best PGS Model: PGS001929",
  "Average PGS Rank"
), all_methods)


filtered_methods$rank_method <- factor(filtered_methods$rank_method, levels = all_methods, labels = pgs_labels)


filtered_methods$group_percentile <- fct_relevel(filtered_methods$group_percentile, "Top 5%", "Top 10%", "Top 20%")


method_colors <- c(
  "RA method: MC4" = "#E41A1C", # Red
  "RA method: Nuclear Norm logs_rank" = "#377EB8", # Blue
  "RA method: Nuclear Norm ams_rank" = "#4169E1", # ROYAL BLUE
  "RA Average Rank" = "#4DAF4A", # Green
  "Supervised3000 OLS Rank" = "#FF7F00", # Orange
  "Supervised3000 Best PGS Model: PGS001929" = "#984EA3", # Purple
  "Average PGS Rank" = "#FFFF33" # Yellow
)


method_shapes <- setNames(c(19, 17, 11, 15, 18, 16, 13), pgs_labels)


plot <- ggplot(filtered_methods, aes(x = group_percentile, y = mean_standardized_difference, group = rank_method,
                                     shape = rank_method, color = rank_method)) +
  geom_point(size = 3) +
  geom_line() +
  scale_color_manual(values = method_colors) +
  scale_shape_manual(values = method_shapes) +
  theme_bw() +
  labs(y = "Mean Standardized Difference", x = "Group Percentile",
       title = "Height (Supervised Eldest3000): Comparison of Rank Methods Across Top Percentiles") +
  guides(color = guide_legend(override.aes = list(shape = c(19, 17, 11, 15, 18, 16, 13))))


plot
ggsave("R3_AMS_INCLUDED_Height_OG_Top_Percentiles.png", plot = plot, width = 10, height = 8, dpi = 300)
