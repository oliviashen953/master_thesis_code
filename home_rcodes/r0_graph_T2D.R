library(readr)
library(dplyr)
library(ggplot2)
library(forcats)  
library(readr)
T2D_summary <- read_csv("T2D_summary_odds_ratio_and_rr_across_groups.csv")


ra_methods <- c("mc4_rank", "logs_rank", "RA_average_rank")
pgs_methods <- c("supervise3000_ols_rank_no", "PGS002308_rank", "pgs_average_rank")
all_methods <- c(ra_methods, pgs_methods)


filtered_methods <- T2D_summary %>%
  filter(rank_method %in% all_methods)

pgs_labels <- setNames(c(
  "MC4",
  "NNM",
  "AVG_RA_Rank",
  "Supervised_OLS",
  "Supervised_Best: PGS002308",
  "AVG_PRS"
), all_methods)


filtered_methods$rank_method <- factor(filtered_methods$rank_method, levels = all_methods, labels = pgs_labels)


filtered_methods$percentile <- fct_relevel(filtered_methods$percentile, "5%", "10%", "20%")


method_colors <- c(
  "MC4" = "#E41A1C", # Red
  "NNM" = "#377EB8", # Blue
  "AVG_RA_Rank" = "#4DAF4A", # Green
  "Supervised_OLS" = "#FF7F00", # Orange
  "Supervised_Best: PGS002308" = "#984EA3", # Purple
  "AVG_PRS" = "#FFFF33" # Yellow
)

method_shapes <- setNames(c(19, 17, 15, 18, 16, 13), pgs_labels)


filtered_methods <- filtered_methods %>%
  mutate(percentile = fct_recode(percentile,
                                 "Top 5%" = "5%",
                                 "Top 10%" = "10%",
                                 "Top 20%" = "20%")) %>%
  mutate(percentile = fct_relevel(percentile, "Top 5%", "Top 10%", "Top 20%"))


plot <- ggplot(filtered_methods, aes(x = percentile, y = mean_odds_ratio, group = rank_method,
                                     shape = rank_method, color = rank_method)) +
  geom_point(size = 3) +
  geom_line() +
  scale_color_manual(values = method_colors) +
  scale_shape_manual(values = method_shapes) +
  theme_bw() +
  labs(y = "Mean Odds Ratio", x = "Group Percentile")+
       # title = "T2D: Comparison of Rank Methods Across Top Percentiles") +
  guides(color = guide_legend(override.aes = list(shape = c(19, 17, 15, 18, 16, 13))))


plot
ggsave("r0_T2D_OG_Top_Percentiles.png", plot = plot, width = 10, height = 8, dpi = 300)
