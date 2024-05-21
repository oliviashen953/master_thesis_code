
height_data <- read_csv("your_data_file.csv")


calculate_standardized_difference <- function(top_mean, bottom_mean, top_se, bottom_se) {
  difference <- top_mean - bottom_mean
  se_difference <- sqrt(top_se^2 + bottom_se^2)
  standardized_difference <- difference / se_difference
  return(standardized_difference)
}


calculate_relative_risk <- function(top_mean, bottom_mean) {
  relative_risk <- top_mean / bottom_mean
  return(relative_risk)
}


height_data <- height_data %>%
  mutate(
    standardized_diff_top5_bottom95 = calculate_standardized_difference(top_5_percent_height, bottom_95_percent_height, top_5_percent_SE, bottom_95_percent_SE),
    standardized_diff_top10_bottom90 = calculate_standardized_difference(top_10_percent_height, bottom_90_percent_height, top_10_percent_SE, bottom_90_percent_SE),
    relative_risk_top5_vs_bottom95 = calculate_relative_risk(top_5_percent_height, bottom_95_percent_height)
  )


print(height_data)
