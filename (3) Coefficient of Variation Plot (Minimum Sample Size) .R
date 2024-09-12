library(readxl)
library(dplyr)
library(lme4)
library(ggplot2)

# First Filtering
# Set working directory
setwd("C:/Users/79098/Desktop/Bumblebee_Data")

calculate_cv_per_segment <- function(df, segments) {
  # Split time into segments
  segment_length <- 3600 / segments
  df <- df %>%
    mutate(segment = ceiling(Standardized_time / segment_length)) %>%
    group_by(Colony.no., week, segment) %>%
    summarise(entrance_pollen_count = sum(Event == "entrance_pollen", na.rm = TRUE),
              entrance_none_count = sum(Event == "entrance_none", na.rm = TRUE),
              exit_count = sum(Event == "exit", na.rm = TRUE), .groups = 'drop')
  
  # Calculate coefficient of variation for each colony
  cv_per_colony <- df %>%
    group_by(Colony.no., week) %>%
    summarise(cv_entrance_pollen = sd(entrance_pollen_count, na.rm = TRUE) / mean(entrance_pollen_count, na.rm = TRUE),
              cv_entrance_none = sd(entrance_none_count, na.rm = TRUE) / mean(entrance_none_count, na.rm = TRUE),
              cv_exit = sd(exit_count, na.rm = TRUE) / mean(exit_count, na.rm = TRUE), .groups = 'drop') %>%
    summarise(cv = mean(c(cv_entrance_pollen, cv_entrance_none, cv_exit), na.rm = TRUE))
  
  return(cv_per_colony)
}

# Filter data to include only observations with duration of 3600 seconds
filtered_data <- all_weeks_final %>%
  filter(Obs_Duration == 3600)

# Calculate the number of filtered groups
num_groups <- filtered_data %>%
  distinct(Colony.no., week) %>%
  nrow()

# Initialize
segments_min <- 2  # Minimum number of segments
segments_max <- 60 
cv_results <- data.frame()

# Iterate over segments to calculate coefficient of variation
for (segments in segments_min:segments_max) {
  # Calculate coefficient of variation for the current number of segments
  cv_per_colony <- calculate_cv_per_segment(filtered_data, segments)
  
  # Calculate the mean or median coefficient of variation for all colonies
  mean_cv <- mean(cv_per_colony$cv, na.rm = TRUE)  # Or use median: median(cv_per_colony$cv, na.rm = TRUE)
  
  # Save the current number of segments and corresponding coefficient of variation
  cv_results <- rbind(cv_results, data.frame(segments = segments, mean_cv = mean_cv))
}

# Plot the change in mean coefficient of variation with the number of segments
plot(cv_results$segments, cv_results$mean_cv, type = "b", xlab = "Number of Segments", ylab = "Mean Coefficient of Variation", main = "Variation in Activity by Number of Segments")

# Plot the change in mean coefficient of variation, x-axis as segment duration (60 / Number of Segments)
plot(60 / cv_results$segments, cv_results$mean_cv, type = "b", 
     xlab = "Segment Duration (min)", 
     ylab = "Mean Coefficient of Variation", 
     main = "Variation in Activity by Segment Duration",
     col = "gray", # Set line and point color to gray
     pch = 21, bg = "gray") # Set point fill color to gray

# Add vertical reference lines at 3 minutes and 10 minutes (3 minutes = 180 seconds, 10 minutes = 600 seconds)
abline(v = 3, col = "black", lty = 2)  # Add black dashed line at 180 seconds
abline(v = 10, col = "black", lty = 2)  # Add black dashed line at 600 seconds

# Identify points corresponding to 3 minutes and 10 minutes
points_3min <- which(abs(60 / cv_results$segments - 3) < 1e-6)
points_10min <- which(abs(60 / cv_results$segments - 10) < 1e-6)

# Set point and line color to black for 3 minutes and 10 minutes
points(60 / cv_results$segments[points_3min], cv_results$mean_cv[points_3min], 
       col = "black", bg = "black", pch = 21)

points(60 / cv_results$segments[points_10min], cv_results$mean_cv[points_10min], 
       col = "black", bg = "black", pch = 21)

# Add horizontal reference lines at the y-values for 3 minutes and 10 minutes
abline(h = cv_results$mean_cv[points_3min], col = "black", lty = 2)
abline(h = cv_results$mean_cv[points_10min], col = "black", lty = 2)

# Identify the number of segments with the most significant change in coefficient of variation
cv_segments <- cv_results[which.max(diff(cv_results$mean_cv)), "segments"]
print(paste("Selected number of segments:", cv_segments))

