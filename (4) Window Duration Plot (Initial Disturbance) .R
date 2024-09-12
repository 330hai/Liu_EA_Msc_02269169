library(dplyr)
library(ggplot2)

# Second Filtering
# Filter data to include only observations with a duration of 3600 seconds
filtered_data <- all_weeks_final %>%
  filter(Obs_Duration == 3600)

# Calculate the difference in event rate and pollen carrying rate over different time windows compared to the overall rates
calculate_rate_difference <- function(df, time_window) {
  # Select data within the first `time_window` minutes
  window_data <- df %>%
    filter(Standardized_time <= time_window * 60)  # Convert time window to seconds
  
  # Calculate the event rate and pollen carrying rate within the time window
  window_summary <- window_data %>%
    group_by(Colony.no., week) %>%
    summarise(
      event_rate_window = sum(Event != "none", na.rm = TRUE) / (time_window * 60),  # Event rate (events per unit time)
      pollen_carrying_rate_window = sum(Event == "entrance_pollen", na.rm = TRUE) / sum(Event == "entrance_pollen" | Event == "entrance_none", na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Calculate the overall event rate and pollen carrying rate
  overall_summary <- df %>%
    group_by(Colony.no., week) %>%
    summarise(
      event_rate_overall = sum(Event != "none", na.rm = TRUE) / 3600,  # Overall event rate (events per unit time)
      pollen_carrying_rate_overall = sum(Event == "entrance_pollen", na.rm = TRUE) / sum(Event == "entrance_pollen" | Event == "entrance_none", na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Merge activity data within the time window and overall data
  comparison <- window_summary %>%
    left_join(overall_summary, by = c("Colony.no.", "week"), suffix = c("_window", "_overall"))
  
  # Calculate the difference in event rate and pollen carrying rate for each colony in the time window compared to the overall rates (relative or absolute difference)
  comparison <- comparison %>%
    mutate(
      diff_event_rate = (event_rate_window - event_rate_overall) / event_rate_overall,  # Relative difference
      diff_pollen_carrying_rate = (pollen_carrying_rate_window - pollen_carrying_rate_overall) / pollen_carrying_rate_overall
    )
  
  # Calculate the average difference (mean across colonies or weeks)
  avg_diff <- comparison %>%
    summarise(
      mean_diff_event_rate = mean(diff_event_rate, na.rm = TRUE),
      mean_diff_pollen_carrying_rate = mean(diff_pollen_carrying_rate, na.rm = TRUE)
    )
  
  return(avg_diff)
}

# Initialize a data frame to store results for different time windows
time_window_results <- data.frame()

# Iterate over different time windows to calculate the difference in event rate and pollen carrying rate
for (time_window in 1:15) {  # From 1 minute to 15 minutes
  diff_result <- calculate_rate_difference(filtered_data, time_window)
  diff_result$time_window <- time_window
  time_window_results <- rbind(time_window_results, diff_result)
}

ggplot(time_window_results, aes(x = time_window)) +
  geom_line(aes(y = mean_diff_event_rate * 100, color = "Event Rate Difference (%)"), size = 1.2, alpha = 0.3) +
  geom_line(aes(y = mean_diff_pollen_carrying_rate * 100, color = "Pollen Carrying Rate Difference (%)"), size = 1.2, alpha = 0.5) +
  geom_point(aes(y = mean_diff_event_rate * 100, color = "Event Rate Difference (%)"), size = 3, alpha = 0.2) +
  geom_point(aes(y = mean_diff_pollen_carrying_rate * 100, color = "Pollen Carrying Rate Difference (%)"), size = 3, alpha = 0.5) +
  labs(x = "Time Window (minutes)", y = "Mean Difference (%)", color = "Metric") +
  theme_bw() +
  ggtitle("Difference in Event Rate and Pollen Carrying Rate Over Time Windows") +
  scale_color_manual(values = c("Event Rate Difference (%)" = "red", "Pollen Carrying Rate Difference (%)" = "blue")) +
  scale_x_continuous(breaks = seq(0, max(time_window_results$time_window), by = 2), # Set x-axis ticks, including 10
                     labels = scales::number_format(accuracy = 1)) + # Ensure consistent tick formatting
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.length = unit(-0.25, "cm"), # Turn ticks inward
    legend.position = "right",
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  ) +
  geom_vline(xintercept = 10, linetype = "dashed", color = "gray", size = 1) + # Add vertical line at 10 minutes
  geom_segment(aes(x = 10, xend = 10, y = 0, yend = max(time_window_results$mean_diff_event_rate * 100)), 
               color = "black", alpha = 0) + # Add vertical black line, with transparency
  geom_point(data = subset(time_window_results, time_window == 10), 
             aes(x = time_window, y = mean_diff_event_rate * 100, color = "Event Rate Difference (%)"), 
             size = 4, alpha = 1) + # Overlay opaque red point at 10-minute mark
  geom_point(data = subset(time_window_results, time_window == 10), 
             aes(x = time_window, y = mean_diff_pollen_carrying_rate * 100, color = "Pollen Carrying Rate Difference (%)"), 
             size = 4, alpha = 1) # Overlay opaque blue point at 10-minute mark



