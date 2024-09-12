
# Define the function to calculate the summary table
calculate_summary <- function(data) {
  # Calculate the summary table
  summary_df <- data %>%
    group_by(Colony.no.) %>%
    summarise(
      Obs_Duration = mean(Obs_Duration),  # Calculate the average observation time for each colony
      event_count = n(),  # Count the number of events for each colony
      event_rate = event_count / Obs_Duration,  # Calculate the event rate
      pollen_carrying_rate = sum(Pollen_foraging_binary, na.rm = TRUE) / sum(Activity == 1 & !is.na(Pollen_foraging_binary), na.rm = TRUE),  # Calculate the pollen carrying rate
      enter_count = sum(Activity == 1, na.rm = TRUE),  # Count the number of entrance events
      exit_count = sum(Activity == 0, na.rm = TRUE),  # Count the number of exit events
      lost_rate = (exit_count - enter_count) / exit_count  # Calculate the lost rate
    )
  
  # Match other variables based on Colony.no.
  colony_info <- data %>%
    filter(Standardized_time == 0) %>%
    dplyr::select(Colony.no., WGT, treatment, `Air_Temp_Avg`, `RH (%) [Smp]`, 
                  `WS_kph_ (kilometers/hour) [Avg]`, `SlrW_Avg (W/m^2) [Avg]`, weight, week)
  
  # Merge this information into the summary table
  summary_df <- summary_df %>%
    left_join(colony_info, by = "Colony.no.")
  
  # Set non-zero weights to 1 and filter out unwanted videos
  summary_df <- summary_df %>%
    mutate(weight = ifelse(Obs_Duration >= 600, 1, 0))  # Filter out records with observation time less than 10 minutes (First filter)
  
  return(summary_df)
}

# Read data for each week
week1_summary <- calculate_summary(week1_final)
week2_summary <- calculate_summary(week2_final)
week3_summary <- calculate_summary(week3_final)
week4_summary <- calculate_summary(week4_final)

# Combine data from all weeks
all_weeks_summary <- bind_rows(week1_summary, week2_summary, week3_summary, week4_summary)

# Convert data types
all_weeks_summary$WGT <- as.factor(all_weeks_summary$WGT)
all_weeks_summary$Colony.no. <- as.factor(all_weeks_summary$Colony.no.)

# Filter out the first 10 minutes (Second filter)
all_weeks_summary_filter <- all_weeks_summary %>%
  filter(Standardized_time >= 600)
