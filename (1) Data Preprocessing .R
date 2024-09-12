library(readxl)
library(dplyr)
library(lubridate)
library(readr)
library(tidyr)

# Set the working directory
setwd("C:/Users/79098/Desktop/Bumblebee_Data")
# Read experimental data
week1 <- read_excel("week1_records.xlsx")
week2 <- read_excel("week2_records.xlsx")
week3 <- read_excel("week3_records.xlsx")
week4 <- read_excel("week4_records.xlsx")

# Read annotation data (i.e., observation time data)
notes1 <- read_excel("week1_notes.xlsx")
notes2 <- read_excel("week2_notes.xlsx")
notes3 <- read_excel("week3_notes.xlsx")
notes4 <- read_excel("week4_notes.xlsx")

# Merge activity records and annotation data, and process observation duration
week1 <- left_join(week1, notes1, by = "Colony.no.") %>%
  mutate(Obs_Duration = ifelse(is.na(Obs_Duration), 3600, Obs_Duration))

week2 <- left_join(week2, notes2, by = "Colony.no.") %>%
  mutate(Obs_Duration = ifelse(is.na(Obs_Duration), 3600, Obs_Duration))

week3 <- left_join(week3, notes3, by = "Colony.no.") %>%
  mutate(Obs_Duration = ifelse(is.na(Obs_Duration), 3600, Obs_Duration))

week4 <- left_join(week4, notes4, by = "Colony.no.") %>%
  mutate(Obs_Duration = ifelse(is.na(Obs_Duration), 3600, Obs_Duration))

# Convert pollen-carrying events into a binary variable
week1 <- week1 %>% mutate(Pollen_foraging_binary = ifelse(Pollen_foraging == "yes", 1, ifelse(Pollen_foraging == "no", 0, NA)))
week2 <- week2 %>% mutate(Pollen_foraging_binary = ifelse(Pollen_foraging == "yes", 1, ifelse(Pollen_foraging == "no", 0, NA)))
week3 <- week3 %>% mutate(Pollen_foraging_binary = ifelse(Pollen_foraging == "yes", 1, ifelse(Pollen_foraging == "no", 0, NA)))
week4 <- week4 %>% mutate(Pollen_foraging_binary = ifelse(Pollen_foraging == "yes", 1, ifelse(Pollen_foraging == "no", 0, NA)))

week1 <- week1 %>% 
  mutate(weight = ifelse(Obs_Duration < 360, 0, 3600 / Obs_Duration))

week2 <- week2 %>% 
  mutate(weight = ifelse(Obs_Duration < 360, 0, 3600 / Obs_Duration))

week3 <- week3 %>% 
  mutate(weight = ifelse(Obs_Duration < 360, 0, 3600 / Obs_Duration))

week4 <- week4 %>% 
  mutate(weight = ifelse(Obs_Duration < 360, 0, 3600 / Obs_Duration))

# Data preprocessing: Add a binary variable for entrance and exit events
week1 <- week1 %>%
  mutate(enter_exit_binary = ifelse(Event %in% c("entrance_none", "entrance_pollen"), 1, ifelse(Event == "exit", 0, NA)))

week2 <- week2 %>%
  mutate(enter_exit_binary = ifelse(Event %in% c("entrance_none", "entrance_pollen"), 1, ifelse(Event == "exit", 0, NA)))

week3 <- week3 %>%
  mutate(enter_exit_binary = ifelse(Event %in% c("entrance_none", "entrance_pollen"), 1, ifelse(Event == "exit", 0, NA)))

week4 <- week4 %>%
  mutate(enter_exit_binary = ifelse(Event %in% c("entrance_none", "entrance_pollen"), 1, ifelse(Event == "exit", 0, NA)))

# Read weather data
weather_data <- read_csv("SilwoodWeatherHourly.csv")

# Convert timestamps in the weather data to POSIXct format and add an end time column
weather_data <- weather_data %>%
  mutate(Start_Time = as.POSIXct(TIMESTAMP, format="%d/%m/%Y %H:%M"),
         End_Time = Start_Time + minutes(59))

# Function: Match weather data based on time range
combine_weather <- function(week_data, weather_data) {
  # Create a datetime column for experimental data
  week_data <- week_data %>%
    mutate(DateTime = as.POSIXct(paste(Date_footage, format(Video_time_stamp, "%H:%M:%S")), format="%Y-%m-%d %H:%M:%S"))
  
  # Initialize new columns for weather data
  weather_columns <- colnames(weather_data)[-which(colnames(weather_data) %in% c("TIMESTAMP", "Start_Time", "End_Time"))]
  week_data[weather_columns] <- NA
  
  # Match weather data based on time range
  for (i in 1:nrow(week_data)) {
    matching_weather <- weather_data %>%
      filter(week_data$DateTime[i] >= Start_Time & week_data$DateTime[i] <= End_Time)
    
    if (nrow(matching_weather) > 0) {
      for (col in weather_columns) {
        week_data[i, col] <- matching_weather[[col]]
      }
    }
  }
  
  return(week_data)
}

# Combine weekly data
week1_final <- combine_weather(week1, weather_data)
week2_final <- combine_weather(week2, weather_data)
week3_final <- combine_weather(week3, weather_data)
week4_final <- combine_weather(week4, weather_data)

# Add week information
week1_final$week <- 1
week2_final$week <- 2
week3_final$week <- 3
week4_final$week <- 4

# Replace treatment names A, B, C with corresponding abbreviations
replace_treatment_names <- function(df) {
  df$treatment <- recode(df$treatment,
                         "A" = "Strep",
                         "B" = "Prothio",
                         "C" = "Control")
  return(df)
}
# Replace treatment names for each week's data
week1_final <- replace_treatment_names(week1_final)
week2_final <- replace_treatment_names(week2_final)
week3_final <- replace_treatment_names(week3_final)
week4_final <- replace_treatment_names(week4_final)

# View the merged data
head(week1_final)
head(week2_final)
head(week3_final)
head(week4_final)

# Merge all weeks' data
all_weeks_final <- bind_rows(week1_final, week2_final, week3_final, week4_final)

# Save final data for each week
write.csv(week1_final, "week1_final.csv", row.names = FALSE)
write.csv(week2_final, "week2_final.csv", row.names = FALSE)
write.csv(week3_final, "week3_final.csv", row.names = FALSE)
write.csv(week4_final, "week4_final.csv", row.names = FALSE)

# Save the merged data
write.csv(all_weeks_final, "all_weeks_final.csv", row.names = FALSE)

