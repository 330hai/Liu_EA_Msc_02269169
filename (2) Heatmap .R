library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(readxl)

# Set working directory
setwd("C:/Users/79098/Desktop/Bumblebee_Data")

# Load data files
week1 <- read_excel("week1_records.xlsx")
week2 <- read_excel("week2_records.xlsx")
week3 <- read_excel("week3_records.xlsx")
week4 <- read_excel("week4_records.xlsx")

# Add week offset
offset <- 4200 # Time span for each week
week1 <- week1 %>% mutate(Standardized_time = Standardized_time)
week2 <- week2 %>% mutate(Standardized_time = Standardized_time + offset)
week3 <- week3 %>% mutate(Standardized_time = Standardized_time + 2 * offset)
week4 <- week4 %>% mutate(Standardized_time = Standardized_time + 3 * offset)

# Combine data
all_weeks_data <- bind_rows(week1, week2, week3, week4)

# Sort by treatment and WGT
all_weeks_data <- all_weeks_data %>%
  arrange(treatment, WGT) %>%
  mutate(Colony.no. = factor(Colony.no., levels = unique(Colony.no.)))

# Create time bins (e.g., each minute as one bin)
bin_size <- 60 # 1 minute time bin
all_weeks_data <- all_weeks_data %>%
  mutate(Time_bin = floor(Standardized_time / bin_size))

# Count the number of events in each time bin
heatmap_data <- all_weeks_data %>%
  count(treatment, WGT, Colony.no., Time_bin) %>%
  spread(Time_bin, n, fill = 0)

# Convert data to long format
heatmap_data_melt <- melt(heatmap_data, id.vars = c("treatment", "WGT", "Colony.no."))

# Convert Time_bin to numeric
heatmap_data_melt$variable <- as.numeric(as.character(heatmap_data_melt$variable))

# Adjust time bin range to ensure correct time intervals
max_time_bin <- max(heatmap_data_melt$variable)
expanded_data <- heatmap_data_melt %>%
  complete(variable = seq(0, max_time_bin, by = 1), nesting(treatment, WGT, Colony.no.), fill = list(value = 0))

# Plot heatmap, using facet_grid to group by treatment and WGT
ggplot(expanded_data, aes(x = variable * bin_size, y = Colony.no., fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue", name = "events/min") +
  scale_x_continuous(breaks = seq(0, max(expanded_data$variable) * bin_size, by = 4200), labels = seq(0, max(expanded_data$variable) * bin_size, by = 4200)) +
  coord_cartesian(xlim = c(0, 4 * 4200)) + # Set x-axis range
  labs(title = "Heatmap of Bumblebee Event Over Time",
       x = "Week",
       y = "Colony Number") +
  theme_minimal() +
  facet_grid(treatment ~ ., scales = "free_y", space = "free_y") + # Group by treatment
  theme(
    panel.spacing = unit(1, "lines"), # Add spacing lines
    strip.background = element_blank(), # Remove facet label background
    strip.text = element_text(size = 8, face = "bold") # Set facet label style
  )
