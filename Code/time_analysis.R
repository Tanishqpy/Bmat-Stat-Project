# Load required libraries
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(viridis)

# Read the CSV file
data <- read_csv("/home/avstr/Tanishq Laptop/Tanishq/B MATH Statistics Project Gagan/database.csv")

# Convert timestamp columns to datetime format
data$Start_timestamp <- as.POSIXct(data$`Start timestamp`, format="%Y-%m-%d %H:%M:%OS")
data$End_timestamp <- as.POSIXct(data$`End timestamp`, format="%Y-%m-%d %H:%M:%OS")

# Calculate time duration in seconds
data$duration_seconds <- as.numeric(difftime(data$End_timestamp, data$Start_timestamp, units="secs"))

# Calculate time duration in minutes
data$duration_minutes <- round(data$duration_seconds / 60, 2)

# Create a summary of duration statistics
duration_summary <- data %>%
  summarize(
    min_duration = min(duration_minutes, na.rm = TRUE),
    max_duration = max(duration_minutes, na.rm = TRUE),
    mean_duration = mean(duration_minutes, na.rm = TRUE),
    median_duration = median(duration_minutes, na.rm = TRUE),
    sd_duration = sd(duration_minutes, na.rm = TRUE)
  )

print(duration_summary)

# Create a histogram of completion times
duration_hist <- ggplot(data, aes(x = duration_minutes)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  labs(title = "Distribution of Survey Completion Times",
       x = "Time (minutes)",
       y = "Count") +
  theme_minimal()

print(duration_hist)
ggsave("/home/avstr/Tanishq Laptop/Tanishq/B MATH Statistics Project Gagan/plots/duration_histogram.png", 
       duration_hist, width = 8, height = 6)

# Create a heatmap of completion times by hour of day and day of week
data <- data %>%
  mutate(
    hour_of_day = hour(Start_timestamp),
    day_of_week = wday(Start_timestamp, label = TRUE)
  )

# Count observations for each hour-day combination
heatmap_data <- data %>%
  group_by(day_of_week, hour_of_day) %>%
  summarize(
    count = n(),
    avg_duration = mean(duration_minutes, na.rm = TRUE)
  )

# Create a heatmap for count of participants
count_heatmap <- ggplot(heatmap_data, aes(x = hour_of_day, y = day_of_week, fill = count)) +
  geom_tile() +
  scale_fill_viridis() +
  labs(title = "Heatmap of Survey Participation",
       x = "Hour of Day",
       y = "Day of Week",
       fill = "Count") +
  theme_minimal() +
  theme(legend.position = "right")

print(count_heatmap)
ggsave("/home/avstr/Tanishq Laptop/Tanishq/B MATH Statistics Project Gagan/plots/count_heatmap.png", 
       count_heatmap, width = 10, height = 6)

# Create a heatmap for average completion times
duration_heatmap <- ggplot(heatmap_data, aes(x = hour_of_day, y = day_of_week, fill = avg_duration)) +
  geom_tile() +
  scale_fill_viridis(option = "plasma") +
  labs(title = "Heatmap of Average Survey Completion Time",
       x = "Hour of Day",
       y = "Day of Week",
       fill = "Minutes") +
  theme_minimal() +
  theme(legend.position = "right")

print(duration_heatmap)
ggsave("/home/avstr/Tanishq Laptop/Tanishq/B MATH Statistics Project Gagan/plots/duration_heatmap.png", 
       duration_heatmap, width = 10, height = 6)
