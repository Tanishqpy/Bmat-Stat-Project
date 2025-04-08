# Install packages if needed
# install.packages(c("readxl", "dplyr", "ggplot2", "tidyr", "stringr"))

# Load libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)

# Read data
data <- read_excel("/home/avstr/Tanishq Laptop/Tanishq/B MATH Statistics Project Gagan/database.xlsx", sheet = "Sheet1")
questions <- read_excel("/home/avstr/Tanishq Laptop/Tanishq/B MATH Statistics Project Gagan/questions and id.xlsx", sheet = "questions")

# Ensure all column names are valid
colnames(data) <- make.names(colnames(data), unique = TRUE, allow_ = TRUE)

# Clean data: Remove rows with missing values
cleaned_data <- data %>%
  drop_na()

# Filter data: Example - Select rows where a specific column meets a condition
filtered_data <- cleaned_data %>%
  filter(Have.you.ever.used.bikesharing..shared.bicycles.. == "Yes")  # Replace 'actual_column_name' and 'specific_value' as needed

# Summarize data: Example - Calculate mean and count for a specific column
summary_stats <- cleaned_data %>%
  group_by(group_column) %>%  # Replace 'group_column' with the column to group by
  summarize(
    mean_value = mean(target_column, na.rm = TRUE),  # Replace 'target_column' with the column to summarize
    count = n()
  )

# Visualize data: Example - Create a bar plot
ggplot(data = summary_stats, aes(x = group_column, y = mean_value)) +  # Replace 'group_column' and 'mean_value'
  geom_bar(stat = "identity") +
  labs(title = "Mean Value by Group", x = "Group", y = "Mean Value") +
  theme_minimal()

# Save cleaned data to a new Excel file
write.xlsx(cleaned_data, "/home/avstr/Tanishq Laptop/Tanishq/B MATH Statistics Project Gagan/cleaned_data.xlsx")

