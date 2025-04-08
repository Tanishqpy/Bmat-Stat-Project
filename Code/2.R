library(readxl)
library(ggplot2)

data <- read_excel("/home/avstr/Tanishq Laptop/Tanishq/B MATH Statistics Project Gagan/database.xlsx", sheet = "Sheet1")
questions <- read_excel("/home/avstr/Tanishq Laptop/Tanishq/B MATH Statistics Project Gagan/questions and id.xlsx", sheet = "questions")

# Convert the data to a data frame
data <- as.data.frame(data)
# Convert the questions to a data frame
questions <- as.data.frame(questions)

# Ensure column names are valid
colnames(data) <- make.names(colnames(data), unique = TRUE, allow_ = TRUE)

# Create directory for plots if it doesn't exist
dir.create("/home/avstr/Tanishq Laptop/Tanishq/B MATH Statistics Project Gagan/plots", showWarnings = FALSE)

# Bar plot for Gender
gender_plot <- ggplot(data, aes(x = Indicate.your.gender)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribution of Gender", x = "Gender", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("/home/avstr/Tanishq Laptop/Tanishq/B MATH Statistics Project Gagan/plots/gender_plot.png", gender_plot)

# Bar plot for Age
age_plot <- ggplot(data, aes(x = Indicate.your.age)) +
  geom_bar(fill = "darkgreen") +
  labs(title = "Distribution of Age", x = "Age", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("/home/avstr/Tanishq Laptop/Tanishq/B MATH Statistics Project Gagan/plots/age_plot.png", age_plot)

# Bar plot for Education
education_plot <- ggplot(data, aes(x = Level.of.education)) +
  geom_bar(fill = "coral") +
  labs(title = "Distribution of Education", x = "Education Level", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("/home/avstr/Tanishq Laptop/Tanishq/B MATH Statistics Project Gagan/plots/education_plot.png", education_plot)

# Bar plot for Employment
employment_plot <- ggplot(data, aes(x = What.is.your.employment.status.)) +
  geom_bar(fill = "purple") +
  labs(title = "Distribution of Employment", x = "Employment Status", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("/home/avstr/Tanishq Laptop/Tanishq/B MATH Statistics Project Gagan/plots/employment_plot.png", employment_plot)

# Bar plot for Residence
residence_plot <- ggplot(data, aes(x = Place.of.residence)) +
  geom_bar(fill = "darkorange") +
  labs(title = "Distribution of Residence", x = "Residence Type", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("/home/avstr/Tanishq Laptop/Tanishq/B MATH Statistics Project Gagan/plots/residence_plot.png", residence_plot)


