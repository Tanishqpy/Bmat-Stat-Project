# Load required libraries
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(broom)
library(car)  # for Levene's test
library(rstatix)  # for statistical tests
library(corrplot)
library(forcats)  # for factor manipulation

# Read the CSV file
data <- read_csv("/home/avstr/Tanishq Laptop/Tanishq/B MATH Statistics Project Gagan/database.csv")

# Create directory for hypothesis testing results
dir.create("/home/avstr/Tanishq Laptop/Tanishq/B MATH Statistics Project Gagan/hypothesis_tests", 
           showWarnings = FALSE)

# Clean data
data_clean <- data %>%
  # Convert timestamp columns to datetime format
  mutate(
    Start_timestamp = as.POSIXct(`Start timestamp`, format="%Y-%m-%d %H:%M:%OS"),
    End_timestamp = as.POSIXct(`End timestamp`, format="%Y-%m-%d %H:%M:%OS"),
    duration_seconds = as.numeric(difftime(End_timestamp, Start_timestamp, units="secs")),
    duration_minutes = round(duration_seconds / 60, 2)
  ) %>%
  # Filter out NA values in key variables
  filter(!is.na(`Indicate your gender`), 
         !is.na(`Indicate your age`),
         !is.na(duration_minutes))

# Convert key variables to factors for statistical testing
data_clean <- data_clean %>%
  mutate(
    gender = factor(`Indicate your gender`),
    age = factor(`Indicate your age`),
    education = factor(`Level of education`),
    employment = factor(`What is your employment status?`),
    residence = factor(`Place of residence`),
    bike_sharing = factor(`Have you ever used bikesharing (shared bicycles)?`),
    ebike_interest = factor(`If available, would you use a shared electric bike system in Santander?`)
  )

# Create binary variables for easier analysis
data_clean <- data_clean %>%
  mutate(
    used_bikesharing = ifelse(bike_sharing == "Yes", 1, 0),
    interested_ebike = ifelse(ebike_interest == "Yes", 1, 0),
    not_interested_ebike = ifelse(ebike_interest == "No", 1, 0),
    undecided_ebike = ifelse(ebike_interest == "Don't know/depends", 1, 0)
  )

# Print data summary
cat("\n=== DATA SUMMARY ===\n")
summary(data_clean %>% select(gender, age, education, employment, residence, 
                             bike_sharing, ebike_interest, duration_minutes))

#======================================================================
# HYPOTHESIS 1: Is there a difference in e-bike interest between genders?
#======================================================================

# Contingency table for gender and e-bike interest
gender_ebike_table <- table(data_clean$gender, data_clean$ebike_interest)
print(gender_ebike_table)

# Chi-square test for gender and e-bike interest
gender_ebike_test <- chisq.test(gender_ebike_table)
print(gender_ebike_test)

# Visualize the results
gender_ebike_plot <- ggplot(data_clean, aes(x = gender, fill = ebike_interest)) +
  geom_bar(position = "fill") +
  labs(title = "E-bike Interest by Gender",
       subtitle = paste("Chi-square p-value =", round(gender_ebike_test$p.value, 4)),
       x = "Gender", 
       y = "Proportion",
       fill = "E-bike Interest") +
  theme_minimal()

print(gender_ebike_plot)
ggsave("/home/avstr/Tanishq Laptop/Tanishq/B MATH Statistics Project Gagan/hypothesis_tests/gender_ebike_interest.png", 
       gender_ebike_plot, width = 10, height = 6)

# Conclusion
cat("\n=== HYPOTHESIS 1 CONCLUSION ===\n")
if (gender_ebike_test$p.value < 0.05) {
  cat("There is a statistically significant difference in e-bike interest between genders (p =", 
      round(gender_ebike_test$p.value, 4), ").\n")
} else {
  cat("There is NO statistically significant difference in e-bike interest between genders (p =", 
      round(gender_ebike_test$p.value, 4), ").\n")
}

#======================================================================
# HYPOTHESIS 2: Is there a difference in e-bike interest across age groups?
#======================================================================

# Contingency table for age and e-bike interest
age_ebike_table <- table(data_clean$age, data_clean$ebike_interest)
print(age_ebike_table)

# Chi-square test for age and e-bike interest
age_ebike_test <- chisq.test(age_ebike_table)
print(age_ebike_test)

# Visualize the results
age_ebike_plot <- ggplot(data_clean, aes(x = age, fill = ebike_interest)) +
  geom_bar(position = "fill") +
  labs(title = "E-bike Interest by Age Group",
       subtitle = paste("Chi-square p-value =", round(age_ebike_test$p.value, 4)),
       x = "Age Group", 
       y = "Proportion",
       fill = "E-bike Interest") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(age_ebike_plot)
ggsave("/home/avstr/Tanishq Laptop/Tanishq/B MATH Statistics Project Gagan/hypothesis_tests/age_ebike_interest.png", 
       age_ebike_plot, width = 10, height = 6)

# Conclusion
cat("\n=== HYPOTHESIS 2 CONCLUSION ===\n")
if (age_ebike_test$p.value < 0.05) {
  cat("There is a statistically significant difference in e-bike interest across age groups (p =", 
      round(age_ebike_test$p.value, 4), ").\n")
} else {
  cat("There is NO statistically significant difference in e-bike interest across age groups (p =", 
      round(age_ebike_test$p.value, 4), ").\n")
}

#======================================================================
# HYPOTHESIS 3: Is there a relationship between prior bikesharing usage and e-bike interest?
#======================================================================

# Contingency table for bikesharing usage and e-bike interest
bike_ebike_table <- table(data_clean$bike_sharing, data_clean$ebike_interest)
print(bike_ebike_table)

# Chi-square test for bikesharing usage and e-bike interest
bike_ebike_test <- chisq.test(bike_ebike_table)
print(bike_ebike_test)

# Visualize the results
bike_ebike_plot <- ggplot(data_clean, aes(x = bike_sharing, fill = ebike_interest)) +
  geom_bar(position = "fill") +
  labs(title = "E-bike Interest by Prior Bikesharing Usage",
       subtitle = paste("Chi-square p-value =", round(bike_ebike_test$p.value, 4)),
       x = "Has Used Bikesharing", 
       y = "Proportion",
       fill = "E-bike Interest") +
  theme_minimal()

print(bike_ebike_plot)
ggsave("/home/avstr/Tanishq Laptop/Tanishq/B MATH Statistics Project Gagan/hypothesis_tests/bikesharing_ebike_interest.png", 
       bike_ebike_plot, width = 10, height = 6)

# Conclusion
cat("\n=== HYPOTHESIS 3 CONCLUSION ===\n")
if (bike_ebike_test$p.value < 0.05) {
  cat("There is a statistically significant relationship between prior bikesharing usage and e-bike interest (p =", 
      round(bike_ebike_test$p.value, 4), ").\n")
} else {
  cat("There is NO statistically significant relationship between prior bikesharing usage and e-bike interest (p =", 
      round(bike_ebike_test$p.value, 4), ").\n")
}

#======================================================================
# HYPOTHESIS 4: Is there a difference in survey completion time between genders?
#======================================================================

# Group data by gender and calculate summary statistics
gender_duration <- data_clean %>%
  group_by(gender) %>%
  summarize(
    n = n(),
    mean_duration = mean(duration_minutes, na.rm = TRUE),
    sd_duration = sd(duration_minutes, na.rm = TRUE),
    se_duration = sd_duration / sqrt(n)
  )

print(gender_duration)

# Perform t-test if we have at least two gender groups
if(n_distinct(data_clean$gender) >= 2) {
  # Select the two main gender groups for t-test
  gender_data_ttest <- data_clean %>%
    filter(gender %in% c("Male", "Female"))
  
  # Levene's test for equality of variances
  levene_test <- leveneTest(duration_minutes ~ gender, data = gender_data_ttest)
  print(levene_test)
  
  # T-test comparing completion time between genders
  gender_ttest <- t.test(duration_minutes ~ gender, data = gender_data_ttest, 
                       var.equal = (levene_test$`Pr(>F)`[1] > 0.05))
  print(gender_ttest)
  
  # Visualize the results
  gender_duration_plot <- ggplot(gender_data_ttest, aes(x = gender, y = duration_minutes, fill = gender)) +
    geom_boxplot(alpha = 0.7) +
    labs(title = "Survey Completion Time by Gender",
         subtitle = paste("t-test p-value =", round(gender_ttest$p.value, 4)),
         x = "Gender", 
         y = "Completion Time (minutes)") +
    theme_minimal() +
    theme(legend.position = "none")
  
  print(gender_duration_plot)
  ggsave("/home/avstr/Tanishq Laptop/Tanishq/B MATH Statistics Project Gagan/hypothesis_tests/gender_completion_time.png", 
         gender_duration_plot, width = 10, height = 6)
  
  # Conclusion
  cat("\n=== HYPOTHESIS 4 CONCLUSION ===\n")
  if (gender_ttest$p.value < 0.05) {
    cat("There is a statistically significant difference in survey completion time between genders (p =", 
        round(gender_ttest$p.value, 4), ").\n")
  } else {
    cat("There is NO statistically significant difference in survey completion time between genders (p =", 
        round(gender_ttest$p.value, 4), ").\n")
  }
}

#======================================================================
# HYPOTHESIS 5: Is there a difference in survey completion time across age groups?
#======================================================================

# Group data by age and calculate summary statistics
age_duration <- data_clean %>%
  group_by(age) %>%
  summarize(
    n = n(),
    mean_duration = mean(duration_minutes, na.rm = TRUE),
    sd_duration = sd(duration_minutes, na.rm = TRUE),
    se_duration = sd_duration / sqrt(n)
  )

print(age_duration)

# ANOVA test comparing completion time across age groups
age_anova <- aov(duration_minutes ~ age, data = data_clean)
print(summary(age_anova))

# Check ANOVA assumptions
anova_residuals <- residuals(age_anova)
shapiro_test <- shapiro.test(anova_residuals)
print(shapiro_test)

# Kruskal-Wallis test as a non-parametric alternative
kruskal_result <- kruskal.test(duration_minutes ~ age, data = data_clean)
print(kruskal_result)

# Post-hoc tests if ANOVA is significant
if(summary(age_anova)[[1]][["Pr(>F)"]][1] < 0.05) {
  tukey_result <- TukeyHSD(age_anova)
  print(tukey_result)
}

# Visualize the results
age_duration_plot <- ggplot(data_clean, aes(x = age, y = duration_minutes, fill = age)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Survey Completion Time by Age Group",
       subtitle = paste("ANOVA p-value =", round(summary(age_anova)[[1]][["Pr(>F)"]][1], 4),
                       ", Kruskal-Wallis p-value =", round(kruskal_result$p.value, 4)),
       x = "Age Group", 
       y = "Completion Time (minutes)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

print(age_duration_plot)
ggsave("/home/avstr/Tanishq Laptop/Tanishq/B MATH Statistics Project Gagan/hypothesis_tests/age_completion_time.png", 
       age_duration_plot, width = 10, height = 6)

# Conclusion
cat("\n=== HYPOTHESIS 5 CONCLUSION ===\n")
if (summary(age_anova)[[1]][["Pr(>F)"]][1] < 0.05 || kruskal_result$p.value < 0.05) {
  cat("There is a statistically significant difference in survey completion time across age groups.\n")
  cat("ANOVA p-value =", round(summary(age_anova)[[1]][["Pr(>F)"]][1], 4), "\n")
  cat("Kruskal-Wallis p-value =", round(kruskal_result$p.value, 4), "\n")
} else {
  cat("There is NO statistically significant difference in survey completion time across age groups.\n")
  cat("ANOVA p-value =", round(summary(age_anova)[[1]][["Pr(>F)"]][1], 4), "\n")
  cat("Kruskal-Wallis p-value =", round(kruskal_result$p.value, 4), "\n")
}

#======================================================================
# HYPOTHESIS 6: Is there a difference in survey completion time between bike sharers and non-bike sharers?
#======================================================================

# Group data by bikesharing usage and calculate summary statistics
bike_duration <- data_clean %>%
  filter(!is.na(bike_sharing)) %>%
  group_by(bike_sharing) %>%
  summarize(
    n = n(),
    mean_duration = mean(duration_minutes, na.rm = TRUE),
    sd_duration = sd(duration_minutes, na.rm = TRUE),
    se_duration = sd_duration / sqrt(n)
  )

print(bike_duration)

# Filter data for the t-test (removing NAs)
bike_data_ttest <- data_clean %>%
  filter(!is.na(bike_sharing))

# Levene's test for equality of variances
bike_levene_test <- leveneTest(duration_minutes ~ bike_sharing, data = bike_data_ttest)
print(bike_levene_test)

# T-test comparing completion time between bikesharing users and non-users
bike_ttest <- t.test(duration_minutes ~ bike_sharing, data = bike_data_ttest, 
                    var.equal = (bike_levene_test$`Pr(>F)`[1] > 0.05))
print(bike_ttest)

# Visualize the results
bike_duration_plot <- ggplot(bike_data_ttest, aes(x = bike_sharing, y = duration_minutes, fill = bike_sharing)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Survey Completion Time by Bikesharing Usage",
       subtitle = paste("t-test p-value =", round(bike_ttest$p.value, 4)),
       x = "Has Used Bikesharing", 
       y = "Completion Time (minutes)") +
  theme_minimal() +
  theme(legend.position = "none")

print(bike_duration_plot)
ggsave("/home/avstr/Tanishq Laptop/Tanishq/B MATH Statistics Project Gagan/hypothesis_tests/bikesharing_completion_time.png", 
       bike_duration_plot, width = 10, height = 6)

# Conclusion
cat("\n=== HYPOTHESIS 6 CONCLUSION ===\n")
if (bike_ttest$p.value < 0.05) {
  cat("There is a statistically significant difference in survey completion time between bike sharers and non-bike sharers (p =", 
      round(bike_ttest$p.value, 4), ").\n")
} else {
  cat("There is NO statistically significant difference in survey completion time between bike sharers and non-bike sharers (p =", 
      round(bike_ttest$p.value, 4), ").\n")
}

#======================================================================
# HYPOTHESIS 7: Is there a difference in the proportion of e-bike interest between residents of Santander and other locations?
#======================================================================

# Create binary variables for Santander residents and e-bike interest
data_clean <- data_clean %>%
  mutate(
    is_santander = ifelse(residence == "Santander", 1, 0),
    wants_ebike = ifelse(ebike_interest == "Yes", 1, 0)
  )

# Create contingency table
santander_ebike_table <- with(data_clean, table(is_santander, ebike_interest))
print(santander_ebike_table)

# Chi-square test
santander_ebike_test <- chisq.test(santander_ebike_table)
print(santander_ebike_test)

# Proportion test for "Yes" responses
yes_props <- data_clean %>%
  group_by(is_santander) %>%
  summarize(
    n = n(),
    yes_count = sum(wants_ebike),
    prop_yes = yes_count / n
  )

prop_test <- prop.test(yes_props$yes_count, yes_props$n)
print(prop_test)

# Visualization
santander_prop_plot <- ggplot(data_clean, aes(x = factor(is_santander, labels = c("Other", "Santander")), 
                                             fill = ebike_interest)) +
  geom_bar(position = "fill") +
  labs(title = "E-bike Interest: Santander Residents vs Others",
       subtitle = paste("Chi-square p-value =", round(santander_ebike_test$p.value, 4),
                       ", Proportion test p-value =", round(prop_test$p.value, 4)),
       x = "Residence", 
       y = "Proportion",
       fill = "E-bike Interest") +
  theme_minimal()

print(santander_prop_plot)
ggsave("/home/avstr/Tanishq Laptop/Tanishq/B MATH Statistics Project Gagan/hypothesis_tests/santander_ebike_interest.png", 
       santander_prop_plot, width = 10, height = 6)

# Conclusion
cat("\n=== HYPOTHESIS 7 CONCLUSION ===\n")
if (santander_ebike_test$p.value < 0.05) {
  cat("There is a statistically significant difference in e-bike interest between Santander residents and others (p =", 
      round(santander_ebike_test$p.value, 4), ").\n")
} else {
  cat("There is NO statistically significant difference in e-bike interest between Santander residents and others (p =", 
      round(santander_ebike_test$p.value, 4), ").\n")
}

#======================================================================
# SUMMARY OF ALL HYPOTHESIS TESTS
#======================================================================

# Create a summary table of all hypothesis tests
hypothesis_summary <- data.frame(
  Hypothesis = c(
    "Difference in e-bike interest between genders",
    "Difference in e-bike interest across age groups",
    "Relationship between prior bikesharing usage and e-bike interest",
    "Difference in survey completion time between genders",
    "Difference in survey completion time across age groups",
    "Difference in survey completion time between bike sharers and non-bike sharers",
    "Difference in e-bike interest between Santander residents and others"
  ),
  Test = c(
    "Chi-square",
    "Chi-square",
    "Chi-square",
    "t-test",
    "ANOVA/Kruskal-Wallis",
    "t-test",
    "Chi-square/Proportion"
  ),
  P_Value = c(
    gender_ebike_test$p.value,
    age_ebike_test$p.value,
    bike_ebike_test$p.value,
    if(exists("gender_ttest")) gender_ttest$p.value else NA,
    summary(age_anova)[[1]][["Pr(>F)"]][1],
    bike_ttest$p.value,
    santander_ebike_test$p.value
  ),
  Significant = c(
    gender_ebike_test$p.value < 0.05,
    age_ebike_test$p.value < 0.05,
    bike_ebike_test$p.value < 0.05,
    if(exists("gender_ttest")) gender_ttest$p.value < 0.05 else NA,
    summary(age_anova)[[1]][["Pr(>F)"]][1] < 0.05,
    bike_ttest$p.value < 0.05,
    santander_ebike_test$p.value < 0.05
  )
)

# Print and save the summary
print(hypothesis_summary)
write.csv(hypothesis_summary, 
          "/home/avstr/Tanishq Laptop/Tanishq/B MATH Statistics Project Gagan/hypothesis_tests/hypothesis_summary.csv", 
          row.names = FALSE)

# Visualization of p-values
p_values_plot <- ggplot(hypothesis_summary, aes(x = reorder(Hypothesis, -P_Value), y = P_Value)) +
  geom_col(aes(fill = Significant)) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
  scale_fill_manual(values = c("TRUE" = "darkgreen", "FALSE" = "gray")) +
  labs(title = "Summary of Statistical Hypothesis Tests",
       subtitle = "Red dashed line indicates p = 0.05 significance threshold",
       x = "Hypothesis",
       y = "p-value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_values_plot)
ggsave("/home/avstr/Tanishq Laptop/Tanishq/B MATH Statistics Project Gagan/hypothesis_tests/hypothesis_summary_plot.png", 
       p_values_plot, width = 12, height = 6)
