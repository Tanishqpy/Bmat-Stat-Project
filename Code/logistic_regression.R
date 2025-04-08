# Load required libraries
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)
library(pROC)
library(car)
library(broom)
library(MASS)
library(effects)

# Create directory for logistic regression results
dir.create("/home/avstr/Tanishq Laptop/Tanishq/B MATH Statistics Project Gagan/logistic_regression", 
           showWarnings = FALSE)

# Read the CSV file
data <- read_csv("/home/avstr/Tanishq Laptop/Tanishq/B MATH Statistics Project Gagan/database.csv")

# Clean and prepare the data
data_clean <- data %>%
  # Select relevant columns
  select(`Indicate your gender`, `Indicate your age`, `Level of education`, 
         `What is your employment status?`, `Place of residence`, 
         `Have you ever used bikesharing (shared bicycles)?`,
         `If available, would you use a shared electric bike system in Santander?`) %>%
  # Rename columns for easier use
  rename(
    gender = `Indicate your gender`,
    age = `Indicate your age`,
    education = `Level of education`,
    employment = `What is your employment status?`,
    residence = `Place of residence`,
    used_bikesharing = `Have you ever used bikesharing (shared bicycles)?`,
    ebike_interest = `If available, would you use a shared electric bike system in Santander?`
  ) %>%
  # Remove rows with NA in key variables
  filter(!is.na(ebike_interest)) %>%
  # Convert to factors where appropriate
  mutate(
    gender = factor(gender),
    age = factor(age),
    education = factor(education),
    employment = factor(employment),
    residence = factor(residence),
    used_bikesharing = factor(used_bikesharing, levels = c("No", "Yes")),
    ebike_interest_binary = ifelse(ebike_interest == "Yes", 1, 0),
    ebike_interest = factor(ebike_interest, levels = c("No", "Yes", "Don't know/depends"))
  )

# Data summary
summary_stats <- data_clean %>%
  group_by(ebike_interest) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

print(summary_stats)

# Exploratory visualization - Distribution of e-bike interest
interest_plot <- ggplot(data_clean, aes(x = ebike_interest, fill = ebike_interest)) +
  geom_bar() +
  geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -0.5) +
  labs(title = "Distribution of E-bike Interest",
       x = "Would use a shared electric bike system in Santander?",
       y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")

print(interest_plot)
ggsave("/home/avstr/Tanishq Laptop/Tanishq/B MATH Statistics Project Gagan/logistic_regression/ebike_interest_distribution.png", 
       interest_plot, width = 10, height = 6)

#======================================================================
# BINARY LOGISTIC REGRESSION (Yes vs No)
#======================================================================

# Create a binary outcome dataset (excluding "Don't know/depends")
binary_data <- data_clean %>%
  filter(ebike_interest %in% c("Yes", "No")) %>%
  mutate(ebike_interest_binary = factor(ebike_interest_binary, levels = c(0, 1), 
                                       labels = c("No", "Yes")))

# Build the binary logistic regression model
binary_model <- glm(ebike_interest_binary ~ gender + age + education + employment + residence + used_bikesharing,
                   data = binary_data, 
                   family = "binomial")

# Model summary
summary_binary <- summary(binary_model)
print(summary_binary)

# Create a cleaner summary using broom
tidy_binary <- tidy(binary_model, conf.int = TRUE, exponentiate = TRUE)
print(tidy_binary)

# Save the model summary
capture.output(summary_binary, 
               file = "/home/avstr/Tanishq Laptop/Tanishq/B MATH Statistics Project Gagan/logistic_regression/binary_model_summary.txt")
write.csv(tidy_binary, 
          "/home/avstr/Tanishq Laptop/Tanishq/B MATH Statistics Project Gagan/logistic_regression/binary_model_coefficients.csv",
          row.names = FALSE)

# Calculate odds ratios and confidence intervals
odds_ratios <- exp(cbind(OR = coef(binary_model), confint(binary_model)))
print(odds_ratios)

# Plot odds ratios for significant variables
significant_vars <- tidy_binary %>% 
  filter(p.value < 0.05 & term != "(Intercept)")

if(nrow(significant_vars) > 0) {
  or_plot <- ggplot(significant_vars, aes(x = reorder(term, estimate), y = estimate)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
    coord_flip() +
    labs(title = "Odds Ratios for Significant Predictors",
         subtitle = "Values > 1 indicate increased odds of e-bike interest",
         x = "Variables",
         y = "Odds Ratio (log scale)") +
    scale_y_log10() +
    theme_minimal()
  
  print(or_plot)
  ggsave("/home/avstr/Tanishq Laptop/Tanishq/B MATH Statistics Project Gagan/logistic_regression/odds_ratios_plot.png", 
         or_plot, width = 10, height = 8)
}

# Model fit statistics
hoslem_test <- hoslem.test(binary_data$ebike_interest_binary == "Yes", fitted(binary_model))
print(hoslem_test)

# ROC Curve and AUC
binary_probs <- predict(binary_model, type = "response")
binary_roc <- roc(binary_data$ebike_interest_binary, binary_probs)
auc_value <- auc(binary_roc)

roc_plot <- ggroc(binary_roc) +
  geom_abline(intercept = 1, slope = 1, linetype = "dashed", color = "gray") +
  labs(title = "ROC Curve for E-bike Interest Prediction",
       subtitle = paste("AUC =", round(auc_value, 3))) +
  theme_minimal()

print(roc_plot)
ggsave("/home/avstr/Tanishq Laptop/Tanishq/B MATH Statistics Project Gagan/logistic_regression/roc_curve.png", 
       roc_plot, width = 8, height = 6)

# Predicted probabilities based on key variables
if("used_bikesharing" %in% names(binary_model$coefficients) && 
   any(grepl("gender", names(binary_model$coefficients)))) {
  
  # Effect plot for bikesharing usage
  bike_effect <- plot(effect("used_bikesharing", binary_model), 
                     main = "Effect of Prior Bikesharing on E-bike Interest")
  dev.copy(png, "/home/avstr/Tanishq Laptop/Tanishq/B MATH Statistics Project Gagan/logistic_regression/bikesharing_effect.png",
           width = 800, height = 600)
  dev.off()
  
  # Effect plot for gender
  gender_effect <- plot(effect("gender", binary_model), 
                      main = "Effect of Gender on E-bike Interest")
  dev.copy(png, "/home/avstr/Tanishq Laptop/Tanishq/B MATH Statistics Project Gagan/logistic_regression/gender_effect.png",
           width = 800, height = 600)
  dev.off()
}

#======================================================================
# MULTINOMIAL LOGISTIC REGRESSION (Yes, No, Don't know/depends)
#======================================================================

# Build the multinomial logistic regression model
multi_model <- multinom(ebike_interest ~ gender + age + education + employment + residence + used_bikesharing,
                      data = data_clean)

# Model summary
summary_multi <- summary(multi_model)
print(summary_multi)

# Calculate p-values for multinomial regression
z_scores <- summary_multi$coefficients / summary_multi$standard.errors
p_values <- (1 - pnorm(abs(z_scores))) * 2
print("P-values for multinomial model:")
print(p_values)

# Save the model summary
capture.output(summary_multi, 
               file = "/home/avstr/Tanishq Laptop/Tanishq/B MATH Statistics Project Gagan/logistic_regression/multinomial_model_summary.txt")

# Predict probabilities
predicted_probs <- predict(multi_model, type = "probs")
head(predicted_probs)

# Create a data frame with actual and predicted categories
multi_results <- data.frame(
  actual = data_clean$ebike_interest,
  predicted = predict(multi_model, type = "class")
)

# Create confusion matrix
conf_matrix <- table(multi_results$actual, multi_results$predicted)
print(conf_matrix)

# Calculate accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("Model accuracy:", round(accuracy * 100, 1), "%\n")

# Save confusion matrix
write.csv(as.data.frame.matrix(conf_matrix), 
          "/home/avstr/Tanishq Laptop/Tanishq/B MATH Statistics Project Gagan/logistic_regression/multinomial_confusion_matrix.csv")

# Create heatmap of confusion matrix
conf_heatmap <- ggplot(as.data.frame(as.table(conf_matrix)), 
                     aes(x = Var2, y = Var1, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white", size = 4) +
  scale_fill_gradient(low = "steelblue", high = "darkblue") +
  labs(title = "Confusion Matrix for Multinomial Logistic Regression",
       subtitle = paste("Overall accuracy:", round(accuracy * 100, 1), "%"),
       x = "Predicted Category",
       y = "Actual Category") +
  theme_minimal()

print(conf_heatmap)
ggsave("/home/avstr/Tanishq Laptop/Tanishq/B MATH Statistics Project Gagan/logistic_regression/confusion_matrix.png", 
       conf_heatmap, width = 8, height = 6)

#======================================================================
# VARIABLE IMPORTANCE AND RELATIONSHIP STRENGTH
#======================================================================

# For binary model - calculate variable importance
varImp_binary <- varImp(binary_model)
varImp_df <- data.frame(
  variable = rownames(varImp_binary),
  importance = varImp_binary[,1]
)
varImp_df <- varImp_df[order(-varImp_df$importance),]
print(varImp_df)

# Create variable importance plot
imp_plot <- ggplot(varImp_df %>% filter(variable != "(Intercept)"), 
                 aes(x = reorder(variable, importance), y = importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Variable Importance for E-bike Interest Prediction",
       x = "Variables",
       y = "Importance Score") +
  theme_minimal()

print(imp_plot)
ggsave("/home/avstr/Tanishq Laptop/Tanishq/B MATH Statistics Project Gagan/logistic_regression/variable_importance.png", 
       imp_plot, width = 10, height = 6)

#======================================================================
# CONCLUSIONS
#======================================================================

# Create a summary of findings
cat("\n=== LOGISTIC REGRESSION ANALYSIS CONCLUSIONS ===\n")

# Binary model significant predictors
sig_predictors <- tidy_binary %>% 
  filter(p.value < 0.05 & term != "(Intercept)") %>%
  arrange(p.value)

cat("\nSignificant predictors of e-bike interest (binary model):\n")
if(nrow(sig_predictors) > 0) {
  for(i in 1:nrow(sig_predictors)) {
    direction <- ifelse(sig_predictors$estimate[i] > 1, "increased", "decreased")
    cat(paste("- ", sig_predictors$term[i], ": ", direction, " odds of e-bike interest (OR = ", 
             round(sig_predictors$estimate[i], 2), ", p = ", round(sig_predictors$p.value[i], 4), ")\n", sep=""))
  }
} else {
  cat("No statistically significant predictors found.\n")
}

cat("\nModel performance:\n")
cat(paste("- Binary logistic regression accuracy: ", round(mean(binary_data$ebike_interest_binary == 
                                                            ifelse(binary_probs > 0.5, "Yes", "No")) * 100, 1), "%\n", sep=""))
cat(paste("- AUC: ", round(auc_value, 3), "\n", sep=""))
cat(paste("- Multinomial model accuracy: ", round(accuracy * 100, 1), "%\n", sep=""))

# Save conclusions
capture.output(
  cat("\n=== LOGISTIC REGRESSION ANALYSIS CONCLUSIONS ===\n"),
  cat("\nSignificant predictors of e-bike interest (binary model):\n"),
  if(nrow(sig_predictors) > 0) {
    for(i in 1:nrow(sig_predictors)) {
      direction <- ifelse(sig_predictors$estimate[i] > 1, "increased", "decreased")
      cat(paste("- ", sig_predictors$term[i], ": ", direction, " odds of e-bike interest (OR = ", 
               round(sig_predictors$estimate[i], 2), ", p = ", round(sig_predictors$p.value[i], 4), ")\n", sep=""))
    }
  } else {
    cat("No statistically significant predictors found.\n")
  },
  cat("\nModel performance:\n"),
  cat(paste("- Binary logistic regression accuracy: ", round(mean(binary_data$ebike_interest_binary == 
                                                              ifelse(binary_probs > 0.5, "Yes", "No")) * 100, 1), "%\n", sep="")),
  cat(paste("- AUC: ", round(auc_value, 3), "\n", sep="")),
  cat(paste("- Multinomial model accuracy: ", round(accuracy * 100, 1), "%\n", sep="")),
  file = "/home/avstr/Tanishq Laptop/Tanishq/B MATH Statistics Project Gagan/logistic_regression/conclusions.txt"
)
