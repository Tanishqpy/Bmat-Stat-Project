# Load required libraries
library(readr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(gplots)
library(corrplot)

# Read the CSV file
data <- read_csv("/home/avstr/Tanishq Laptop/Tanishq/B MATH Statistics Project Gagan/database.csv")

# Create directory for chi-square test results
dir.create("/home/avstr/Tanishq Laptop/Tanishq/B MATH Statistics Project Gagan/chi_square_results", 
           showWarnings = FALSE)

# Clean the data by removing rows with missing values in key variables
data_clean <- data %>%
  filter(!is.na(`Indicate your gender`)) %>%
  filter(!is.na(`Indicate your age`))

# Function to perform chi-square test between two categorical variables
perform_chi_square_test <- function(data, var1, var2) {
  # Check if both variables exist in the data
  if(!(var1 %in% colnames(data)) || !(var2 %in% colnames(data))) {
    cat("\nError: One or both variables don't exist in the dataset.\n")
    return(NULL)
  }
  
  # Extract the variables, removing NA values
  df <- data %>%
    select(all_of(c(var1, var2))) %>%
    filter(!is.na(!!sym(var1)), !is.na(!!sym(var2)))
  
  # Create the contingency table
  cont_table <- table(df[[var1]], df[[var2]])
  
  # Check if the table has at least one positive entry
  if(sum(cont_table) == 0) {
    cat("\nError: The contingency table between", var1, "and", var2, "has no entries.\n")
    return(NULL)
  }
  
  # Perform chi-square test with error handling
  tryCatch({
    chi_test <- chisq.test(cont_table)
    
    # Print results
    cat("\n========================================\n")
    cat("Chi-square test:", var1, "x", var2, "\n")
    cat("----------------------------------------\n")
    print(cont_table)
    cat("\nChi-square value:", chi_test$statistic, "\n")
    cat("Degrees of freedom:", chi_test$parameter, "\n")
    cat("p-value:", chi_test$p.value, "\n")
    cat("Significant (p < 0.05):", chi_test$p.value < 0.05, "\n")
    cat("========================================\n")
    
    # Create a visualization of the contingency table
    # First, convert to percentage for better visualization
    plot_data <- as.data.frame(cont_table)
    names(plot_data) <- c("var1", "var2", "Freq")
    
    # Calculate percentages within var1
    plot_data <- plot_data %>%
      group_by(var1) %>%
      mutate(percentage = Freq / sum(Freq) * 100)
    
    # Create the plot
    p <- ggplot(plot_data, aes(x = var1, y = percentage, fill = var2)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label = sprintf("%.1f%%", percentage)), 
                position = position_dodge(width = 0.9), 
                vjust = -0.5, size = 3) +
      labs(title = paste0("Chi-square test: ", var1, " x ", var2),
           subtitle = paste0("p-value = ", round(chi_test$p.value, 4), 
                             " (", ifelse(chi_test$p.value < 0.05, "significant", "not significant"), ")"),
           x = var1,
           y = "Percentage",
           fill = var2) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Save the plot
    filename <- paste0("/home/avstr/Tanishq Laptop/Tanishq/B MATH Statistics Project Gagan/chi_square_results/", 
                        gsub(" ", "_", var1), "_vs_", gsub(" ", "_", var2), ".png")
    ggsave(filename, p, width = 10, height = 6)
    
    return(list(table = cont_table, test = chi_test, plot = p))
  }, error = function(e) {
    cat("\nError performing chi-square test:", e$message, "\n")
    return(NULL)
  })
}

# List of key variables to test
key_variables <- c(
  "Indicate your gender",
  "Indicate your age",
  "Level of education",
  "What is your employment status?",
  "Place of residence",
  "Have you ever used bikesharing (shared bicycles)?",
  "If available, would you use a shared electric bike system in Santander?"
)

# Perform chi-square tests for combinations of variables
results <- list()

# Test relationships between background variables and bikesharing usage
cat("\n\n== CHI-SQUARE TESTS: DEMOGRAPHIC VARIABLES VS. BIKESHARING USAGE ==\n")
for(var in key_variables[1:5]) {
  results[[paste(var, "vs bikesharing")]] <- 
    perform_chi_square_test(data_clean, var, "Have you ever used bikesharing (shared bicycles)?")
}

# Test relationships between background variables and e-bike interest
cat("\n\n== CHI-SQUARE TESTS: DEMOGRAPHIC VARIABLES VS. E-BIKE INTEREST ==\n")
for(var in key_variables[1:5]) {
  results[[paste(var, "vs e-bike")]] <- 
    perform_chi_square_test(data_clean, var, "If available, would you use a shared electric bike system in Santander?")
}

# Test relationship between bikesharing usage and e-bike interest
cat("\n\n== CHI-SQUARE TEST: BIKESHARING USAGE VS. E-BIKE INTEREST ==\n")
results[["bikesharing vs e-bike"]] <- 
  perform_chi_square_test(data_clean, 
                         "Have you ever used bikesharing (shared bicycles)?", 
                         "If available, would you use a shared electric bike system in Santander?")

# Create a summary table of all test results
summary_table <- data.frame(
  Relationship = character(),
  ChiSquare = numeric(),
  DoF = numeric(),
  PValue = numeric(),
  Significant = logical(),
  stringsAsFactors = FALSE
)

# Populate the summary table
for(name in names(results)) {
  if(!is.null(results[[name]])) {
    summary_table <- rbind(summary_table, data.frame(
      Relationship = name,
      ChiSquare = results[[name]]$test$statistic,
      DoF = results[[name]]$test$parameter,
      PValue = results[[name]]$test$p.value,
      Significant = results[[name]]$test$p.value < 0.05
    ))
  }
}

# Sort the summary table by significance and p-value
summary_table <- summary_table %>%
  arrange(Significant, PValue)

# Print and save the summary table
print(summary_table)
write.csv(summary_table, 
          "/home/avstr/Tanishq Laptop/Tanishq/B MATH Statistics Project Gagan/chi_square_results/summary_table.csv", 
          row.names = FALSE)

# Create a plot of p-values for visualization
p_values_plot <- ggplot(summary_table, aes(x = reorder(Relationship, -PValue), y = PValue)) +
  geom_col(aes(fill = Significant)) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
  scale_fill_manual(values = c("TRUE" = "darkgreen", "FALSE" = "gray")) +
  labs(title = "Summary of Chi-square Test Results",
       subtitle = "Red dashed line indicates p = 0.05 significance threshold",
       x = "Variable Relationship",
       y = "p-value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_values_plot)
ggsave("/home/avstr/Tanishq Laptop/Tanishq/B MATH Statistics Project Gagan/chi_square_results/p_values_plot.png", 
       p_values_plot, width = 12, height = 6)

# Create a correlation plot for significant relationships
significant_results <- summary_table %>%
  filter(Significant == TRUE)

if(nrow(significant_results) > 0) {
  cat("\n\nSIGNIFICANT RELATIONSHIPS FOUND:\n")
  print(significant_results)
  
  cat("\nThese relationships suggest statistically significant associations between the variables.\n")
  cat("Examine the visualizations in the chi_square_results folder for more details.\n")
} else {
  cat("\n\nNO SIGNIFICANT RELATIONSHIPS FOUND.\n")
}
