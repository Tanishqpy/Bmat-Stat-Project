# Load required libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(gplots)

# Read data
data <- read_excel("/home/avstr/Tanishq Laptop/Tanishq/B MATH Statistics Project Gagan/database.xlsx", sheet = "Sheet1")
data <- as.data.frame(data)

# Ensure all column names are valid
colnames(data) <- make.names(colnames(data), unique = TRUE, allow_ = TRUE)

# Create directory for results
dir.create("/home/avstr/Tanishq Laptop/Tanishq/B MATH Statistics Project Gagan/crosstabs", showWarnings = FALSE)

# Function to perform chi-square test and create cross-tabulation visualization
analyze_association <- function(var1, var2, data) {
  # Check if columns exist in the dataset
  if(!(var1 %in% colnames(data)) || !(var2 %in% colnames(data))) {
    cat("\nError: One or both variables don't exist in the dataset.\n")
    cat("Available columns:", paste(colnames(data), collapse=", "), "\n")
    return(NULL)
  }
  
  # Create a contingency table
  cont_table <- table(data[[var1]], data[[var2]])
  
  # Check if the contingency table has at least one positive entry
  if(sum(cont_table) == 0 || all(cont_table == 0)) {
    cat("\nError: The contingency table between", var1, "and", var2, "has no positive entries.\n")
    print(cont_table)
    return(NULL)
  }
  
  # Perform chi-square test with error handling
  tryCatch({
    chi_test <- chisq.test(cont_table)
    
    # Print results
    cat("\nCross-tabulation of", var1, "×", var2, "\n")
    print(cont_table)
    cat("\nChi-square test results:\n")
    print(chi_test)
    
    # Create mosaic plot
    png(paste0("/home/avstr/Tanishq Laptop/Tanishq/B MATH Statistics Project Gagan/crosstabs/", 
               var1, "_", var2, "_mosaic.png"), 
        width = 800, height = 600)
    mosaicplot(cont_table, 
               main = paste0("Mosaic Plot of ", var1, " × ", var2, 
                            "\np-value: ", round(chi_test$p.value, 4)),
               color = TRUE,
               shade = TRUE)
    dev.off()
    
    # Create bar plot with percentages
    plot_data <- as.data.frame(cont_table)
    names(plot_data) <- c("var1", "var2", "count")
    
    # Calculate percentages within each level of var1
    plot_data <- plot_data %>%
      group_by(var1) %>%
      mutate(percentage = count / sum(count) * 100)
    
    bar_plot <- ggplot(plot_data, aes(x = var1, y = percentage, fill = var2)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label = sprintf("%.1f%%", percentage)), 
                position = position_dodge(width = 0.9), 
                vjust = -0.5, size = 3) +
      labs(title = paste0(var1, " × ", var2, 
                        "\nChi-square p-value: ", round(chi_test$p.value, 4)),
           x = var1, y = "Percentage", fill = var2) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggsave(paste0("/home/avstr/Tanishq Laptop/Tanishq/B MATH Statistics Project Gagan/crosstabs/", 
                  var1, "_", var2, "_barplot.png"), bar_plot)
    
    return(list(table = cont_table, test = chi_test, plot = bar_plot))
  }, error = function(e) {
    cat("\nError in chi-square test:", e$message, "\n")
    return(NULL)
  })
}

# Display the first few columns of data to find the e-bike usage column
cat("Exploring column names:\n")
print(head(colnames(data)))

# Check if there's a column that might represent e-bike usage
ebike_cols <- colnames(data)[grep("ebike|bike|use|usage", colnames(data), ignore.case = TRUE)]
if(length(ebike_cols) > 0) {
  cat("\nPotential e-bike usage columns found:", paste(ebike_cols, collapse=", "), "\n")
}

# Using the correct column names from your dataset
gender_col <- "Indicate.your.gender"
age_col <- "Indicate.your.age"
education_col <- "Level.of.education"
employment_col <- "What.is.your.employment.status."
residence_col <- "Place.of.residence"

# For demonstration, let's use one of the identified e-bike columns or prompt the user
# Replace 'use_ebike_col' with the actual column name related to e-bike usage
use_ebike_col <- "YOUR_EBIKE_USAGE_COLUMN"  # Replace this with the actual column name

cat("\nPlease check the output above and replace 'use_ebike_col' with the appropriate column name before continuing.\n")
cat("Then uncomment and run the following analysis lines:\n")

# Example analysis calls with correct column names
# gender_ebike <- analyze_association(gender_col, use_ebike_col, data)
# age_ebike <- analyze_association(age_col, use_ebike_col, data)
# education_ebike <- analyze_association(education_col, use_ebike_col, data)
# employment_ebike <- analyze_association(employment_col, use_ebike_col, data)
# residence_ebike <- analyze_association(residence_col, use_ebike_col, data)
