# LIBRARIES ------------------------------------------------------------------------------------------------
# Library to perform basic descriptive statistics
# Check if libraries are already installed or install them if necessary
if(!require(skimr)) install.packages("skimr")
library(skimr)
if(!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)
if(!require(corrplot)) install.packages("corrplot")
library(corrplot)
# LOAD THE DATA---------------------------------------------------------------------------------------------
# Define the path to the datasets
datasets_dir <- paste(dirname(rstudioapi::getActiveDocumentContext()$path),"datasets", sep = "/")

# Now that the dataframe is stored in an .RData format we can simply load it and the 
# it will have the same name as the filename
load(paste(datasets_dir, "train_df.RData", sep = "/"))

# In case we may want to do some processing to the test dataset later I'll change the name to the dataframe
data <- get("train_df")

# BASIC DESCRIPTIVE STATISTICS -----------------------------------------------------------------------------
skim(data)

# Work in progress...

# Plot the distribution of the categorical variables
factor_variables <- sapply(data, is.factor)
factor_variables_names <- names(factor_variables[factor_variables])
par(mfrow = c(1, length(factor_variables_names)))  # Set up a multi-panel plot
for (col in factor_variables_names) {
  barplot(table(data[[col]]), main = col, xlab = col, ylab = "Frequency")
}

# Plot the distribution of the numerical variables
numeric_columns <- sapply(data, is.numeric)
numeric_variable_names <- names(numeric_columns[numeric_columns])
par(mfrow = c(1, length(numeric_variable_names)))  # Set up a multi-panel plot
for (col in numeric_variable_names) {
  hist(data[[col]], main = col, xlab = col, ylab = "Frequency")
}

# Plot the correlation of the numerical variables
corrplot(
  cor(data[, numeric_columns]),  # Calculate correlation matrix, remove missing values
  method = "color",  # Specify method for plotting correlations (using colors)
  type = "upper",    # Specify type of correlation plot (upper triangular)
  order = "hclust",  # Specify order of rows and columns (hierarchical clustering)
  tl.col = "black",  # Set color of text labels (axis labels) to black
  tl.srt = 90,       # Set rotation angle for text labels (orthogonal to the axis)
  addCoef.col = "black",  # Set color of correlation coefficients to black
  tl.cex = 1,      # Set text label character expansion (font size)
  number.cex = 1  # Set text label character expansion for correlation values
)

# Compute independence test of categorical variables using the Pearson's Chi-Squared test.

# Dataframe to store values
chi_squared_results <- data.frame(
  Variable1 = c(),
  Variable2 = c(),
  p_value = c()
)

k <- 2
for (i in factor_variables_names[1:7]){
  for (j in factor_variables_names[k:8]){
    # Extract the variables for the chi-squared test
    var1 <- data[, i]
    var2 <- data[, j]
    
    # Create a contingency table
    contingency_table <- table(var1, var2)

    # Perform the chi-squared test
    result <- chisq.test(contingency_table)

    # Add new row
    new_row <- data.frame(
      Variable1 = i,
      Variable2 = j,
      p_value = result$p.value
    )    
    # Store the result in the list
    chi_squared_results <- rbind(chi_squared_results, new_row)
  }
  k <- k + 1
}
# Print the results
# print(chi_squared_results)
max_row <- chi_squared_results[which.max(chi_squared_results[['p_value']]), ]
print(max_row)