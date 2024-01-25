# LIBRARIES ------------------------------------------------------------------------------------------------
# Library to perform basic descriptive statistics
# Check if libraries are already installed or install them if necessary
if(!require(skimr)) install.packages("skimr")
library(skimr)
if(!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)
if(!require(corrplot)) install.packages("corrplot")
library(corrplot)
library(rlang)
library(dplyr)
if(!require(patchwork)) install.packages("patchwork")
library(patchwork)

# FUNCTIONS-------------------------------------------------------------------------------------------------
BarPlotCategoricalPercentageByResponse <- function(data, variable, save_plot = FALSE){
  var_name <- rlang::sym(variable)
  data_summary <- data %>%
    group_by(across(all_of(variable)), Response) %>%
    summarise(Count = n(), .groups = "drop") %>%
    mutate(Percentage = Count / sum(Count))
  
  p <- ggplot(data_summary, aes(x = !!var_name, y = Percentage, fill = Response)) +
    geom_bar(position = "dodge", stat = "identity", color = "black") +
    labs(title = paste(variable, "in terms of Response"), x = variable, y = "Percentage") +
    scale_fill_manual(values = c("red", "blue")) +
    theme(
      text = element_text(size = 12),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 12),
      axis.text = element_text(size = 14),
      axis.title = element_text(size = 14),
    ) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1))
  print(p)
  
  if(save_plot){
    ggsave(paste0(current_path, "/plots/", variable, "_PercentageByResponse.png"), p)
  }
}

BarPlotByResponse <- function(data, variable, save_plot = FALSE){
  var_name <- rlang::sym(variable)
  p <- ggplot(data, aes(x = !!var_name, fill = Response)) + 
    geom_bar(position = "dodge", color = "black") +
    # labs(title = paste(variable, "in terms of Response"), x = variable, y = "Count") +  
    scale_fill_manual(values = c("red", "blue")) +  
    theme(
      text = element_text(size = 12),  
      legend.text = element_text(size = 12),  
      # legend.title = element_text(size = 12),  
      axis.text = element_text(size = 14),  
      # axis.title = element_text(size = 14),  
    )
  
  # print(p)
  
  if(save_plot){
    ggsave(paste0(current_path, "/plots/", variable, "_ByResponse.png"), p)
  }

  return(p)
} 

BarPlotsMatrix <- function(data, save_plot = FALSE){
  p1 <- BarPlotByResponse(data, "Gender")
  p2 <- BarPlotByResponse(data, "Driving_License")
  p3 <- BarPlotByResponse(data, "Region_Code")
  p4 <- BarPlotByResponse(data, "Previously_Insured")
  p5 <- BarPlotByResponse(data, "Vehicle_Age")
  p6 <- BarPlotByResponse(data, "Vehicle_Damage")
  p7 <- BarPlotByResponse(data, "Policy_Sales_Channel")

  p <- p1 + p2 + p3 + 
    p4 + p5 + p6 + 
    p7 + patchwork::plot_spacer() + 
    plot_layout(ncol = 3) +
    plot_annotation(title = "Categorical Variables by Reponse")
  
  print(p)

  if(save_plot){
    ggsave(paste0(current_path, "/plots/", "BarPlots.png"), p)
  }
}

HistogramByResponse <- function(data, variable, save_plot = FALSE){
  var_name <- rlang::sym(variable)
  p <- ggplot(data, aes(x = !!var_name, fill = Response)) +
    geom_histogram(position = "identity", alpha = 0.5) +
    labs(x = variable, y = "Frequency") + 
    scale_fill_manual(values = c("red", "blue")) + 
    theme(
      text = element_text(size = 12),  
      legend.text = element_text(size = 12),  
      axis.text = element_text(size = 14),  
      axis.title = element_text(size = 14)
    )
  
  # print(p)
  
  if(save_plot){
    ggsave(paste0(current_path, "/plots/", variable, "_ByResponse.png"), p)
  }
  
  return(p)
}

HistogramPlotsMatrix <- function(data, save_plot = FALSE){
  p1 <- HistogramByResponse(data, "Age")
  p2 <- HistogramByResponse(data, "Annual_Premium")
  p3 <- HistogramByResponse(data, "Vintage")
  
  p <- p1 + p2 + p3 +
    plot_annotation(title = "Numerical Variables by Reponse")
  
  print(p)
  
  if(save_plot){
    ggsave(paste0(current_path, "/plots/", "HistogramPlots.png"), p)
  }
}

BoxPlotByResponse <- function(data, variable, save_plot = FALSE){
  p <- ggplot(data, aes(x = .data[["Response"]], y = .data[[variable]])) +
    geom_boxplot(fill="steelblue") +
    labs(x = "Response", y = variable) + 
    theme(
      text = element_text(size = 12),  
      legend.text = element_text(size = 12),  
      axis.text = element_text(size = 14)  
    )
  
  if(save_plot){
    ggsave(paste0(current_path, "/plots/Box_", variable, "_ByResponse.png"), p)
  }
  
  return(p)
}

BoxPlotMatrix <- function(data, save_plot = FALSE){
  p1 <- BoxPlotByResponse(data, "Age")
  p2 <- BoxPlotByResponse(data, "Annual_Premium")
  p3 <- BoxPlotByResponse(data, "Vintage")
  
  p <- p1 + p2 + p3 +
    plot_annotation(title = "Numerical Variables by Response")
    
  if(save_plot){
    ggsave(paste0(current_path, "/plots/", "BoxPlots.png"), p)
  }
  
  return(p)
}

# LOAD THE DATA---------------------------------------------------------------------------------------------
# Define the path to the datasets
current_path <- dirname(rstudioapi::getActiveDocumentContext()$path)
datasets_dir <- paste(current_path,"datasets", sep = "/")

# Now that the dataframe is stored in an .RData format we can simply load it and the 
# it will have the same name as the filename
load(paste(datasets_dir, "train_df.RData", sep = "/"))

# In case we may want to do some processing to the test dataset later I'll change the name to the dataframe
data <- get("train_df")

# BASIC DESCRIPTIVE STATISTICS -----------------------------------------------------------------------------

skim(data)

# DISTRIBUTIONS WRT RESPONSE-- -----------------------------------------------------------------------------

## Plot the distribution of the categorical variables

BarPlotsMatrix(data, save_plot = TRUE)

# Previous code
# Open a PNG file for saving plots
png(paste0(current_path, "/plots/", "BarPlotsAllCategorical.png"), width = 800, height = 500)
par(mfrow = c(1, length(factor_variables_names)))  # Set up a multi-panel plot
for (col in factor_variables_names) {
  barplot(table(data[[col]]), main = col, xlab = col, ylab = "Frequency")
}
# Close the PNG file
dev.off()

factor_variables <- sapply(data, is.factor)
factor_variables_names <- names(factor_variables[factor_variables])
factor_variables_names <- factor_variables_names[!factor_variables_names %in% "Response"]

for (variable in factor_variables_names){
  BarPlotByResponse(data, variable, save_plot = TRUE)
  # BarPlotCategoricalPercentageByResponse(data, variable, save_plot = FALSE)
}

## Plot the distribution of the numerical variables

numeric_columns <- sapply(data, is.numeric)
numeric_variables_names <- names(numeric_columns[numeric_columns])

for (variable in numerical_variable_names){
  HistogramByResponse(data, variable, save_plot = TRUE)
}

HistogramPlotsMatrix(data, save_plot = TRUE)

# Previuos code
png(paste0(current_path, "/plots/", "HistogramNumerical.png"), width = 800, height = 500)
par(mfrow = c(1, length(numeric_variables_names)))  # Set up a multi-panel plot
for (col in numeric_variables_names) {
  hist(data[[col]], main = col, xlab = col, ylab = "Frequency")
}
dev.off()

# Box Plots
BoxPlotMatrix(data, save_plot = TRUE)

# CORRELATION AND DEPENDECY BETWEEN VARIABLES-- ------------------------------------------------------------

# Plot the correlation of the numerical variables
png(paste0(current_path, "/plots/", "CorrelationNumericalVars.png"), width = 800, height = 500)
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
dev.off()

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