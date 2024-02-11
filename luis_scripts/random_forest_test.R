library(randomForest)
library(dplyr)
library(pROC)
library(ggplot2)

current_path <- dirname(rstudioapi::getActiveDocumentContext()$path)
datasets_dir <- paste(current_path,"../datasets", sep = "/")
datasets_dir

load(paste(datasets_dir, "unbalanced_train.RData", sep = "/"))
load(paste(datasets_dir, "unbalanced_test.RData", sep = "/"))
unbalanced_train <- select(unbalanced_train, -Policy_Sales_Channel, -Region_Code)
unbalanced_test <- select(unbalanced_test, -Policy_Sales_Channel, -Region_Code)

#*FUNCTION TO PERFORM THE MODEL ASSESSMENT -------------------------------------
random_forest_assessment <- function(model, test_data, save_plots = FALSE, plot_auc_name = NULL, plot_cmatrix_name = NULL){
  # Probabilities prediction of the positive class
  probabilities <- predict(model, newdata = subset(test_data, select = -Response), type = "prob")[, "Yes"]

  # Compute ROC curve
  roc_curve <- roc(test_data$Response, probabilities)
  
  # Calculate AUC
  auc_score <- auc(roc_curve)

  # Find optimal threshold using Youden's J statistic
  youdens_j <- coords(roc_curve, "best", best.method = "youden")
  optimal_threshold <- youdens_j$threshold

  # Save ROC curve plot if specified
  if(save_plots){
    # Open a PNG file to save the ROC curve
    png(paste0(current_path, "/../plots/", plot_auc_name, ".png"),
        width = 10, height = 10,
        units = "in", res = 300)

    # Plot the ROC curve using plot.roc from the pROC package
    plot.roc(roc_curve, col = "blue", main = "ROC Curve", lwd = 2)

    # Add a point for the best threshold
    points(youdens_j$specificity, youdens_j$sensitivity, pch = 19, col = "red")

    # Adding a legend or text to mark the point
    text(youdens_j$specificity, youdens_j$sensitivity, labels = paste("Threshold:", round(optimal_threshold, 2)), pos = 4)

    # Add labels and legend
    abline(h = 0, v = 1, lty = 2, col = "gray")
    legend("topright", legend = paste("AUC =", round(auc(roc_curve), 2)), col = "blue", lwd = 2)

    # Close the PNG file
    dev.off()
  }

  # Obtain predicted classes based on the optimal threshold
  predicted_classes <- ifelse(probabilities > optimal_threshold, "Yes", "No")

  # Create the confusion matrix
  conf_matrix <- table(Actual = test_data$Response, Predicted = predicted_classes)

  conf_matrix_prop <- prop.table(conf_matrix, margin = 1)

  if(save_plots){
    # Plot confusion matrix
    p <- ggplot(data = as.data.frame(conf_matrix_prop), 
                aes(x = Actual, y = Predicted, fill = Freq)) +
      geom_tile(color = "white") +
      geom_text(aes(label = scales::percent(Freq)), vjust = 1) +
      scale_fill_gradient(low = "white", high = "blue") +
      labs(x = "Predicted", y = "Actual", fill = "Proportion") +
      theme_minimal()

    # Save the plot
    ggsave(paste0(current_path, "/../plots/", plot_cmatrix_name, ".png"),
           plot = p,
           width = 10, height = 10, dpi = 300)
  }

  # Calculate accuracy
  accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)

  # Calculate true positive rate
  tpr <- conf_matrix[2, 2] / sum(conf_matrix[2, ])

  # Calculate false positive rate
  fpr <- conf_matrix[1, 2] / sum(conf_matrix[1, ])

  # Calculate true negative rate
  tnr <- conf_matrix[1, 1] / sum(conf_matrix[1, ])

  # Calculate false negative rate
  fnr <- conf_matrix[2, 1] / sum(conf_matrix[2, ])

  # Calculate precision
  precision <- conf_matrix[2, 2] / sum(conf_matrix[, 2])

  # Store the results in a data frame
  results_df <- data.frame(AUC = auc_score,
                          Accuracy = accuracy,
                          TPR = tpr, 
                          FPR = fpr,
                          TNR = tnr,
                          FNR = fnr,
                          Precision = precision,
                          Threshold = optimal_threshold)
  # Return the results
  return(results_df)

}

#*TRAIN THE RANDOM FOREST MODEL ------------------------------------------------

# Set the seed for reproducibility
set.seed(123)

# Train the random forest model
rf_model <- randomForest(Response ~ ., data = unbalanced_train, ntree = 100)

# Assess the model using the test set and don't save the plots
rf_assessment <- random_forest_assessment(rf_model, unbalanced_test)

rf_assessment

# Accuracy of the dummy classifier
# the max proportion of the classes in the test set 
dummy_accuracy <- max(prop.table(table(unbalanced_test$Response)))
dummy_accuracy

# Example of a Random Forest model in R

# # Load necessary libraries
# library(randomForest)
# library(dplyr)

# # Load the iris dataset
# data(iris)

# # Convert it into a binary classification problem
# iris_binary <- iris %>%
#   mutate(Target = ifelse(Species == "setosa", 1, 0))

# # Convert the target variable to a factor
# iris_binary$Target <- as.factor(iris_binary$Target)

# # Refactor the levels of the target variable 
# # 0: No, 1: Yes
# levels(iris_binary$Target) <- c("No", "Yes")

# # Split the dataset into training and testing sets
# set.seed(123)
# sample_index <- sample(1:nrow(iris), nrow(iris) * 0.7)
# train_data <- iris_binary[sample_index, ]
# test_data <- iris_binary[-sample_index, ]

# # Build a random forest model with a specific number of trees (e.g., 100)
# rf_model <- randomForest(Target ~ ., data = train_data, ntree = 100)

# # Predict on the test set and extract probabilities
# rf_probs <- predict(rf_model, newdata = subset(test_data, select = -Target), type = "prob")[, "Yes"]
# rf_probs
# # Find the best threshold for classification using the ROC curve
# library(pROC)

# roc_curve <- roc(test_data$Target, rf_probs)
# best_threshold <- coords(roc_curve, "best", best.method = "youden")$threshold
# best_threshold

# # Evaluate the model using the best threshold
# rf_pred <- ifelse(rf_probs > best_threshold, "Yes", "No")
# conf_matrix <- table(Actual = test_data$Target, Predicted = rf_pred)

# # Print the confusion matrix and accuracy
# print(conf_matrix)
# accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
# cat("Accuracy:", accuracy, "\n")


# # Accuracy of the dummy classifier
# dummy_pred <- rep(1, nrow(test_data))
# dummy_conf_matrix <- table(Actual = test_data$Target, Predicted = dummy_pred)
# dummy_accuracy <- sum(diag(dummy_conf_matrix)) / sum(dummy_conf_matrix)

# # Compare the accuracy of the random forest model with the dummy classifier
# cat("Random Forest Accuracy:", accuracy, "\n")
# cat("Dummy Classifier Accuracy:", dummy_accuracy, "\n")
