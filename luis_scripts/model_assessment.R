# LIBRARIES ------------------------------------------------------------------------------------------------
library(mgcViz)
library(ROSE)
library(ggplot2)
library(mgcv)
library(dplyr)
library(skimr)
library(pROC)
library(caret)

# LOAD THE DATA-----------------------------------------------------------------
# Define the path to the datasets
current_path <- dirname(rstudioapi::getActiveDocumentContext()$path)
datasets_dir <- paste(current_path,"../datasets", sep = "/")
datasets_dir

# load(paste(datasets_dir, "train_reduced.RData", sep = "/"))
load(paste(datasets_dir, "train_data.RData", sep = "/"))
load(paste(datasets_dir, "test_data.RData", sep = "/"))


# FUNCTION TO PERFORM THE MODEL ASSESSMENT -------------------------------------
models_assessment <- function(model, test_data, save_plots = FALSE, plot_auc_name = NULL, plot_cmatrix_name = NULL){
  # Predict probabilities
  probabilities <- predict(model, newdata = subset(test_data, select = -Response), type = "response")

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

  return(list(auc_score, accuracy, optimal_threshold, conf_matrix_prop))
}

# MODELING ---------------------------------------------------------------------

gam_all <- gam(Response ~ Gender + Driving_License + Previously_Insured + 
                  Vehicle_Damage + Channels_Reduced + s(log(Age)) +
                  s(log(Annual_Premium)) + Region_Reduced + Vehicle_Age, 
                  data = train_data,
                  family = binomial(), 
                  optimizer = 'efs', 
                  select = TRUE)#, # automatic smoothness selection with CV
                  # method = "REML") #Restricted Maximum Likelihood estimation for smoother selection
                  # this approach indirectly achieves regularization by selecting the appropriate degrees of freedom for each smoothing term.
summary(gam_all)  

# Performance Metrics: AUC-ROC, Accuracy and Confusion Matrix
result <- models_assessment(gam_all, test_data, save_plots = TRUE, plot_auc_name = "ROCGAMAll", plot_cmatrix_name = "ConfusionMatrixGAMAll")

# Accessing individual elements
auc_score <- result[[1]]
auc_score
accuracy <- result[[2]]
accuracy
optimal_threshold <- result[[3]]
optimal_threshold
conf_matrix_prop <- result[[4]]
conf_matrix_prop