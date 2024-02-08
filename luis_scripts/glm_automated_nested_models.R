#*LIBRARIES ------------------------------------------------------------------------------------------------
library(mgcViz)
library(ROSE)
library(ggplot2)
library(mgcv)
library(dplyr)
library(skimr)
library(pROC)
library(caret)
library(purrr)
#*LOAD THE DATA-----------------------------------------------------------------
# Define the path to the datasets
current_path <- dirname(rstudioapi::getActiveDocumentContext()$path)
datasets_dir <- paste(current_path,"../datasets", sep = "/")
datasets_dir


# load(paste(datasets_dir, "train_reduced.RData", sep = "/"))
load(paste(datasets_dir, "train_data.RData", sep = "/"))
load(paste(datasets_dir, "test_data.RData", sep = "/"))
train_data <- select(train_data, -Policy_Sales_Channel, -Region_Code)
test_data <- select(test_data, -Policy_Sales_Channel, -Region_Code)

#*--------------------------------------------------------------------
#*FUNCTIONS 
#*--------------------------------------------------------------------

# * This function assumes that `Policy_Sales_Channel` and `Region_Code` have been removed from the data
ranking_nested_models_gml <- function(train_data, test_data) {
  #*VARIABLES IMPORTANCE RANKING --------------------------------------------------

  # Sort variables by importance wrt AIC
  # We remove one variable at a time and by decreasing AIC we get the most important variables
  # i.e., the variables that when removed increase the AIC is important
  predictors <- colnames(train_data)
  predictors <- predictors[predictors != 'Response']
  ranking_variables_models <- list()

  for (predictor in predictors){
    if (predictor == "Age"){
      formula_string <- paste("Response ~ . - Annual_Premium + I(log(Annual_Premium)) -", predictor)
    } else if (predictor == "Annual_Premium"){
      formula_string <- paste("Response ~ . - Age + I(log(Age)) -", predictor)
    } else {
      formula_string <- paste("Response ~ . - Age - Annual_Premium + I(log(Age)) + I(log(Annual_Premium)) -", predictor)
    }
    model_formula <- as.formula(formula_string)
    model <- glm(model_formula, data = train_data, family = binomial)
    ranking_variables_models[[predictor]] <- model
  }

  # Compute AIC values
  ranking_variables_aic_values <- sapply(ranking_variables_models, AIC)

  # Sort variables by AIC values
  df_ranking_variables_aic <- data.frame(VariableRemoved = predictors, AIC = ranking_variables_aic_values)
  # Assuming df is your DataFrame
  df_sorted_ranking_variables_aic <- df_ranking_variables_aic[order(df_ranking_variables_aic$AIC, decreasing=TRUE), ]
  # df_sorted_ranking_variables_aic

  #* NESTED MODELS ----------------------------------------------------------------
  variables_order <- df_sorted_ranking_variables_aic$VariableRemoved
  # variables_order
  variables_nested <- c()
  nested_models <- list()

  for (variable in variables_order) {
    if (variable == "Age"){
      variables_nested <- c(variables_nested, "I(log(Age))")
    } else if (variable == "Annual_Premium"){
      variables_nested <- c(variables_nested, "I(log(Annual_Premium))")
    } else {
      variables_nested <- c(variables_nested, variable)
    }
    formula_string <- paste("Response", "~", paste(variables_nested, collapse = " + "))
    print(formula_string)  
    model_formula <- as.formula(formula_string)
    model <- glm(model_formula, data = train_data, family = binomial)
    nested_models[[variable]] <- model
  }

  # Compute AIC values
  raking_nested_models_aic_values <- sapply(nested_models, AIC)
  df_ranking_nested_models_aic <- data.frame(Model_Name = variables_order, AIC = raking_nested_models_aic_values)
  # df_ranking_nested_models_aic

  # Sort variables by AIC values
  df_sorted_ranking_nested_models_aic <- df_ranking_nested_models_aic[order(df_ranking_nested_models_aic$AIC, decreasing=TRUE), ]
  # df_sorted_ranking_nested_models_aic

  # Print the order of the variables to compare 
  # variables_order

  # COMPUTE AUC AND ACCURACY FOR EACH MODEL -------------------------------------

  # Apply models_assessment function to each model using map
  results_list <- map(nested_models, ~models_assessment(.x, test_data))

  # Compute AUC values
  auc_values <- list()
  accuracy_values <- list()
  tpr_values <- list()
  fpr_values <- list()
  tnr_values <- list()
  fnr_values <- list()
  precision_values <- list()
  threshold_values <- list()

  for (i in 1:length(results_list)){
    auc_values <- c(auc_values, as.numeric(results_list[[i]][1]))
    accuracy_values <- c(accuracy_values, as.numeric(results_list[[i]][2]))
    tpr_values <- c(tpr_values, as.numeric(results_list[[i]][3]))
    fpr_values <- c(fpr_values, as.numeric(results_list[[i]][4]))
    tnr_values <- c(tnr_values, as.numeric(results_list[[i]][5]))
    fnr_values <- c(fnr_values, as.numeric(results_list[[i]][6]))
    precision_values <- c(precision_values, as.numeric(results_list[[i]][7]))
    threshold_values <- c(threshold_values, as.numeric(results_list[[i]][8]))
  }

  result_df <- data.frame(
    Model_Name = df_ranking_nested_models_aic$Model_Name, 
    AIC = df_ranking_nested_models_aic$AIC,
    AUC = unlist(auc_values),
    Accuracy = unlist(accuracy_values),
    TPR = unlist(tpr_values),
    FPR = unlist(fpr_values),
    TNR = unlist(tnr_values),
    FNR = unlist(fnr_values),
    Precision = unlist(precision_values),
    Threshold = unlist(threshold_values)
  )

  # Return ranking of variables and results
  return(list(Ranking_Variables = df_sorted_ranking_variables_aic, Results = result_df))

}

#*FUNCTION TO PERFORM THE MODEL ASSESSMENT -------------------------------------
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

  return(list(auc_score = auc_score, accuracy = accuracy,
              tpr = tpr, fpr = fpr, tnr = tnr, fnr = fnr,
              precision = precision, optimal_threshold = optimal_threshold))

}

#*--------------------------------------------------------------------
#*RUN THE MODEL

result <- ranking_nested_models_gml(train_data, test_data)
result
