#*****************************************************************************************
#* Script with commands I'd like to test
#*****************************************************************************************

# Residuals vs. Fitted Values:
plot(gam_sampled, 1)
# residuals should be scattered randomly
# here we can see a pattern, which means that the model is not capturing all the information

# QQ Plot of Residuals:
plot(gam_sampled, 2)
# The residuals should follow a normal distribution.

# Scale-Location Plot:
plot(gam_sampled, 3)
# residuals should be consistent spread across all levels of predicted values.

# Residuals vs. Predictor:
plot(gam_sampled, 4)
# patterns against each predictor

# Residuals vs. Smoothed Predictor:
plot(gam_sampled, 5)
# Useful for GAMs with smoothed terms.
# Check for patterns in residuals against smoothed predictors.


# plot(gam_sampled, pages=1, scheme=1, unconditional=TRUE)
# plot(gam_sampled, pages=1, scheme=2)
# plot(gam_sampled, pages=1, residuals=TRUE)
# plot(gam_sampled, pages=2, residuals=TRUE)
# plot(gam_sampled, pages=1,seWithMean=TRUE)

# # What does the p-value mean here?
# gam.check(gam_sampled)

# gam_sampledViz <- getViz(gam_sampled)
# print(plot(gam_sampledViz, allTerms = T), pages = 1)
# plot(gam_sampledViz)

# pl <- plot(gam_sampledViz, allTerms = T) + l_points() + l_fitLine(linetype = 3) + l_fitContour() + 
#       l_ciLine(colour = 2) + l_ciBar() + l_fitPoints(size = 1, col = 2) + theme_get() + labs(title = NULL) #+ 
#       # l_dens(type = "cond")
# print(pl, pages = 1)

# # What does the p-value mean here?
# check(gam_sampledViz)

# gam_sampledViz <- getViz(gam_sampled, nsim=100)
# gridPrint(check1D(gam_sampledViz, "log(Age)") + l_gridCheck1D(gridFun = sd, showReps = TRUE), 
#           check1D(gam_sampledViz, "Annual_Premium") + l_gridCheck1D(gridFun = sd, showReps = TRUE),
#           check1D(gam_sampledViz, "Gender") + l_gridCheck1D(gridFun = sd, showReps = TRUE),
#           check1D(gam_sampledViz, "Driving_License") + l_gridCheck1D(gridFun = sd, showReps = TRUE),
#           check1D(gam_sampledViz, "Previously_Insured") + l_gridCheck1D(gridFun = sd, showReps = TRUE),
#           check1D(gam_sampledViz, "Vehicle_Damage") + l_gridCheck1D(gridFun = sd, showReps = TRUE),
#           check1D(gam_sampledViz, "Channels_Reduced") + l_gridCheck1D(gridFun = sd, showReps = TRUE),
#           check1D(gam_sampledViz, "Region_Reduced") + l_gridCheck1D(gridFun = sd, showReps = TRUE),
#           check1D(gam_sampledViz, "Vehicle_Age") + l_gridCheck1D(gridFun = sd, showReps = TRUE)
# )


# The Variance Inflation Factor (VIF) is used to measure the degree of multicollinearity among the predictor variables in a regression model. High VIF values indicate that the variables may be highly correlated, leading to unstable and unreliable estimates of the regression coefficients.

# In your case, the VIF values are extremely high, leading to an issue known as perfect multicollinearity. This happens when one or more variables in the model can be exactly predicted from others. In your car::vif output, all VIF values are infinite (Inf), suggesting that there is a perfect linear relationship between the predictors.

# Looking at your data, it seems like the issue might be related to the way factors are defined. The factors Gender, Driving_License, Previously_Insured, Vehicle_Age, Vehicle_Damage, Channels_Reduced, and Region_Reduced are all converted to factors with two levels. This might lead to perfect multicollinearity because these factors might be perfectly predictable from each other.

# car::vif(gam_sampled)


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
    ggsave(paste0(plot_path, "/../plots/", plot_cmatrix_name, ".png"),
           plot = p,
           width = 10, height = 10, dpi = 300)
  }

  return(list(auc_score, optimal_threshold, conf_matrix_prop))
}