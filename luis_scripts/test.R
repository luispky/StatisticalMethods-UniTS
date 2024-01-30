#*****************************************************************************************
#* Script with commands I'd like to test
#*****************************************************************************************

# In case we may want to do some processing to the test dataset later I'll change the name to the dataframe
# data <- get("train_reduced")

# BALANCE THE DATASET ----------------------------------------------------------
# and train/test split

# Upsample the minority class to balance the dataset
# data_upsampled_balanced <- ROSE(Response ~ ., data = data, seed = 42)$data

# data_upsampled_balanced <- ovun.sample(Response ~ ., data = data, method = "over")$data
# skim(data_upsampled_balanced)

# # Percentage of the data to be used for training
# sample_percentage <- 0.7

# # Sample a subset of the data
# sampled_rows <- sample(nrow(data_upsampled_balanced), round(sample_percentage * nrow(data_upsampled_balanced)), replace = FALSE)

# # Create the training set (the sampled rows)
# data_balanced_train <- data_upsampled_balanced[sampled_rows, ]
# skim(data_balanced_train)

# # Create the test set (the remaining rows)
# data_balanced_test <- data_upsampled_balanced[-sampled_rows, ]
# skim(data_balanced_test)



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




#1. AUC-ROC:
# For binary classification, the Area Under the Receiver Operating Characteristic Curve (AUC-ROC) measures the model's ability to discriminate between classes.

probabilities <- predict(gam_all, newdata = subset(test_data, select = -Response), type = "response") 

roc_curve <- roc(test_data$Response, probabilities)
auc_score <- auc(roc_curve)
# auc_score

youdens_j <- coords(roc_curve, "best", best.method = "youden")
optimal_threshold <- youdens_j$threshold
# print(optimal_threshold)

# Open a PNG file to save the ROC curve
png(paste0(current_path, "/../plots/ROCGAMAll.png"),
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

#2. Confusion Matrix and Accuracy:

# Obtain predicted classes based on the optimal threshold
predicted_classes <- ifelse(probabilities > optimal_threshold, "Yes", "No")

# Create the confusion matrix
conf_matrix <- table(Actual = test_data$Response, Predicted = predicted_classes)

conf_matrix_prop <- prop.table(conf_matrix, margin = 1)

p2 <- ggplot(data = as.data.frame(conf_matrix_prop), 
       aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = scales::percent(Freq)), vjust = 1) +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(x = "Predicted", y = "Actual", fill = "Proportion") +
  theme_minimal()
p2

ggsave(paste0(current_path, "/../plots/ConfusionMatrixGAMAll.png"),
      plot = p2,
      width = 10, height = 10, dpi = 300)


# Calculate accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)









# 3. Binned Residuals:
binned.resids <- function(x, y, nclass = sqrt(length(x))){
  breaks.index <- floor(length(x) * (1 : (nclass))/nclass)
  breaks <- c(-Inf, sort(x)[breaks.index], Inf)
  output <- NULL
  xbreaks <- NULL
  x.binned <- as.numeric(cut(x, breaks))
  for(i in 1:nclass){
    items <- (1:length(x))[x.binned == i]
    x.range <- range(x[items])
    xbar <- mean(x[items])
    ybar <- mean(y[items])
    n <- length(items)
    sdev <- sd(y[items])
    output <- rbind(output, c(xbar, ybar, n, x.range, 2 * sdev/sqrt(n)))
  }
  colnames(output) <- c("xbar", "ybar", "n", "x.lo", "x.hi", "2se")
  return(list(binned = output, xbreaks = xbreaks))
}

gam_all$aic

# Calculate predicted probabilities on training data
train_prob <- predict(gam_all, type = "response")
# Calculate residuals on training data
train_res <- residuals(gam_all, type = "response")

# Calculate binned residuals
binned_residuals <- binned.resids(train_prob, train_res, nclass = 50)$binned

# Plot binned residuals
plot(binned_residuals[,1],
     binned_residuals[,2],
     type = "n",
     xlab = "Estimated Pr(Switching)", ylab = "Average residual",
     main = "Binned Residual Plot")

# Add lines for different columns in binned_residuals
lines(binned_residuals[,1], col = "red")
lines(binned_residuals[,2], col = "blue")
lines(binned_residuals[,6], col = "green")

# Add legend
legend("topright", legend = c("Column 1", "Column 2", "Column 6"), col = c("red", "blue", "green"), lty = 1)


# install.packages("arm")
# library(arm)

# # Get predicted values from the model
# predicted_values <- predict(gam_all, test_data, type = "response")

# # Calculate residuals
# residuals <- residuals(gam_all, type = "deviance")

# # Use binned.resids function
# binned_residuals <- binned.resids(predicted_values, residuals, nclass = 10)

# # Print binned residuals
# print(binned_residuals)

# str(binned_residuals)

# # Plot binned residuals
# plot(
#   binned_residuals$binned[, "xbar"],
#   binned_residuals$binned[, "ybar"],
#   type = "o",  # 'o' for connecting points with lines
#   xlab = "Predicted Values",
#   ylab = "Average Residual",
#   main = "Binned Residual Plot"
# )
 