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


plot(gam_sampled, pages=1, scheme=1, unconditional=TRUE)
plot(gam_sampled, pages=1, scheme=2)
plot(gam_sampled, pages=1, residuals=TRUE)
plot(gam_sampled, pages=2, residuals=TRUE)
plot(gam_sampled, pages=1,seWithMean=TRUE)

# What does the p-value mean here?
gam.check(gam_sampled)

gam_sampledViz <- getViz(gam_sampled)
print(plot(gam_sampledViz, allTerms = T), pages = 1)
plot(gam_sampledViz)

pl <- plot(gam_sampledViz, allTerms = T) + l_points() + l_fitLine(linetype = 3) + l_fitContour() + 
      l_ciLine(colour = 2) + l_ciBar() + l_fitPoints(size = 1, col = 2) + theme_get() + labs(title = NULL) #+ 
      # l_dens(type = "cond")
print(pl, pages = 1)

# What does the p-value mean here?
check(gam_sampledViz)

gam_sampledViz <- getViz(gam_sampled, nsim=100)
gridPrint(check1D(gam_sampledViz, "log(Age)") + l_gridCheck1D(gridFun = sd, showReps = TRUE), 
          check1D(gam_sampledViz, "Annual_Premium") + l_gridCheck1D(gridFun = sd, showReps = TRUE),
          check1D(gam_sampledViz, "Gender") + l_gridCheck1D(gridFun = sd, showReps = TRUE),
          check1D(gam_sampledViz, "Driving_License") + l_gridCheck1D(gridFun = sd, showReps = TRUE),
          check1D(gam_sampledViz, "Previously_Insured") + l_gridCheck1D(gridFun = sd, showReps = TRUE),
          check1D(gam_sampledViz, "Vehicle_Damage") + l_gridCheck1D(gridFun = sd, showReps = TRUE),
          check1D(gam_sampledViz, "Channels_Reduced") + l_gridCheck1D(gridFun = sd, showReps = TRUE),
          check1D(gam_sampledViz, "Region_Reduced") + l_gridCheck1D(gridFun = sd, showReps = TRUE),
          check1D(gam_sampledViz, "Vehicle_Age") + l_gridCheck1D(gridFun = sd, showReps = TRUE)
)

#  Binned Residuals and Partial Residuals

library(arm)

# Get predicted values from the model
predicted_values <- predict(model, test_data, type = "response")

# Calculate residuals
residuals <- residuals(model, type = "deviance")

# Create partial residual plots for each predictor variable
par(mfrow = c(2, 2))  # Adjust the layout based on the number of predictor variables

for (var in colnames(test_data)) {
  if (var != "response") {
    partial_residuals <- residuals(model, type = "partial", terms = var)
    
    # Plot partial residuals
    plot(
      test_data[[var]], 
      partial_residuals,
      type = "p",
      xlab = var,
      ylab = "Partial Residuals",
      main = paste("Partial Residual Plot for", var)
    )
    
    # Calculate and plot binned residuals
    binned_residuals <- binned.resids(test_data[[var]], partial_residuals, nclass = 10)
    
    plot(
      binned_residuals$binned[, "xbar"],
      binned_residuals$binned[, "ybar"],
      type = "p",
      xlab = var,
      ylab = "Average Residual",
      main = paste("Binned Residual Plot for", var),
      ylim = c(-2, 2)
    )
  }
}

# Reset plotting parameters
par(mfrow = c(1, 1))



library(arm)

# Get predicted values from the model
predicted_values <- predict(model, test_data, type = "response")

# Calculate residuals
residuals <- residuals(model, type = "deviance")

# Use binned.resids function
binned_residuals <- binned.resids(predicted_values, residuals, nclass = 50)$binned

# Print binned residuals
print(binned_residuals)

str(binned_residuals)

# Plot binned residuals
plot(
  range(binned_residuals[, 1]),
  range(binned_residuals[, 2], binned_residuals[, 6], -binned_residuals[, 6]),
  type = "n",  
  xlab = "Estimated Pr(Interest in Car Insurance)",
  ylab = "Average Residual",
  main = "Binned Residual Plot",
  # ylim = c(-2, 2)
)
abline(h = 0, col = "gray")
points(binned_residuals[,1], binned_residuals[,2], type = "p", col = "blue")
lines(binned_residuals[,1], binned_residuals[,6], type = "l", col = "blue")
lines(binned_residuals[,1], -binned_residuals[,6], type = "l", col = "blue")
