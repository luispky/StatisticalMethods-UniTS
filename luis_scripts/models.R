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

# MODELING ---------------------------------------------------------------------

gam_all <- gam(Response ~ Gender + Driving_License + Previously_Insured + 
                  Vehicle_Damage + Channels_Reduced + s(log(Age)) +
                  s(log(Annual_Premium)) + Region_Reduced + Vehicle_Age, 
                  data = train_data,
                  # te(Age, Annual_Premium),
                  family = binomial(), 
                  optimizer = 'efs', 
                  select = TRUE)#, # automatic smoothness selection with CV
                  # method = "REML") #Restricted Maximum Likelihood estimation for smoother selection
                  # this approach indirectly achieves regularization by selecting the appropriate degrees of freedom for each smoothing term.
summary(gam_all)  

# Performance Metrics:

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
legend("upright", legend = paste("AUC =", round(auc(roc_curve), 2)), col = "blue", lwd = 2)

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

ggsave(paste(current_path, "/../plots/ConfusionMatrixGAMAll.png", sep = "/"),
      plot = p2,
      width = 10, height = 10, dpi = 300)


# Calculate accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("Accuracy:", round(accuracy, 2), "\n")

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

res <- residuals(gam_all, type="response")

# Calculate binned residuals
binned_residuals <- binned.resids(probabilities, res, nclass = 50)$binned

# Plot binned residuals
plot(range(binned_residuals[,1]),
     range(binned_residuals[,2]),
     type = "n",
     xlab = "Estimated Pr(Switching)", ylab = "Average residual",
     main = "Binned Residual Plot")

# Add lines for different columns in binned_residuals
lines(binned_residuals[,1], col = "red")
lines(binned_residuals[,2], col = "blue")
lines(binned_residuals[,6], col = "green")

# Add legend
legend("topright", legend = c("Column 1", "Column 2", "Column 6"), col = c("red", "blue", "green"), lty = 1)

