# LIBRARIES ------------------------------------------------------------------------------------------------
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

load(paste(datasets_dir, "train_reduced.RData", sep = "/"))

prop.table(table(train_reduced$Response))

data <- get("train_reduced")

# # Percentage of the data to be used for training
sample_percentage <- 0.7

# # Sample a subset of the data
sampled_rows <- sample(nrow(data), round(sample_percentage * nrow(data)), replace = FALSE)

# # Create the training set (the sampled rows)
train_data <- data[sampled_rows, ]
prop.table(table(train_data$Response))

# # Create the test set (the remaining rows)
test_data <- data[-sampled_rows, ]
prop.table(table(test_data$Response))

# MODELING ---------------------------------------------------------------------

gam_all <- gam(Response ~ Gender + Driving_License + Previously_Insured + 
                  Vehicle_Damage + Channels_Reduced + s(I(log(Age))) +
                  s(I(log(Annual_Premium))) + Region_Reduced + Vehicle_Age, 
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
auc_score

youdens_j <- coords(roc_curve, "best", best.method = "youden")
optimal_threshold <- youdens_j$threshold
# print(optimal_threshold)

# Plot the ROC curve using plot.roc from the pROC package
plot.roc(roc_curve, col = "blue", main = "ROC Curve", lwd = 2)

# Add a point for the best threshold
points(youdens_j$specificity, youdens_j$sensitivity, pch = 19, col = "red")

# Adding a legend or text to mark the point
text(youdens_j$specificity, youdens_j$sensitivity, labels = paste("Threshold:", round(optimal_threshold, 2)), pos = 4)

# Add labels and legend
abline(h = 0, v = 1, lty = 2, col = "gray")
legend("topright", legend = paste("AUC =", round(auc(roc_curve), 2)), col = "blue", lwd = 2)

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


# Calculate accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("Accuracy:", round(accuracy, 2), "\n")
