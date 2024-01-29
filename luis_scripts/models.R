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

test_predictions <- predict(gam_all, newdata = test_data, type = "response") 

roc_curve <- roc(test_data$Response, test_predictions)

auc_score <- auc(roc_curve)
auc_score

#2. Accuracy, Sensitivity, Specificity:
# Traditional metrics like accuracy, sensitivity, and specificity can be useful.
confusion_matrix <- confusionMatrix(predict(gam_all, type = "response") > 0.5, test_data$Response)
accuracy <- confusion_matrix$overall["Accuracy"]
sensitivity <- confusion_matrix$byClass["Sensitivity"]
specificity <- confusion_matrix$byClass["Specificity"]