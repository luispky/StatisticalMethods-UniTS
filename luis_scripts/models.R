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

load(paste(datasets_dir, "train_reduced.RData", sep = "/"))

# In case we may want to do some processing to the test dataset later I'll change the name to the dataframe
data <- get("train_reduced")

# BALANCE THE DATASET ----------------------------------------------------------
# and train/test split

# Upsample the minority class to balance the dataset
# data_upsampled_balanced <- ROSE(Response ~ ., data = data, seed = 42)$data

data_upsampled_balanced <- ovun.sample(Response ~ ., data = data, method = "over")$data
skim(data_upsampled_balanced)

# Percentage of the data to be used for training
sample_percentage <- 0.7

# Sample a subset of the data
sampled_rows <- sample(nrow(data_upsampled_balanced), round(sample_percentage * nrow(data_upsampled_balanced)), replace = FALSE)

# Create the training set (the sampled rows)
data_balanced_train <- data_upsampled_balanced[sampled_rows, ]
skim(data_balanced_train)

# Create the test set (the remaining rows)
data_balanced_test <- data_upsampled_balanced[-sampled_rows, ]
skim(data_balanced_test)

# MODELING ---------------------------------------------------------------------

# data_balanced_sampled <- data_upsampled_balanced[sample(nrow(data_upsampled_balanced), 100000, replace = FALSE), ]

gam_sampled <- gam(Response ~ Gender + Driving_License + Previously_Insured + 
                  Vehicle_Damage + Channels_Reduced + s(log(Age)) +
                  s(log(Annual_Premium)) + Region_Reduced + Vehicle_Age, 
                  data = data_balanced_train,
                  # te(Age, Annual_Premium),
                  family = binomial(), 
                  optimizer = 'efs', 
                  select = TRUE)#, # automatic smoothness selection with CV
                  # method = "REML") #Restricted Maximum Likelihood estimation for smoother selection
                  # this approach indirectly achieves regularization by selecting the appropriate degrees of freedom for each smoothing term.
summary(gam_sampled)  

# Performance Metrics:

#1. AUC-ROC:
# For binary classification, the Area Under the Receiver Operating Characteristic Curve (AUC-ROC) measures the model's ability to discriminate between classes.

test_predictions <- predict(gam_sampled, newdata = data_balanced_test, type = "response") 

roc_curve <- roc(data_balanced_test$Response, test_predictions)

auc_score <- auc(roc_curve)
auc_score

#2. Accuracy, Sensitivity, Specificity:
# Traditional metrics like accuracy, sensitivity, and specificity can be useful.
confusion_matrix <- confusionMatrix(predict(gam_sampled, type = "response") > 0.5, data_balanced_test$Response)
accuracy <- confusion_matrix$overall["Accuracy"]
sensitivity <- confusion_matrix$byClass["Sensitivity"]
specificity <- confusion_matrix$byClass["Specificity"]