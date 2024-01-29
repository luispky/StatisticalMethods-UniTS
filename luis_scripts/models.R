# LIBRARIES ------------------------------------------------------------------------------------------------
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(ROSE)) install.packages("ROSE")
if(!require(mgcViz)) install.packages("mgcViz")
library(mgcViz)
library(ROSE)
library(ggplot2)
library(mgcv)
library(dplyr)
library(skimr)
library(pROC)


# LOAD THE DATA-----------------------------------------------------------------
# Define the path to the datasets
current_path <- dirname(rstudioapi::getActiveDocumentContext()$path)
datasets_dir <- paste(current_path,"../datasets", sep = "/")
datasets_dir

load(paste(datasets_dir, "train_reduced.RData", sep = "/"))

# In case we may want to do some processing to the test dataset later I'll change the name to the dataframe
data <- get("train_reduced")

# MODELING ---------------------------------------------------------------------

# Upsample the minority class to balance the dataset
# data_upsampled_balanced <- ROSE(Response ~ ., data = data, seed = 42)$data

data_upsampled_balanced <- ovun.sample(Response ~ ., data = data, method = "over")$data
skim(data_upsampled_balanced)

sample_percentage <- 0.35
data_balanced_sampled <- data_upsampled_balanced[sample(nrow(data_upsampled_balanced), round(sample_percentage * nrow(data_upsampled_balanced)), replace = FALSE), ]
skim(data_balanced_sampled)

# data_balanced_sampled <- data_upsampled_balanced[sample(nrow(data_upsampled_balanced), 100000, replace = FALSE), ]

gam_sampled <- gam(Response ~ Gender + Driving_License + Previously_Insured + 
                  Vehicle_Damage + Channels_Reduced + s(log(Age)) +
                  s(log(Annual_Premium)) + Region_Reduced + Vehicle_Age, 
                  data = data_balanced_sampled,
                  # te(Age, Annual_Premium),
                  family = binomial(), 
                  optimizer = 'efs', 
                  select = TRUE)#, # automatic smoothness selection with CV
                  # method = "REML") #Restricted Maximum Likelihood estimation for smoother selection
                  # this approach indirectly achieves regularization by selecting the appropriate degrees of freedom for each smoothing term.
summary(gam_sampled)  

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


# Performance Metrics:

load(paste(datasets_dir, "test_reduced.RData", sep = "/"))

#1. Deviance Explained:
# Deviance is a measure of how well the model explains the observed data. Lower deviance indicates a better fit.
# deviance <- summary(gam_sampled)$deviance
# deviance

#2. AUC-ROC:
# For binary classification, the Area Under the Receiver Operating Characteristic Curve (AUC-ROC) measures the model's ability to discriminate between classes.

test_predictions <- predict(gam_sampled, newdata = test_reduced, type = "response") 

roc_curve <- roc(train_reduced$Response, predict(gam_sampled, newdata = test_reduced, type = "response"))

auc_score <- auc(roc_curve)

?roc

#3. Accuracy, Sensitivity, Specificity:
# Traditional metrics like accuracy, sensitivity, and specificity can be useful.
# library(caret)
# confusion_matrix <- confusionMatrix(predict(gam_sampled, type = "response") > 0.5, train_df$Response)
# accuracy <- confusion_matrix$overall["Accuracy"]
# sensitivity <- confusion_matrix$byClass["Sensitivity"]
# specificity <- confusion_matrix$byClass["Specificity"]