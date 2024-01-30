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
train_data <- select(train_data, -Policy_Sales_Channel, -Region_Code)
test_data <- select(test_data, -Policy_Sales_Channel, -Region_Code)

#### VARIABLES IMPORTANCE ####

# Sort variables by importance wrt AIC
# We remove one variable at a time and by decreasing AIC we get the most important variables
# i.e., the variables that when removed increase the AIC is important
predictors <- colnames(train_data)
names <- names[names != 'Response']
ranking_variables_models <- list()

for (predictor in predictors){
  if (predictor == "Age"){
    formula_string <- paste0("Response ~ . - Annual_Premium + I(log(Annual_Premium)) -", predictor)
  } else if (predictor == "Annual_Premium"){
    formula_string <- paste0("Response ~ . - Age + I(log(Age)) -", predictor)
  } else {
    formula_string <- paste0("Response ~ . - Age - Annual_Premium + I(log(Age)) + I(log(Annual_Premium)) - ", predictor)
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
df_sorted_ranking_variables_aic

# Compute nested models
variables_order <- df_sorted_ranking_variables_aic$VariableRemoved
variables_order
variables_nested <- c()
nest_models <- list()

for (variable in variables_order) {
  print(paste('variable:', variable))
  variables_nested <- c(variables_nested, variable)
  print(variables_nested)
  if (predictor == "Age"){
    formula_str <- paste("Response", "~", paste(variables_nested, collapse = " + "))


    formula_string <- paste0("Response ~ . - Annual_Premium + I(log(Annual_Premium)) -", predictor)
  } else if (predictor == "Annual_Premium"){
    formula_str <- paste("Response", "~", paste(variables_nested, collapse = " + "))
  
  
    formula_string <- paste0("Response ~ . - Age + I(log(Age)) -", predictor)
  } else {
    formula_str <- paste("Response", "~", paste(variables_nested, collapse = " + "))
    
    
    formula_string <- paste0("Response ~ . - Age - Annual_Premium + I(log(Age)) + I(log(Annual_Premium)) - ", predictor)
  }
  model_formula <- as.formula(formula_string)
  model <- glm(model_formula, data = train_data, family = binomial)



  formula <- as.formula(formula_str)
  model <- glm(formula, data = train_data, family = binomial)
  nest_models[[variable]] <- model
}
aic_values2 <- sapply(nest_models, AIC)
df_aic2 <- data.frame(Model_Name = variables_order, AIC = aic_values2)
df_aic2
# Assuming df is your DataFrame
df_sorted2 <- df_aic2[order(df_aic2$AIC, decreasing=TRUE), ]
df_sorted2

#********************************************************
#* INCLUDE anova ANALYSIS TO CHECK FOR SIGNIFICANT VARS *
#* INCLUDE vif ANALYSIS TO CHECK FOR MULTICOLLINEARITY  *
#******************************************************** 
