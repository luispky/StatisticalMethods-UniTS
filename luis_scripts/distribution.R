# LIBRARIES ------------------------------------------------------------------------------------------------
library(mgcViz)
library(ROSE)
library(ggplot2)
library(rlang)
library(mgcv)
library(dplyr)
library(skimr)
library(pROC)
library(caret)
library(patchwork)


# LOAD THE DATA-----------------------------------------------------------------
# Define the path to the datasets
current_path <- dirname(rstudioapi::getActiveDocumentContext()$path)
datasets_dir <- paste(current_path,"../datasets", sep = "/")
datasets_dir

# load(paste(datasets_dir, "train_reduced.RData", sep = "/"))
load(paste(datasets_dir, "train_data.RData", sep = "/"))
load(paste(datasets_dir, "test_data.RData", sep = "/"))
load(paste(datasets_dir, "train_reduced.RData", sep = "/"))

numerical_variables <- names(train_reduced)[sapply(train_reduced, is.numeric)]
categorical_variables <- names(train_reduced)[sapply(train_reduced, is.factor)]
categorical_variables

p1a <- ggplot(train_reduced, aes(x = !!sym(numerical_variables[1]))) + 
  geom_histogram()

p1b <- ggplot(train_data, aes(x = !!sym(numerical_variables[1]))) + 
  geom_histogram()

p1a + p1b

p2a <- ggplot(train_reduced, aes(x = !!sym(numerical_variables[2]))) + 
  geom_histogram()

p2b <- ggplot(train_data, aes(x = !!sym(numerical_variables[2]))) + 
  geom_histogram()

p2a + p2b

p3a <- ggplot(train_reduced, aes(x = !!sym(numerical_variables[3]))) + 
  geom_histogram()

p3b <- ggplot(train_data, aes(x = !!sym(numerical_variables[3]))) + 
  geom_histogram()

p3a + p3b

p4a <- ggplot(train_reduced, aes(x = !!sym(categorical_variables[1]))) + 
  geom_bar()

p4b <- ggplot(train_data, aes(x = !!sym(categorical_variables[1]))) + 
  geom_bar()

p4a + p4b

p5a <- ggplot(train_reduced, aes(x = !!sym(categorical_variables[2]))) + 
  geom_bar()

p5b <- ggplot(train_data, aes(x = !!sym(categorical_variables[2]))) + 
  geom_bar()

p5a + p5b

p6a <- ggplot(train_reduced, aes(x = !!sym(categorical_variables[3]))) + 
  geom_bar()

p6b <- ggplot(train_data, aes(x = !!sym(categorical_variables[3]))) + 
  geom_bar()

p6a + p6b

p7a <- ggplot(train_reduced, aes(x = !!sym(categorical_variables[4]))) + 
  geom_bar()

p7b <- ggplot(train_data, aes(x = !!sym(categorical_variables[4]))) + 
  geom_bar()

p7a + p7b

p8a <- ggplot(train_reduced, aes(x = !!sym(categorical_variables[5]))) + 
  geom_bar()

p8b <- ggplot(train_data, aes(x = !!sym(categorical_variables[5]))) + 
  geom_bar()

p8a + p8b

p9a <- ggplot(train_reduced, aes(x = !!sym(categorical_variables[6]))) + 
  geom_bar()

p9b <- ggplot(train_data, aes(x = !!sym(categorical_variables[6]))) + 
  geom_bar()

p9a + p9b

p10a <- ggplot(train_reduced, aes(x = !!sym(categorical_variables[7]))) + 
  geom_bar()

p10b <- ggplot(train_data, aes(x = !!sym(categorical_variables[7]))) + 
  geom_bar()

p10a + p10b

p11a <- ggplot(train_reduced, aes(x = !!sym(categorical_variables[9]))) + 
  geom_bar()

p11b <- ggplot(train_data, aes(x = !!sym(categorical_variables[9]))) + 
  geom_bar()

p11a + p11b

p12a <- ggplot(train_reduced, aes(x = !!sym(categorical_variables[10]))) + 
  geom_bar()

p12b <- ggplot(train_data, aes(x = !!sym(categorical_variables[10]))) + 
  geom_bar()

p12a + p12b


