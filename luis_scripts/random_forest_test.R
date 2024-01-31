install.packages("randomForest")
library(randomForest)

current_path <- dirname(rstudioapi::getActiveDocumentContext()$path)
datasets_dir <- paste(current_path,"../datasets", sep = "/")
datasets_dir

# load(paste(datasets_dir, "train_reduced.RData", sep = "/"))
load(paste(datasets_dir, "train_data.RData", sep = "/"))
load(paste(datasets_dir, "test_data.RData", sep = "/"))
train_data <- select(train_data, -Policy_Sales_Channel, -Region_Code)
test_data <- select(test_data, -Policy_Sales_Channel, -Region_Code)

# Your formula for the model
formula <- as.formula(paste("Response ~ ."))

# Train the Random Forest model
rf_model <- randomForest(formula, data = train_data)
