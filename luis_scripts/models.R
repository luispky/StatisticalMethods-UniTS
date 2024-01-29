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
data_upsampled_balanced <- ROSE(Response ~ ., data = data, seed = 42)$data

# sample_percentage <- 0.50
# sampled_data <- your_data[sample(nrow(your_data), round(sample_percentage * nrow(your_data)), replace = FALSE), ]

data_balanced_sampled <- data_upsampled_balanced[sample(nrow(data_upsampled_balanced), 100000, replace = FALSE), ]

gam_sampled <- gam(Response ~ Gender + Driving_License + Previously_Insured + 
                        Vehicle_Damage + Channels_Reduced + s(log(Age)) +
                        s(Annual_Premium) + Region_Reduced + Vehicle_Age, 
                        data = data_balanced_sampled, family = binomial(), 
                        optimizer = 'efs', select = TRUE)

summary(gam_sampled)



