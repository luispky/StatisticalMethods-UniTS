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
                        optimizer = 'efs', 
                        select = TRUE)#, # automatic smoothness selection with CV
                        # method = "REML")

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

# 