# LIBRARIES ------------------------------------------------------------------------------------------------
if(!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)


# LOAD THE DATA---------------------------------------------------------------------------------------------
# Define the path to the datasets
current_path <- dirname(rstudioapi::getActiveDocumentContext()$path)
datasets_dir <- paste(current_path,"datasets", sep = "/")

# Now that the dataframe is stored in an .RData format we can simply load it and the 
# it will have the same name as the filename
load(paste(datasets_dir, "train_df.RData", sep = "/"))

# In case we may want to do some processing to the test dataset later I'll change the name to the dataframe
data <- get("train_df")

# NAME YOUR SECTION AND DOCUMENT YOUR CODE-------------------------------------------------------------------