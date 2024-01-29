library(dplyr)
library(forcats)

# Get the directory of the current script is (the directory of the project) and
# concatenate it with the name of the folder with the datasets 
datasets_dir <- paste(dirname(rstudioapi::getActiveDocumentContext()$path),"../datasets", sep = "/")
datasets_dir

# Load the dataset with `stringsAsFactors = TRUE` takes care of the categorical variables as string 
# to de directly imported as factors
train_df <- read.csv(paste(datasets_dir, "train.csv", sep = "/"), stringsAsFactors = TRUE)
columns_to_convert <- c("Driving_License", "Region_Code",
                        "Previously_Insured", "Policy_Sales_Channel",
                        "Response")
# Convert categorical variables to factors
train_df[columns_to_convert] <- lapply(train_df[columns_to_convert], as.factor)

# Recode the 'Response' variable to have the levels as "No" and "Yes"
train_df$Response <- fct_recode(train_df$Response, "No" = "0", "Yes" = "1")
# Recode the 'Driving_License' variable to have the levels as "No" and "Yes"
train_df$Driving_License <- fct_recode(train_df$Driving_License, "No" = "0", "Yes" = "1")
# Recode the 'Previously_Insured' variable to have the levels as "No" and "Yes"
train_df$Previously_Insured <- fct_recode(train_df$Previously_Insured, "No" = "0", "Yes" = "1")

# Ensure that the 'Age' variable is an integer
train_df$Age <- as.integer(train_df$Age)
# Ensure that the 'Annual_Premium' variable is numeric
train_df$Annual_Premium <- as.numeric(train_df$Annual_Premium)
# Ensure that the 'Vintage' variable is an integer
train_df$Vintage <- as.integer(train_df$Vintage)

your_data$numeric_variable <- as.integer(your_data$numeric_variable)


# Drop the 'id' column
train_df <- subset(train_df, select = -id)

skim(train_df)

# Save as .RData to have consistency when working with the converted datasets later
save(train_df, file = paste(datasets_dir, "train_df.RData", sep = '/'))

# Now that the dataframe is stored in an .RData format we can simply load it and the 
# it will have the same name as the filename

# Do the same for the test dataset
test_df <- read.csv(paste(datasets_dir, "test.csv", sep = "/"), stringsAsFactors = TRUE)
columns_to_convert <- c("Driving_License", "Region_Code",
                        "Previously_Insured", "Policy_Sales_Channel")
test_df[columns_to_convert] <- lapply(test_df[columns_to_convert], as.factor)

# Recode the 'Driving_License' variable to have the levels as "No" and "Yes"
test_df$Driving_License <- fct_recode(test_df$Driving_License, "No" = "0", "Yes" = "1")
# Recode the 'Previously_Insured' variable to have the levels as "No" and "Yes"
test_df$Previously_Insured <- fct_recode(test_df$Previously_Insured, "No" = "0", "Yes" = "1")

# Ensure that the 'Age' variable is an integer
test_df$Age <- as.integer(test_df$Age)
# Ensure that the 'Annual_Premium' variable is numeric
test_df$Annual_Premium <- as.numeric(test_df$Annual_Premium)
# Ensure that the 'Vintage' variable is an integer
test_df$Vintage <- as.integer(test_df$Vintage)

# Drop the 'id' column
test_df <- subset(test_df, select = -id)

save(test_df, file = paste(datasets_dir, "test_df.RData", sep = '/'))

# Reduced dataframe-----

# TRAIN DATASET
data <- train_df

# Reduce Policy_Sales_Channel
data_summary <- data %>%
  group_by(Policy_Sales_Channel) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Percentage = Count / sum(Count)) %>%
  arrange(desc(Percentage)) %>%
  head(4) # Select the top 4 categories
data_summary

# Convert factor to character
selected_channels <- as.character(data_summary$Policy_Sales_Channel[1:4])

# Reduce the levels of Policy_Sales_Channel in the dataframe
data$Channels_Reduced <- fct_other(data$Policy_Sales_Channel, keep = selected_channels, other_level = "0")

# Print the summary of the new variable
summary(data$Channels_Reduced)

# Reduce Region_Code
data_summary_region <- data %>%
  group_by(Region_Code) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Percentage = Count / sum(Count)) %>%
  arrange(desc(Percentage)) %>%
  head(4)  # Select the top 4 categories

# Convert factor to character
selected_regions <- as.character(data_summary_region$Region_Code[1:4])

# Reduce the levels of Region_Code in the dataframe
data$Region_Reduced <- fct_other(data$Region_Code, keep = selected_regions, other_level = "0")

# Print the summary of the new variable
summary(data$Region_Reduced)

str(data)

# New proportions of the categories

data_summary <- data %>%
  group_by(Region_Reduced) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Percentage = Count / sum(Count)) %>%
  arrange(desc(Percentage)) 
data_summary

data_summary <- data %>%
  group_by(Channels_Reduced) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Percentage = Count / sum(Count)) %>%
  arrange(desc(Percentage)) 
data_summary

train_reduced <- data

train_reduced <- select(train_reduced, -Policy_Sales_Channel, -Region_Code)
save(train_reduced, file = paste(datasets_dir, "train_reduced.RData", sep = '/'))

# TEST DATASET

data <- test_df

# Reduce Policy_Sales_Channel
data_summary <- data %>%
  group_by(Policy_Sales_Channel) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Percentage = Count / sum(Count)) %>%
  arrange(desc(Percentage)) %>%
  head(4) # Select the top 4 categories
data_summary

# Convert factor to character
selected_channels <- as.character(data_summary$Policy_Sales_Channel[1:4])

# Reduce the levels of Policy_Sales_Channel in the dataframe
data$Channels_Reduced <- fct_other(data$Policy_Sales_Channel, keep = selected_channels, other_level = "0")

# Print the summary of the new variable
summary(data$Channels_Reduced)

# Reduce Region_Code
data_summary_region <- data %>%
  group_by(Region_Code) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Percentage = Count / sum(Count)) %>%
  arrange(desc(Percentage)) %>%
  head(4)  # Select the top 4 categories

# Convert factor to character
selected_regions <- as.character(data_summary_region$Region_Code[1:4])

# Reduce the levels of Region_Code in the dataframe
data$Region_Reduced <- fct_other(data$Region_Code, keep = selected_regions, other_level = "0")

# Print the summary of the new variable
summary(data$Region_Reduced)

str(data)

test_reduced <- data

test_reduced <- select(test_reduced, -Policy_Sales_Channel, -Region_Code)
save(test_reduced, file = paste(datasets_dir, "test_reduced.RData", sep = '/'))


# NOW WE ARE ABLE TO LOAD THE DATASETS IN THE FOLLOWING WAY
# -----------------IF YOU ARE IN THE ROOT DIRECTORY OF THE PROJECT----------------------------------
# THE FOLLOWING LINE ALLOW YOU TO GET THE DIRECTORY OF THE DATASETS AND THEN YOU CAN PROCEED SMOOTHLY AS FOLLOWS 
# datasets_dir <- paste(dirname(rstudioapi::getActiveDocumentContext()$path),"datasets", sep = "/")
# load(paste(datasets_dir, "train_reduced.RData", sep="/"))
# WE CAN USE THE DATASET WITH THE NAME IT ALREADY HAS AND WE CAN USE IT LIKE THIS:
# skim(train_reduced)
# WE CAN RENAME IT USING THE FOLLOWING SYNTAX:
# df <- get("train_reduced")