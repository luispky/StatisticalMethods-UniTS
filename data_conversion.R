library(dplyr)

# Get the directory of the current script is (the directory of the project) and
# concatenate it with the name of the folder with the datasets 
datasets_dir <- paste(dirname(rstudioapi::getActiveDocumentContext()$path),"datasets", sep = "/")
datasets_dir

# Load the dataset with `stringsAsFactors = TRUE` takes care of the categorical variables as string 
# to de directly imported as factors
train_df <- read.csv(paste(datasets_dir, "train.csv", sep = "/"), stringsAsFactors = TRUE)
columns_to_convert <- c("Driving_License", "Region_Code",
                        "Previously_Insured", "Policy_Sales_Channel",
                        "Response")
# Convert categorical variables to factors
train_df[columns_to_convert] <- lapply(train_df[columns_to_convert], as.factor)

# Drop the 'id' column
train_df <- subset(train_df, select = -id)

# Save as .RData to have consistency when working with the converted datasets later
save(train_df, file = paste(datasets_dir, "train_df.RData", sep = '/'))

# Now that the dataframe is stored in an .RData format we can simply load it and the 
# it will have the same name as the filename

# Do the same for the test dataset
test_df <- read.csv(paste(datasets_dir, "test.csv", sep = "/"), stringsAsFactors = TRUE)
columns_to_convert <- c("Driving_License", "Region_Code",
                        "Previously_Insured", "Policy_Sales_Channel")
test_df[columns_to_convert] <- lapply(test_df[columns_to_convert], as.factor)

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
  head(10)

data <- data %>% mutate(Channels_Reduced = case_when(Policy_Sales_Channel %in% data_summary$Policy_Sales_Channel[1:4] ~ Policy_Sales_Channel, TRUE ~ as.factor("0")))

# Reduce Region_Code
data_summary <- data %>%
  group_by(Region_Code) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Percentage = Count / sum(Count)) %>%
  arrange(desc(Percentage)) %>%
  head(10)
data <- data %>% mutate(Region_Reduced = case_when(Region_Code %in% data_summary$Region_Code[1:4] ~ Region_Code, TRUE ~ as.factor("0")))


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
  head(10)

data <- data %>% mutate(Channels_Reduced = case_when(Policy_Sales_Channel %in% data_summary$Policy_Sales_Channel[1:4] ~ Policy_Sales_Channel, TRUE ~ as.factor("0")))

# Reduce Region_Code
data_summary <- data %>%
  group_by(Region_Code) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Percentage = Count / sum(Count)) %>%
  arrange(desc(Percentage)) %>%
  head(10)
data <- data %>% mutate(Region_Reduced = case_when(Region_Code %in% data_summary$Region_Code[1:4] ~ Region_Code, TRUE ~ as.factor("0")))

# New proportions of the categories

data_summary <- data %>%
  group_by(Region_Reduced) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Percentage = Count / sum(Count)) %>%
  arrange(desc(Percentage)) 

data_summary <- data %>%
  group_by(Channels_Reduced) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Percentage = Count / sum(Count)) %>%
  arrange(desc(Percentage)) 

test_reduced <- data

test_reduced <- select(test_reduced, -Policy_Sales_Channel, -Region_Code)
save(test_reduced, file = paste(datasets_dir, "test_reduced.RData", sep = '/'))


# NOW WE ARE ABLE TO LOAD THE DATASETS IN THE FOLLOWING WAY
# load(paste(datasets_dir, "train_reduced.RData", sep="/"))
# WE CAN USE THE DATASET WITH THE NAME IT ALREADY HAS AND WE CAN USE IT LIKE THIS:
# skim(train_reduced)
# WE CAN RENAME IT USING THE FOLLOWING SYNTAX:
# df <- get("train_reduced")