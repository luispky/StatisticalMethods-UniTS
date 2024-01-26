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
# train_df <- subset(train_df, select = -id)

# Save as .RData to have consistency when working with the converted datasets later
save(train_df, file = paste(datasets_dir, "train_df.RData", sep = '/'))

# Now that the dataframe is stored in an .RData format we can simply load it and the 
# it will have the same name as the filename

# Do the same for the test dataset
test_df <- read.csv(paste(datasets_dir, "test.csv", sep = "/"), stringsAsFactors = TRUE)
columns_to_convert <- c("Driving_License", "Region_Code",
                        "Previously_Insured", "Policy_Sales_Channel")
test_df[columns_to_convert] <- lapply(test_df[columns_to_convert], as.factor)

# test_df <- subset(test_df, select = -id)

save(test_df, file = paste(datasets_dir, "test_df.RData", sep = '/'))
