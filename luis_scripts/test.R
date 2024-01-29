datasets_dir <- paste(dirname(rstudioapi::getActiveDocumentContext()$path),"../datasets", sep = "/")
datasets_dir

load(paste(datasets_dir, "train_df.RData", sep = "/"))
load(paste(datasets_dir, "train_reduced.RData", sep = "/"))
train_reduced
library(skimr)
skim(train_reduced)
str(train_reduced)
