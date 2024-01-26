getwd()
setwd("Desktop/uni/statistical_methods/final_project/stats_project")

##### Libraries and preliminaries #####
setwd("Desktop/uni/statistical_methods/final_project/stats_project")

library(ggplot2); library(car); library(mgcv); library(skimr); library(viridis)
library(psych); library(gridExtra); library(dplyr); library(skimr)

df <- read.csv('datasets/train.csv') 
last_col <- ncol(df)

# Reorder the columns
new_order <- c(last_col, 1:(last_col - 1))
df <- df[, new_order]

###### Data Exploration ######
#View(df)
#dim(df)---> dimension=381109x12
#describe(df) # asterisk means categorical vars converted to num for description
#head(df)
skim(df)
summary(df)

### gender               -> binary
### age                  -> numerical
### driving license      -> binary
### region code          -> classification? 52
### previously insured   -> binary 
### vehicle age          -> categorical
### vehicle damage       -> binary
### annual premium       -> numerical
### policy sales channel -> categorical
### vintage              -> numerical
### response             -> binary

# shall we convert to numbers?
df$Gender <- as.factor(df$Gender)
df$Driving_License <- as.factor(df$Driving_License)
df$Region_Code <- as.factor(df$Region_Code)
df$Previously_Insured <- as.factor(df$Previously_Insured)
df$Vehicle_Age <- as.factor(df$Vehicle_Age)
df$Vehicle_Damage <- as.factor(df$Vehicle_Damage)
df$Policy_Sales_Channel <- as.factor(df$Policy_Sales_Channel)
df$Response <- as.factor(df$Response)


#######################################################################à

barplot(table(df$Response))
table(df$Driving_License)
table(df$Previously_Insured)


#gender_bar <- ggplot(df, aes(x = Gender, fill = "Blue")) +
#  geom_bar(color = "black", size = 0.2) +
#  labs(title = "Barplot of Gender",
#       x = "GENDER",
#       y = "Count") +
#  scale_fill_manual(values = c("Blue" = "lightblue"))


plot(df$Age, df$Annual_Premium)
prop.table(table(df$Response, df$Vehicle_Age), margin=1)


mosaicplot(table(df$Response, df$Vehicle_Age), margin=1)
### BARPLOTS
names = colnames(df)[! (colnames(df) %in% c("id", "Age", "Region_Code", "Annual_Premium", "Policy_Sales_Channel", "Vintage")) ]
names2 <- colnames(df)[(colnames(df) %in% c("Age", "Region_Code", "Policy_Sales_Channel", "Vintage")) ]
# function to create the simplest 
simple_bars <- function(data, names){
  plot_list <- list()
# Iterate through the columns of the data frame
  for (col_name in colnames(data)) {
    
    # Create a bar plot for the current column
    bar_plot <- ggplot(data, aes(x = !!sym(col_name), fill = "Blue")) +
      geom_bar(color = "black", size = 0.2) +
      labs(title = paste("Barplot of", col_name),
           x = col_name,
           y = "") +
      scale_fill_manual(values = c("Blue" = "lightblue"))
    
    # Append the bar plot to the list
    plot_list[[col_name]] <- bar_plot
  }
  return(plot_list)
}
plots <- simple_bars(df, names2)
grid.arrange(grobs = plots, nrow = 2, ncol=2)


gender_bar <- ggplot(df, aes(x = Gender, fill = "Blue")) +
  geom_bar(color = "black", size = 0.2) +
  labs(title = "Barplot of Gender",
       x = "GENDER",
       y = "") +
  scale_fill_manual(values = c("Blue" = "lightblue"))

colnames(df)

### BARPLOTS with predictors

names <- colnames(df)[! (colnames(df) %in% c("Response", "id", "Age", "Region_Code", "Annual_Premium", "Policy_Sales_Channel", "Vintage")) ]

resp_bars <- function(data, names) {
  plot_list <- list()
  for (col_name in names) {
    # Create a bar plot for the current column
    bar_plot_resp <- ggplot(data, aes(x = !!sym(col_name), fill = as.factor(Response))) +
      geom_bar(position = "dodge") +
      labs(#title = paste("Barplot of ", col_name, "vs. Response"),
           x = col_name,
           y = "",
           fill = "Response") +
      scale_x_discrete(labels = function(x) as.character(x)) +
      scale_fill_manual(values = c("0" = "gray50", "1" = "coral"))
    
    # Append the bar plot to the list
    plot_list[[col_name]] <- bar_plot_resp
  }
  return(plot_list)
}

plot_list <- resp_bars(df, names)

# Display the plots in a grid
grid.arrange(grobs = plot_list, nrow = 2, ncol = 3)


## try different barplots
fill_patterns <- c(1, 40)
par(mfrow = c(1, 2))
barplot(prop.table(table(df$Response, df$Age), margin = 1),
        beside = TRUE, legend.text = c("No", "Yes"), xlab = "Age (years)", col = c("gray90", "blue"), density = fill_patterns)
barplot(prop.table(table(df$Response, df$Age), margin = 2), , col = c("gray90", "blue"), density = fill_patterns)
        beside = TRUE, legend.text = c("No", "Yes"), xlab = "Age (years)")


# histograms for numerical
hist(df$Age)

ggplot(df, aes(x = Annual_Premium, fill = as.factor(Response))) +
  geom_bar(position = "dodge") +
  labs(title = "Barplot of Gender vs. Response",
       x = "Gender",
       y = "Proportion",
       fill = "Response") +
  scale_x_discrete(labels = function(x) as.character(x)) +
  scale_fill_manual(values = c("0" = "aquamarine", "1" = "coral"))

ggplot(df, aes(x = Gender, y = Response, fill = "Blue")) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Barplot of Gender with Binary Response",
       x = "Gender",
       y = "Response") +
  scale_fill_manual(values = c("Blue" = "lightblue"))


library(dplyr)

percentage_male <- mean(df$Response[df$Gender == "Male"]) * 100
percentage_female <- mean(df$Response[df$Gender == "Female"]) * 100

# Create a data frame for the bar plot
percentage_data <- data.frame(Gender = c("Male", "Female"),
                              Percentage = c(percentage_male, percentage_female))

# Create the bar plot
ggplot(percentage_data, aes(x = Gender, y = Percentage, fill = Gender)) +
  geom_bar(stat = "identity") +
  labs(title = "Percentage of Response by Gender",
       x = "Gender",
       y = "Percentage") +
  scale_y_continuous(labels = scales::percent_format(scale = 1))

## Binary trasformations
df$GenderB <- ifelse(df$Gender=="Female",1,0)
df$Vehicle_DamageB <- ifelse(df$Vehicle_Damage=="Yes",1,0)
df$Vehicle_AgeB <- ifelse(df$Vehicle_Age=="< 1 Year",0,
                          ifelse(df$Vehicle_Age=="1-2 Year", 1, 2))
dfb = subset(df, select =-c(1,2,7,8))
library(corrplot)
corrplot(cor(dfb), method="number")
#library(ggplot2)
hist(dfb$Age)
hist(dfb$Annual_Premium)
ggplot(dfb, aes(x = factor(Previously_Insured))) +
geom_bar()
ggplot(Data, aes(x = factor(Gender))) +
#geom_bar()
#ggplot(dfb, aes(x = factor(Vehicle_DamageB))) +
#geom_bar()
#ggplot(Data, aes(x = factor(Vehicle_Age))) +
#geom_bar()
#ggplot(dfb, aes(x = factor(Response))) +
#geom_bar()


hist(log(df$Age))
hist(df$Annual_Premium)
ggplot(df, aes(x = factor(Previously_Insured))) +
geom_bar()
ggplot(df, aes(x = factor(Gender))) +
geom_bar()
ggplot(dfB, aes(x = factor(Vehicle_DamageB))) +
geom_bar()
ggplot(df, aes(x = factor(Vehicle_Age))) +
geom_bar()
ggplot(dfB, aes(x = factor(Response))) +
geom_bar()

par(mfrow = c(1, 2))
boxplot(Age ~ Response, df = df, horizontal = TRUE,
        xlab = "Age", ylab = "Response")
barplot(prop.table(table(df$Driving_License, df$Age), margin = 1),
        beside = TRUE, legend.text = c("Yes", "No"), xlab = "Driving License")


df2 = subset(df, Driving_License=1)

factor_columns <- sapply(df, is.factor)
factor_variable_names <- names(factor_columns[factor_columns])
par(mfrow = c(1, length(factor_variable_names)))  # Set up a multi-panel plot
for (col in factor_variable_names) {
  barplot(table(df[[col]]), main = col, xlab = col, ylab = "Frequency")
}

ggplot(df, aes(x = as.factor(CountVar), fill = BinaryVar)) +
  geom_bar(position = "dodge") +
  labs(title = "Frequency of Count Variable by Binary Category", x = df$Age, y = df$Response) +
  scale_x_discrete(name = "Count Variable") +
  scale_fill_discrete(name = "Binary Variable")
ggplot(df=df, aes(x = Age, y=Response, fill = Response)) +
  geom_col(position="dodge"
           , color='gray50', width=0.7) +
  scale_fill_viridis_d())


ggplot(df=df, aes(x = Age, fill = factor(Response))) +
  geom_bar() +
  #scale_fill_viridis(discrete = TRUE) +
  scale_fill_manual(values = c("0" = "orange", "1" = "green")) +
  labs(title = "Barplot of Binary Variable",
       x = "Response",
       y = "Count")



###### Models ######

model.log <- glm(df$Response ~., data=df, family = binomial(link = "logit"))
summary(model.log)
model.log2 <- glm(df$Response ~.-id-Region_Code-Vintage, data=df, family = binomial(link = "logit"))
summary(model.log2)
AIC(model.log, model.log2)
