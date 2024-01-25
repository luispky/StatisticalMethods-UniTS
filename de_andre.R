getwd()
setwd("Desktop/uni/statistical_methods/final_project/stats_project")

library(ggplot2); library(car); library(mgcv); library(skimr); library(viridis)
library(psych); library(gridExtra)

install.packages("viridis")

df <- read.csv('train.csv')
last_col <- ncol(df)

# Reorder the columns
new_order <- c(last_col, 1:(last_col - 1))
df <- df[, new_order]

###### Data Exploration ######
#View(df)
#dim(df)---> dimension=381109x12
describe(df) # asterisk means categorical vars converted to num for description
#head(df)
barplot(table(df$Response))
table(df$Driving_License)
table(df$Previously_Insured)

#######################################################################à

### gender -> binary
### age -> numerical
### driving license -> binary
### region code -> classification? 52
### previously insured -> binary 
### veichle age -> categorical
### vehicle damage -> binary
### annual premium -> numerical
### policy sales channel -> categorical
### vintage -> numerical (count?)
### response -> binary


#gender_bar <- ggplot(df, aes(x = Gender, fill = "Blue")) +
#  geom_bar(color = "black", size = 0.2) +
#  labs(title = "Barplot of Gender",
#       x = "GENDER",
#       y = "Count") +
#  scale_fill_manual(values = c("Blue" = "lightblue"))

df$Annual_Premium <- log(df$Annual_Premium)

library(dplyr) # from version 1.0.0 

df %>%
  relocate(Response) %>%
  head()
### BARPLOTS
names = colnames(df)[! (colnames(df) %in% c("id", "Age", "Region_Code", "Annual_Premium", "Policy_Sales_Channel", "Vintage")) ]
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
plots <- simple_bars(df, names)
grid.arrange(grobs = plots, nrow = 2, ncol=3)


gender_bar <- ggplot(df, aes(x = Gender, fill = "Blue")) +
  geom_bar(color = "black", size = 0.2) +
  labs(title = "Barplot of Gender",
       x = "GENDER",
       y = "") +
  scale_fill_manual(values = c("Blue" = "lightblue"))

colnames(df)

### BARPLOTS with predictors

plot_list <- list()
for (col_name in names) {
  # Create a bar plot for the current column
  bar_plot_resp <- ggplot(df, aes(x = !!sym(col_name), fill = as.factor(Response))) +
    geom_bar(position = "dodge") +
    labs(title = paste("Barplot of ", col_name, "vs. Response"),
         x = col_name,
         y = "",
         fill = "Response") +
    scale_x_discrete(labels = function(x) as.character(x)) +
    scale_fill_manual(values = c("0" = "aquamarine", "1" = "coral"))
  
  # Append the bar plot to the list
  plot_list[[col_name]] <- bar_plot_resp
}
grid.arrange(grobs = plot_list, nrow = 4, ncol=3)


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
