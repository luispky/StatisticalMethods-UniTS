getwd()
setwd("Desktop/uni/statistical_methods/final_project/stats_project")

##### Libraries and preliminaries #####
setwd("Desktop/uni/statistical_methods/final_project/stats_project")

load(file='datasets/test_data.RData')
load(file='datasets/train_data.RData')

library(ggplot2); library(car); library(mgcv); library(skimr); library(viridis)
library(psych); library(gridExtra); library(dplyr); library(skimr)

hist(df$Policy_Sales_Channel)

df.raw <- df
df <- read.csv('datasets/train.csv')
df_test <- read.csv('datasets/test.csv')
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
df.raw = df
# shall we convert to numbers?
df$Gender <- as.factor(df$Gender)
df$Driving_License <- as.factor(df$Driving_License)
df$Region_Code <- as.factor(df$Region_Code)
df$Previously_Insured <- as.factor(df$Previously_Insured)
df$Vehicle_Age <- as.factor(df$Vehicle_Age)
df$Vehicle_Damage <- as.factor(df$Vehicle_Damage)
df$Policy_Sales_Channel <- as.factor(df$Policy_Sales_Channel)
df$Response <- as.factor(df$Response)




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
barplot(prop.table(table(df$Response, df$Policy_Sales_Channel), margin = 1),
        beside = TRUE, legend.text = c("No", "Yes"), xlab = "Age (years)", col = c("gray90", "blue"), density = fill_patterns)
barplot(prop.table(table(df$Response, df$Region_Code), margin = 2), , col = c("gray90", "blue"), density = fill_patterns)
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



##### MODELS #####
model.glm.raw <- glm(df.raw$Response ~., data=df.raw, family = binomial(link = "logit"))
model.glm.log <- glm(df.raw$Response ~.-Annual_Premium+I(Annual_Premium^-1), data=df.raw, family = binomial(link = "logit"))
summary(model.glm.log)
model.glm.sq <- glm(df.raw$Response ~.-Age+I(Age^2), data=df.raw, family = binomial(link = "logit"))
AIC(model.glm, model.glm.log, model.glm.sq)

model.glm <- glm(df$Response ~., data=df, family = binomial(link = "logit"))
summary(model.glm)
par(mfrow=c(2,2))
plot(model.glm)

# cooks distance outliers analysis
cooks.distance <- cooks.distance(model.glm)
plot(cooks.distance, pch="*", main="Cook's distance")
abline(h=0.001, col="red") # Points above this line are influential



model.glm <- glm(Response ~. -Annual_Premium + I(1/Annual_Premium), data=df, family = binomial(link = "logit"))
summary(model.log)
pchisq(model.log$null.deviance - model.log$deviance, model.log$df.null-model.log$df.residual, lower.tail=FALSE)
plot(model.glm)


# residual analysis


#### RESIDUAL ANALYSIS####

residuals_values <- residuals(model.glm, type = "response")
df$residuals <- residuals_values
hist(df$residuals)


# Subset the dataframe where residuals are greater than 10
k = 0.5
par(mfrow=c(2,2))
dfr <- df[abs(df$residuals) > k, ]
dim(dfr)
hist(dfr$Age)
fill_patterns <- c(1, 40)
barplot(prop.table(table(dfr$Response, dfr$Age), margin = 2), col = c("gray90", "blue"), density = fill_patterns)
  beside = TRUE, legend.text = c("No", "Yes"), xlab = "Age (years)")

dfr2 <- df[abs(df$residuals) < k, ]
dim(dfr)
hist(dfr2$Age)
fill_patterns <- c(1, 40)
barplot(prop.table(table(dfr2$Response, dfr2$Age), margin = 2), col = c("gray90", "blue"), density = fill_patterns)
beside = TRUE, legend.text = c("No", "Yes"), xlab = "Age (years)")


bar_plot_resp <- ggplot(data=dfr, aes(x = Gender, fill = as.factor(Response))) +
  geom_bar(position = "dodge") +
  labs(#title = paste("Barplot of ", col_name, "vs. Response"),
    x = "name",
    y = "",
    fill = "Response") +
  scale_x_discrete(labels = function(x) as.character(x)) +
  scale_fill_manual(values = c("0" = "gray50", "1" = "coral"))
bar_plot_resp


AIC(model.log.raw, model.log)

model.log.red <- glm(df$Response ~.-id, data=df, family = binomial(link = "logit"))
AIC(model.log.raw, model.log, model.log.red)
summary(model.log.red)

hist(log(df$Age))

#### BINNED RESIDUALS ####
pred <- predict(model.glm, type="response")
res <- residuals(model.glm, type="response")
par(mfrow=c(1,1))
plot(c(0,1),c(-1,1), xlab = "Estimated Pr(Switching)", type = "n",
     ylab = "Observed - Estimated", main = "Residual Plot")
abline(h = 0, col = "gray", lwd = 0.5)
points(pred, res, pch = 20, cex = 0.2)
abline(c(0,-1), col = "red", lwd = 0.25)
abline(c(1,-1), col = "red", lwd = 0.25)

library(arm)
binned.resids <- function(x, y, nclass = sqrt(length(x))){
  breaks.index <- floor(length(x) * (1 : (nclass))/nclass)
  breaks <- c(-Inf, sort(x)[breaks.index], Inf)
  output <- NULL
  xbreaks <- NULL
  x.binned <- as.numeric(cut(x, breaks))
  for(i in 1:nclass){
    items <- (1:length(x))[x.binned == i]
    x.range <- range(x[items])
    xbar <- mean(x[items])
    ybar <- mean(y[items])
    n <- length(items)
    sdev <- sd(y[items])
    output <- rbind(output, c(xbar, ybar, n, x.range, 2 * sdev/sqrt(n)))
  }
  colnames(output) <- c("xbar", "ybar", "n", "x.lo", "x.hi", "2se")
  return(list(binned = output, xbreaks = xbreaks))
}

br7 <- binned.resids(pred, res, nclass = 50)$binned
plot(range(br7[,1]), range(br7[,2],br7[,6], -br7[,6]), type = "n",
     xlab = "Estimated Pr(Switching)", ylab = "Average residual", main = "Binned Residual Plot")
abline(h = 0, col = "gray", lwd = 0.5); points(br7[,1], br7[,2], pch = 19, cex = 0.5)
lines(br7[,1], br7[,6], col = "gray", lwd = 0.5); lines(br7[,1], -br7[,6], col = "gray", lwd = 0.5)

par(mfrow = c(1, 2))
br.age <- binned.resids(data$age, res,
                         nclass = 50)$binned
plot(range(br.age[,1]), range(br.age[,2], br.age[,6], -br.age[,6]), type = "n",
     xlab = "Age", ylab = "Average residual", main = "Binned residual plot")
abline(h = 0, col = "gray", lwd = 0.5)
lines(br.dist[,1], br.dist[,6], col = "gray", lwd = 0.5)
lines(br.dist[,1], -br.dist[,6], col = "gray", lwd = 0.5)
points(br.dist[,1], br.dist[,2], pch = 19, cex = 0.5)
br.arsenic <- binned.resids(data$arsenic, res7,
                            nclass = 40)$binned
plot(range(br.arsenic[,1]), range(br.arsenic[,2], br.arsenic[,6], -br.arsenic[,6]), type = "n",
     xlab = "Arsenic concentration", ylab = "Average residual", main = "Binned residual plot")
abline(h = 0, col = "gray", lwd = 0.5)
lines(br.arsenic[,1], br.arsenic[,6], col = "gray", lwd = 0.5)
lines(br.arsenic[,1], -br.arsenic[,6], col = "gray", lwd = 0.5)
points(br.arsenic[,1], br.arsenic[,2], pch = 19, cex = 0.5)


##### RIDGE/LASSO REGRESSION #####
library(glmnet)
n <- dim(df)[1]
p <- dim(df)[2]
dim(df)
X <- as.matrix(df[, 2:12])
y <- df[, p]
fit_ridge <- glmnet(X, y, alpha=0)
round(t(coef(fit_ridge, s=0.1)), 4)
plot(fit_ridge)

##### LUIS ANALYSIS ######

current_path <- dirname(rstudioapi::getActiveDocumentContext()$path)
datasets_dir <- paste(current_path,"datasets", sep = "/")

# Now that the dataframe is stored in an .RData format we can simply load it and the 
# it will have the same name as the filename
load(paste(datasets_dir, "train_df.RData", sep = "/"))

# In case we may want to do some processing to the test dataset later I'll change the name to the dataframe
data <- get("train_df")
skim(data)
library(dplyr)
data_summary <- data %>%
  group_by(Region_Code, Response) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Percentage = Count / sum(Count)) %>%
  arrange(desc(Percentage)) %>%
  head(10)
data_summary
str(data_summary)

summary(df)
library(brglm)
mod <- brglm(Response~., data=df, family=binomial)
summary(mod)


#### VARIABLES IMPORTANCE ####

names <- colnames(train_data)
models <- list()
for (name in names) {
  model <- glm(Response~.-!!sym(name), data=train_data, family=binomial)
  models[[col_name]] <- model
}
