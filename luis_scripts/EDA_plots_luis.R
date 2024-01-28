library(skimr)
library(ggplot2)
library(patchwork)
library(GGally)

current_path <- dirname(rstudioapi::getActiveDocumentContext()$path)
datasets_dir <- paste(current_path,"../datasets", sep = "/")

load(paste(datasets_dir, "train_df.RData", sep = "/"))
load(paste(datasets_dir, "train_reduced.RData", sep = "/"))

# EDA ------------------------------------------------------------------------------------------------------
# Summary of the data
skim(train_df)

# Pie chart of Response variable with percentages of the total observations. 
# Calculate the percentages
response_counts <- table(train_df$Response)
response_props <- round(prop.table(response_counts) * 100, 2)

# Create a data frame for the pie chart
pie_data <- data.frame(
  Response = factor(names(response_counts), labels = c("No", "Yes")),
  Count = as.numeric(response_counts),
  Label = paste0(response_props, "%")
)

# Create the pie chart
p0 <- ggplot(pie_data, aes(x = "", y = Count, fill = Response)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(x = "", y = "", fill = "Interest in Car Insurance") +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 15), 
    text = element_text(size = 15)
  ) +
  geom_text(
    aes(y = Count / 2, label = Label),
    position = position_stack(vjust = 0.8),
    size = 8
  ) +
  scale_fill_discrete(labels = c("No", "Yes"))
p0

# The vast majority of our customers don't seem to be interested in car insurance product. This only highlights the need for better customer targetting methods.

ggsave(paste0(current_path, "/../plots/PercentagesResponse.png"),
      plot = p0,
      width = 10, height = 10, dpi = 300)

# Most of the customers own a driving lincense.  

# Distribution and correlation of the numerical variables 
p2 <- ggpairs(train_df, 
              columns = c("Age", "Annual_Premium", "Vintage"),
              aes(color = Response),
              diag = list(discrete="barDiag", 
                continuous = wrap("densityDiag", alpha=0.7)))
p2

# The distribution of the Age variable with respect to the Response variable shows that the majority of the customers who are interested in car insurance are middle-aged (between 30 and 60 years old), which coindicdes with the age people are more likely to own a car. 
# The customers not interested in acquiring a car insurance policy are mostly distributed among yourger people and some middle-aged adults in their 50s. The distribution is slightly skewed to the right.

# The histogram for Annual Premium suggest that the costs of the car insurance policy is independent of the interest if the customers to buy the product. It has a highly right-skewed distribution, with most of the data concentrated on the lower end of the premium scale and a long tail extending to higher premium values. The lower tail shows a high values as a consequence of entry level health insurance policy as expected. 

# The histogram for Vintage shows a nearly uniform distribution, with a slight increase in frequency towards the middle range of the Vintage variable.

# The variables show negligible linear correlation between them, which is clearly shown in the scatter plot and in the correlation coeffcients.

ggsave(paste0(current_path, "/../plots/NumericalPairs.png"),
      plot = p2,
      width = 10, height = 10, dpi = 300)

# Manually it would be something like 
# subset_data <- subset(train_df, Response == "0")
# # Calculate the density
# density_values <- density(subset_data$Age)
# plot(density_values, col = "red", main = "Density for Response = 0")


train_df$logAge <- log(train_df$Age)
train_df$logAnnual_Premium <- log(train_df$Annual_Premium)
colnames(train_df)
sum(is.null(train_df$logAnnual_Premium))
sum(is.na(train_df$logAnnual_Premium))

p3 <- ggpairs(train_df, 
              columns = c("logAge", "logAnnual_Premium"),
              aes(color = Response),
              diag = list(discrete="barDiag", 
                continuous = wrap("densityDiag", alpha=0.7 )))
p3

# Box plot of Age by Response
p18 <- ggplot(train_df, aes(x = Response, y = Age, fill = Response)) +
  geom_boxplot() +
  labs(x = "", y = "Age (years)", fill = "Response") +
  theme(
    text = element_text(size = 12),
    axis.text.x = element_blank(),
    axis.text = element_text(size = 14)
  ) + 
  guides(fill = FALSE)
p18

# Box plot of logAge by Response
p19 <- ggplot(train_df, aes(x = Response, y = logAge, fill = Response)) +
  geom_boxplot() +
  scale_fill_manual(values = c("No" = "blue", "Yes" = "red")) +
  labs(x = "", y = "log(Age)", fill = "Response") +
  theme(
    text = element_text(size = 12),
    axis.text.x = element_blank(),
    axis.text = element_text(size = 14)
  ) + 
  guides(fill = FALSE)
p19

# Violin plot of Annual_Premium by Response
p20 <- ggplot(train_df, aes(x = Response, y = Annual_Premium, fill = Response)) +
  geom_violin() +
  labs(x = "Response", y = "Annual Premium (au)", fill = "Response") +
  theme(
    text = element_text(size = 12),
    axis.text = element_text(size = 14)
  ) + 
  guides(fill = FALSE)
p20

# Violin plot of logAnnual_Premium by Response
p21 <- ggplot(train_df, aes(x = Response, y = logAnnual_Premium, fill = Response)) +
  geom_violin() +
  scale_fill_manual(values = c("No" = "blue", "Yes" = "red")) +
  labs(x = "Response", y = "log(Annual Premium)(au)", fill = "Response") +
  theme(
    text = element_text(size = 12),
    axis.text = element_text(size = 14)
  ) + 
  guides(fill = FALSE)
p21

p22 <- (p18+p29)/(p20+p21)
p22

# The log transformations of the Age and Annual_Premium variables show more symmetric distributions. 

ggsave(paste0(current_path, "/../plots/LogPairsAge-Annual_Premium.png"),
      plot = p3,
      width = 10, height = 10, dpi = 300) 

ggsave(paste0(current_path, "/../plots/BoxPlotAge-Annual_Premium.png"),
      plot = p22,
      width = 10, height = 10, dpi = 300)

# Create a density plot
p4 <- ggplot(train_df, aes(x = Age, fill = Response)) +
  geom_density(alpha = 0.7) +
  labs(x = "Age (years)", y = "Density per age") +
  theme(
    text = element_text(size = 12),  
    legend.text = element_text(size = 12),  
    axis.text = element_text(size = 14)  
  ) + 
  guides(fill = FALSE)
p4

p5 <- ggplot(train_df, aes(x = Age, fill = Response)) +
  geom_histogram(position = "fill", bins = 50) +
  labs(x = "Age (years)", y = "Density for each age", fill = "Interest in Car Insurance") +
  scale_fill_discrete(labels = c("No", "Yes")) +
  theme(
    text = element_text(size = 12),
    axis.text = element_text(size = 14)
  ) 
p5

p6 <- p4 + p5
p6

# In these plot we can see that the majority of the customers who are interested in car insurance are middle-aged (between 30 and 60 years old).  

ggsave(paste0(current_path, "/../plots/LogPairsAge-Annual_Premium.png"),
      plot = p6,
      width = 10, height = 10, dpi = 300)

# # # Alternative approach with prop.table
# prop.table -> USEFUL FOR ANALYSING THE CONTRIBUTION OF PEOPLE IN THE RESIDUALS OF THE MODELS
# # Create a bar plot
# barplot(prop.table(table(train_df$Response, train_df$Age), margin=1),
#         col = c("blue", "red"), beside = TRUE, 
#         xlab = "Age (years)", 
#         ylab = "Density")
# # Add a legend title
# legend("topright", 
#       legend = c("No", "Yes"),
#       fill = c("blue", "red"),
#       title = "Response")
# # Create a bar plot
# barplot(prop.table(table(train_df$Response, train_df$Age), margin=2),
#         col = c("blue", "red"), 
#         xlab = "Age (years)", 
#         ylab = "Density")
# # Add a legend title
# legend("topright", 
#       legend = c("No", "Yes"),
#       fill = c("blue", "red"),
#       title = "Response")

# Bar plot of Gender by Response
p7 <- ggplot(train_df, aes(x = Gender, fill = Response)) + 
  geom_bar(position = "fill", color = "black") +
  labs(y = "Percentage", fill = "Response") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme(
    text = element_text(size = 12),
    axis.text = element_text(size = 14)
  ) +
  guides(fill = FALSE)

p7

prop.test(table(train_df$Gender, train_df$Response))

# The data shows a similar trend for the interest of males and females customers in the product. However, there is a statistically significant difference in their proportions.

# Bar plot of Driving License by Response in counts
p8 <- ggplot(train_df, aes(x = Driving_License, fill = Response)) + 
  geom_bar(position = "dodge", color = "black") +
  labs(y = "Count", fill = "Response") +
  theme(
    text = element_text(size = 12),
    axis.text = element_text(size = 14)
  ) 
p8

# Most customers who are interested in car insurance have a driving license. The vast majority of customers have a driving license but are not interested in the product. Few customers don't have a driving license but showed interested in the car insurance policy. 

# These customers could introduce a bias in the model, since they are not allowed to drive a car and thus are not likely to be interested in a car insurance policy. However, we can't be sure of this assumption, since they could be interested in the product for other reasons, such as the possibility of buying a car in the future


# Histogram of Age variable by Response and Driving_License variables
px <- ggplot(train_df, aes(x = Age, fill = Response)) +
  geom_histogram() +
  facet_grid(~ Driving_License ) +
  labs(x = "Age (years)", y = "Density", fill = "Response") +
  scale_fill_discrete(labels = c("No", "Yes")) +
  ggtitle("Histogram of Age by Driving License Status and Interest in Car Insurance") + 
  theme(
    text = element_text(size = 12),
    axis.text = element_text(size = 14)
  ) 
px

# The distribution of the age of the customers interested in the product doesn't change too much when considering the driving license status. However, the distribution of the age of the customers not interested in the product changes significantly when considering the driving license status.

py <- (p7 + p8) / px
py

ggsave(paste0(current_path, "/../plots/Gender-Driving_License-AgeByResponse.png"),
      plot = py,
      width = 10, height = 10, dpi = 300)

# Bar plot of Gender variable by Vehicle_Damage variable
p01 <- ggplot(train_df, aes(x = Gender, fill = Vehicle_Damage)) +
  geom_bar(position = "dodge2") +
  labs(y = "Percentage", fill = "Vehicle_Damage") +
  theme(
    text = element_text(size = 12),
    axis.text = element_text(size = 14)
  ) + 
  guides(fill = FALSE)

# More than a half of the customers had their vehilce damaged in the past, irrespective of their Gender. 

# Density of Age variable by Vehicle_Damage variable
p02 <- ggplot(train_df, aes(x = Age, fill = Vehicle_Damage)) +
  geom_density(alpha = 0.7) +
  labs(x = "Age (years)", y = "Density", fill = "Vehicle_Damage") +
  theme(
    text = element_text(size = 12),
    axis.text = element_text(size = 14)
  )
# The Age of the customers who had their vehicle damaged in the past is more evenly distributed than the Age of the customers who didn't have their vehicle damaged in the past. Middle-aged customers are more likely to have their vehicle damaged in the past and more young customers didn't have their cars damaged.


p1 <- p01 + p02
p1

ggsave(paste0(current_path, "/../plots/Gender-AgeByVehicle_Damage.png"),
      plot = p1,
      width = 10, height = 10, dpi = 300)

# Bar plot of Region Code by Response in counts
# We plot a subset of labels because there are too many categories
p9 <- ggplot(train_df, aes(x = Region_Code, fill = Response)) + 
  geom_bar(position = "dodge", color = "black") +
  labs(y = "Count", fill = "Response") +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 2)]) +
  theme(
    text = element_text(size = 12),
    axis.text = element_text(size = 14)
  )
p9

# Bar plot of Region Code by Response in counts for train_reduced
p10 <- ggplot(train_reduced, aes(x = Region_Reduced, fill = Response)) + 
  geom_bar(position = "dodge", color = "black") +
  labs(y = "Count", fill = "Response") +
  theme(
    text = element_text(size = 12),
    axis.text = element_text(size = 14)
  ) + 
  guides(fill = FALSE)
p10

p11 <- ggplot(train_reduced, aes(x = Region_Reduced, fill=Response)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), color = "black") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(y = "Percentage", x = "Region_Code") +
  theme(
    text = element_text(size = 12),
    axis.text = element_text(size = 14)
  )
p11

p12 <- p9 / (p10+p11)
p12

# The vast majority of the customers are from region 28. The customers from region 28 are also the ones who are most interested in car insurance. 
# Almost half of the customers are distributed among regions 8, 28, 41, 46 accounting for ~47% of the total customers.
# The customers of all the other regions account for almost the same amount of interested customers as the customers from region 28.

ggsave(paste0(current_path, "/../plots/Region_CodeByResponse.png"),
      plot = p12,
      width = 10, height = 10, dpi = 300)

# Bar plots of Previously_Insured, Vehicle_Age, Vehicle_Damage by Response as the color or fill variable.
increase_text_size <- function(p) {
  p + theme(text = element_text(size = 14),
            axis.text = element_text(size = 14),
            axis.title = element_text(size = 16))
}

# Create the ggpairs plot
p13 <- ggpairs(train_reduced, 
               columns = c("Previously_Insured", "Vehicle_Age", "Vehicle_Damage"),
               aes(color = Response), 
               axisLabels = "show"
              )
# Apply the function to each plot in the ggpairs object
p13 <- p13 + ggplot2::theme_update(text = ggplot2::element_text(size = 14))

p13

# We can clearly see that almost a half of the customers don't posses a car insurance.

# Among all the customers, only a few of those who didn't have their vehicle insured in the past are interested in car insurance. 

# People with cars that are in the range of 1 and 2 years old are the most interested in the car insurance product, being the most common age of the customers's cars. Newer or older cars are less likely to be insured. Within one year of purchased cars are more likely to damaged.

# We can see that customers who didn't have their cars damaged in the past are not interested in the car insurance product, regardless of their previous car insurance condition. Only customers who had their vehicle damaged in the past and didn't have their cars insured are more likely to be interested in the product. 

# More than a half of the customers had their vehilce damaged in the past. These customers are also way more likely to be interested in vehicle insurance. This creates a very interesting but also risky path for the potential targeting of those customers in a new marketing campaign.

# The riskiness stems from the fact that these clients are most likely to damage their cars in the future and thus could possibly negatively impact the profits. If this group were to enroll onto the car insurance plan, an adequate pricing of the insurance plan must be ensured with respect to the likelyhood of damaging their car in the future.

ggsave(paste0(current_path, "/../plots/Previously_Insured-Vehicle_Age-Vehicle_DamageByResponse.png"),
      plot = p13,
      width = 10, height = 10, dpi = 300)

# Bar plot of Policy_Sales_Channel by Response in counts
# We plot a subset of labels because there are too many categories
p14 <- ggplot(train_df, aes(x = Policy_Sales_Channel, fill = Response)) + 
  geom_bar(position = "dodge", color = "black") +
  labs(y = "Count", fill = "Response") +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 10)]) +
  theme(
    text = element_text(size = 12),
    axis.text = element_text(size = 14)
  )
p14

# Bar plot of Region Code by Response in counts for train_reduced
p15 <- ggplot(train_reduced, aes(x = Channels_Reduced, fill = Response)) + 
  geom_bar(position = "dodge", color = "black") +
  labs(y = "Count", fill = "Response") +
  theme(
    text = element_text(size = 12),
    axis.text = element_text(size = 14)
  ) + 
  guides(fill = FALSE)
p15

p16 <- ggplot(train_reduced, aes(x = Channels_Reduced, fill=Response)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), color = "black") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(y = "Percentage", x = "Channels_Reduced") +
  theme(
    text = element_text(size = 12),
    axis.text = element_text(size = 14)
  )
p16

p17 <- p14 / (p15+p16)
p17

# The vast majority of the customers where contacted from channel 152. However, the rest of the channels are more likely to be interested in the car insurance product. Channels 26, 124, 152 and 160 alone account for more than 80% of the customers. Channels 26 and 124 are the ones with the highest percentage of customers interested in the product. Abut 20% of the customers interested in the product are distributed in the rest of the channels of outreach. Channel 160 is the one with the lowest percentage of customers interested in the product.

ggsave(paste0(current_path, "/../plots/PolicySalesChannelByResponse.png"),
      plot = p17,
      width = 10, height = 10, dpi = 300)

# Compute independence test of categorical variables using the Pearson's Chi-Squared test.

factor_variables <- sapply(train_df, is.factor)
factor_variables_names <- names(factor_variables[factor_variables])
factor_variables_names <- factor_variables_names[!factor_variables_names %in% "Response"]

# Dataframe to store values
chi_squared_results <- data.frame(
  Variable1 = c(),
  Variable2 = c(),
  p_value = c()
)

k <- 2
for (i in factor_variables_names[1:7]){
  for (j in factor_variables_names[k:8]){
    # Extract the variables for the chi-squared test
    var1 <- train_df[, i]
    var2 <- train_df[, j]
    
    # Create a contingency table
    contingency_table <- table(var1, var2)

    # Perform the chi-squared test
    result <- chisq.test(contingency_table)

    # Add new row
    new_row <- data.frame(
      Variable1 = i,
      Variable2 = j,
      p_value = result$p.value
    )    
    # Store the result in the list
    chi_squared_results <- rbind(chi_squared_results, new_row)
  }
  k <- k + 1
}
# Print the results
# print(chi_squared_results)
max_row <- chi_squared_results[which.max(chi_squared_results[['p_value']]), ]
print(max_row)