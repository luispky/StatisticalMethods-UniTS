#*****************************************************************************************
#* Script with commands I'd like to test
#*****************************************************************************************

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


# plot(gam_sampled, pages=1, scheme=1, unconditional=TRUE)
# plot(gam_sampled, pages=1, scheme=2)
# plot(gam_sampled, pages=1, residuals=TRUE)
# plot(gam_sampled, pages=2, residuals=TRUE)
# plot(gam_sampled, pages=1,seWithMean=TRUE)

# # What does the p-value mean here?
# gam.check(gam_sampled)

# gam_sampledViz <- getViz(gam_sampled)
# print(plot(gam_sampledViz, allTerms = T), pages = 1)
# plot(gam_sampledViz)

# pl <- plot(gam_sampledViz, allTerms = T) + l_points() + l_fitLine(linetype = 3) + l_fitContour() + 
#       l_ciLine(colour = 2) + l_ciBar() + l_fitPoints(size = 1, col = 2) + theme_get() + labs(title = NULL) #+ 
#       # l_dens(type = "cond")
# print(pl, pages = 1)

# # What does the p-value mean here?
# check(gam_sampledViz)

# gam_sampledViz <- getViz(gam_sampled, nsim=100)
# gridPrint(check1D(gam_sampledViz, "log(Age)") + l_gridCheck1D(gridFun = sd, showReps = TRUE), 
#           check1D(gam_sampledViz, "Annual_Premium") + l_gridCheck1D(gridFun = sd, showReps = TRUE),
#           check1D(gam_sampledViz, "Gender") + l_gridCheck1D(gridFun = sd, showReps = TRUE),
#           check1D(gam_sampledViz, "Driving_License") + l_gridCheck1D(gridFun = sd, showReps = TRUE),
#           check1D(gam_sampledViz, "Previously_Insured") + l_gridCheck1D(gridFun = sd, showReps = TRUE),
#           check1D(gam_sampledViz, "Vehicle_Damage") + l_gridCheck1D(gridFun = sd, showReps = TRUE),
#           check1D(gam_sampledViz, "Channels_Reduced") + l_gridCheck1D(gridFun = sd, showReps = TRUE),
#           check1D(gam_sampledViz, "Region_Reduced") + l_gridCheck1D(gridFun = sd, showReps = TRUE),
#           check1D(gam_sampledViz, "Vehicle_Age") + l_gridCheck1D(gridFun = sd, showReps = TRUE)
# )


# The Variance Inflation Factor (VIF) is used to measure the degree of multicollinearity among the predictor variables in a regression model. High VIF values indicate that the variables may be highly correlated, leading to unstable and unreliable estimates of the regression coefficients.

# In your case, the VIF values are extremely high, leading to an issue known as perfect multicollinearity. This happens when one or more variables in the model can be exactly predicted from others. In your car::vif output, all VIF values are infinite (Inf), suggesting that there is a perfect linear relationship between the predictors.

# Looking at your data, it seems like the issue might be related to the way factors are defined. The factors Gender, Driving_License, Previously_Insured, Vehicle_Age, Vehicle_Damage, Channels_Reduced, and Region_Reduced are all converted to factors with two levels. This might lead to perfect multicollinearity because these factors might be perfectly predictable from each other.

# car::vif(gam_sampled)


