  # LIBRARIES ------------------------------------------------------------------------------------------------
  if(!require(ggplot2)) install.packages("ggplot2")
  if(!require(ROSE)) install.packages("ROSE")
  if(!require(mgcViz)) install.packages("mgcViz")
  library(mgcViz)
  library(ROSE)
  library(ggplot2)
  library(mgcv)
  library(dplyr)


  # LOAD THE DATA-----------------------------------------------------------------
  # Define the path to the datasets
  current_path <- dirname(rstudioapi::getActiveDocumentContext()$path)
  datasets_dir <- paste(current_path,"../datasets", sep = "/")
  datasets_dir

# Now that the dataframe is stored in an .RData format we can simply load it and the 
# it will have the same name as the filename
load(paste(datasets_dir, "train_df.RData", sep = "/"))

# In case we may want to do some processing to the test dataset later I'll change the name to the dataframe
data <- get("train_df")

#-------------------------------------------------------------------------------
# MODELS
#-------------------------------------------------------------------------------

# splines and logarithm for Age and Annual_Premium | NO region and channels----
gam_all_no_region_channels <- gam(Response ~ Gender + Driving_License + Previously_Insured + 
                                  Vehicle_Age + Vehicle_Damage + id + s(log(Age)) + 
                                  s(log(Annual_Premium)) + Vintage,  data = data,
                                  family = binomial)
summary(gam_all_no_region_channels)

# Family: binomial 
# Link function: logit 

# Formula:
#   Response ~ Gender + Driving_License + Previously_Insured + Vehicle_Age + 
#   Vehicle_Damage + id + s(log(Age)) + s(log(Annual_Premium)) + 
#   Vintage

# Parametric coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                   -4.692e+00  1.689e-01 -27.782  < 2e-16 ***
#   GenderMale                     7.363e-02  1.124e-02   6.554 5.62e-11 ***
#   Driving_License1               1.034e+00  1.640e-01   6.304 2.89e-10 ***
#   Previously_Insured1           -3.977e+00  8.293e-02 -47.951  < 2e-16 ***
#   Vehicle_Age> 2 Years           6.829e-01  3.356e-02  20.350  < 2e-16 ***
#   Vehicle_Age1-2 Year            4.264e-01  2.773e-02  15.381  < 2e-16 ***
#   Vehicle_DamageYes              2.048e+00  3.443e-02  59.470  < 2e-16 ***
#   id                            -1.665e-08  4.952e-08  -0.336    0.737    
# Vintage                       -6.896e-06  6.510e-05  -0.106    0.916    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Approximate significance of smooth terms:
#   edf Ref.df Chi.sq p-value    
# s(log(Age))            8.797  8.980   5116  <2e-16 ***
#   s(log(Annual_Premium)) 4.486  5.467    244  <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# R-sq.(adj) =  0.177   Deviance explained = 27.3%
# UBRE = -0.45868  Scale est. = 1         n = 381109

# splines and logarithm for Age and Annual_Premium | NO region, channels, id and vintage----
gam_all_no_region_channels_id_vintage <- gam(Response ~ Gender + Driving_License + Previously_Insured + 
                                             Vehicle_Age + Vehicle_Damage + s(log(Age)) +
                                             s(log(Annual_Premium)),
                                             data = data, family = binomial)
summary(gam_all_no_region_channels_id_vintage)

# Family: binomial 
# Link function: logit 

# Formula:
#   Response ~ Gender + Driving_License + Previously_Insured + Vehicle_Age + 
#   Vehicle_Damage + s(log(Age)) + s(log(Annual_Premium))

# Parametric coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                   -4.69631    0.16832 -27.901  < 2e-16 ***
#   GenderMale                     0.07362    0.01124   6.553 5.66e-11 ***
#   Driving_License1               1.03415    0.16403   6.305 2.89e-10 ***
#   Previously_Insured1           -3.97659    0.08293 -47.951  < 2e-16 ***
#   Vehicle_Age> 2 Years           0.68291    0.03356  20.349  < 2e-16 ***
#   Vehicle_Age1-2 Year            0.42642    0.02773  15.380  < 2e-16 ***
#   Vehicle_DamageYes              2.04774    0.03443  59.470  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Approximate significance of smooth terms:
#   edf Ref.df Chi.sq p-value    
# s(log(Age))            8.797  8.980   5116  <2e-16 ***
#   s(log(Annual_Premium)) 4.485  5.467    244  <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# R-sq.(adj) =  0.177   Deviance explained = 27.3%
# UBRE = -0.45869  Scale est. = 1         n = 381109

# splines and logarithm for Age and Annual_Premium | NO channels, id and vintage----
gam_all_no_channels_id_vintage <- gam(Response ~ Gender + Region_Code + Driving_License + Previously_Insured +
                                      Vehicle_Age + Vehicle_Damage + s(log(Age)) + s(log(Annual_Premium)),
                                      data = data, family = binomial)
summary(gam_all_no_channels_id_vintage)

# Family: binomial 
# Link function: logit 

# Formula:
# Response ~ Gender + Region_Code + Driving_License + Previously_Insured + 
#     Vehicle_Age + Vehicle_Damage + s(log(Age)) + s(log(Annual_Premium))

# Parametric coefficients:
#                               Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                   -5.34500    0.18793 -28.442  < 2e-16 ***
# GenderMale                     0.08017    0.01130   7.098 1.27e-12 ***
# Region_Code1                   0.24981    0.13460   1.856 0.063457 .  
# Region_Code2                   0.45429    0.10654   4.264 2.01e-05 ***
# Region_Code3                   0.87745    0.09034   9.712  < 2e-16 ***
# Region_Code4                   0.88320    0.10981   8.043 8.79e-16 ***
# Region_Code5                   0.70543    0.12612   5.593 2.23e-08 ***
# Region_Code6                   0.86597    0.10075   8.595  < 2e-16 ***
# Region_Code7                   0.69876    0.10021   6.973 3.11e-12 ***
# Region_Code8                   0.55396    0.08583   6.454 1.09e-10 ***
# Region_Code9                   0.34897    0.10905   3.200 0.001374 ** 
# Region_Code10                  0.59221    0.10918   5.424 5.83e-08 ***
# Region_Code11                  1.09905    0.09169  11.986  < 2e-16 ***
# Region_Code12                  0.71184    0.10619   6.703 2.04e-11 ***
# Region_Code13                  0.62646    0.10294   6.085 1.16e-09 ***
# Region_Code14                  0.77083    0.10124   7.614 2.66e-14 ***
# Region_Code15                  0.42328    0.09093   4.655 3.24e-06 ***
# Region_Code16                  0.42780    0.12734   3.360 0.000781 ***
# Region_Code17                  0.44329    0.11772   3.766 0.000166 ***
# Region_Code18                  0.98204    0.09561  10.271  < 2e-16 ***
# Region_Code19                  0.72945    0.11203   6.511 7.44e-11 ***
# Region_Code20                  0.34697    0.11140   3.115 0.001842 ** 
# Region_Code21                  0.72878    0.10709   6.805 1.01e-11 ***
# Region_Code22                  0.27108    0.14703   1.844 0.065230 .  
# Region_Code23                  0.91157    0.10888   8.372  < 2e-16 ***
# Region_Code24                  0.76314    0.10487   7.277 3.42e-13 ***
# Region_Code25                  0.13316    0.13413   0.993 0.320817    
# Region_Code26                  0.13707    0.11572   1.185 0.236186    
# Region_Code27                  0.42636    0.11342   3.759 0.000170 ***
# Region_Code28                  0.84050    0.08403  10.002  < 2e-16 ***
# Region_Code29                  1.07803    0.08971  12.017  < 2e-16 ***
# Region_Code30                  0.82499    0.09223   8.945  < 2e-16 ***
# Region_Code31                  0.37154    0.11467   3.240 0.001195 ** 
# Region_Code32                  0.67971    0.11137   6.103 1.04e-09 ***
# Region_Code33                  0.66836    0.09344   7.153 8.51e-13 ***
# Region_Code34                  0.33427    0.12901   2.591 0.009571 ** 
# Region_Code35                  0.97364    0.09300  10.469  < 2e-16 ***
# Region_Code36                  0.53375    0.09371   5.696 1.23e-08 ***
# Region_Code37                  0.47764    0.09921   4.814 1.48e-06 ***
# Region_Code38                  0.91987    0.10350   8.887  < 2e-16 ***
# Region_Code39                  0.59595    0.09610   6.202 5.59e-10 ***
# Region_Code40                  0.57729    0.12439   4.641 3.47e-06 ***
# Region_Code41                  0.97602    0.08732  11.178  < 2e-16 ***
# Region_Code42                  0.24575    0.18617   1.320 0.186829    
# Region_Code43                  0.39986    0.11012   3.631 0.000282 ***
# Region_Code44                  0.05249    0.20553   0.255 0.798410    
# Region_Code45                  0.72248    0.09587   7.536 4.85e-14 ***
# Region_Code46                  0.66587    0.08729   7.628 2.38e-14 ***
# Region_Code47                  0.44975    0.09444   4.762 1.91e-06 ***
# Region_Code48                  0.24639    0.09679   2.546 0.010910 *  
# Region_Code49                  0.32670    0.12576   2.598 0.009383 ** 
# Region_Code50                  0.20915    0.09412   2.222 0.026264 *  
# Region_Code51                  0.80537    0.23927   3.366 0.000763 ***
# Region_Code52                  0.76559    0.22097   3.465 0.000531 ***
# Driving_License1               1.02098    0.16430   6.214 5.16e-10 ***
# Previously_Insured1           -4.00663    0.08301 -48.269  < 2e-16 ***
# Vehicle_Age> 2 Years           0.61577    0.03363  18.310  < 2e-16 ***
# Vehicle_Age1-2 Year            0.36314    0.02769  13.115  < 2e-16 ***
# Vehicle_DamageYes              2.03408    0.03450  58.965  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Approximate significance of smooth terms:
#                          edf Ref.df  Chi.sq p-value    
# s(log(Age))            8.802  8.981 4777.78  <2e-16 ***
# s(log(Annual_Premium)) 3.648  4.501   51.71  <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# R-sq.(adj) =  0.182   Deviance explained = 27.8%
# UBRE = -0.46218  Scale est. = 1         n = 381109

# splines and logarithm for Age, Annual_Premium and channels + region reduced | NO channels, id and vintage | LASSO penalization----
gam_all_no_id_vintage_lasso <- gam(Response ~ Gender + Channels_Reduced + Driving_License +
                             Previously_Insured + Vehicle_Damage + s(log(Age)) +
                             s(log(Annual_Premium)) + Region_Reduced + Vehicle_Age,
                             data = data, family = binomial(), 
                             method = "REML") #LASSO
summary(gam_all_no_id_vintage_lasso)
anova(gam_all_no_id_vintage_lasso)

# Family: binomial 
# Link function: logit 

# Formula:
#   Response ~ Gender + Channels_Reduced + Driving_License + Previously_Insured + 
#   Vehicle_Damage + s(log(Age)) + s(log(Annual_Premium)) + Region_Reduced + 
#   Vehicle_Age

# Parametric coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                   -4.0168422  0.1692898 -23.728  < 2e-16 ***
#   GenderMale                     0.0651583  0.0113388   5.746 9.11e-09 ***
#   Channels_Reduced124           -0.2130120  0.0142575 -14.940  < 2e-16 ***
#   Channels_Reduced152           -1.0122912  0.0275359 -36.763  < 2e-16 ***
#   Channels_Reduced160           -1.6330238  0.0556325 -29.354  < 2e-16 ***
#   Channels_Reduced0             -0.2446565  0.0149380 -16.378  < 2e-16 ***
#   Driving_License1               1.0485105  0.1641555   6.387 1.69e-10 ***
#   Previously_Insured1           -3.9129117  0.0828061 -47.254  < 2e-16 ***
#   Vehicle_DamageYes              2.0050234  0.0344312  58.233  < 2e-16 ***
#   Region_Reduced8               -0.1011966  0.0224234  -4.513 6.39e-06 ***
#   Region_Reduced28               0.1190311  0.0136445   8.724  < 2e-16 ***
#   Region_Reduced41               0.2968047  0.0274239  10.823  < 2e-16 ***
#   Region_Reduced46              -0.0050018  0.0274889  -0.182    0.856    
# Vehicle_Age> 2 Years           0.2317352  0.0347094   6.676 2.45e-11 ***
#   Vehicle_Age1-2 Year            0.0004002  0.0290268   0.014    0.989    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Approximate significance of smooth terms:
#   edf Ref.df Chi.sq p-value    
# s(log(Age))            8.432  8.865 4181.9  <2e-16 ***
#   s(log(Annual_Premium)) 4.860  5.885  132.5  <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# R-sq.(adj) =  0.186   Deviance explained =   28%
# -REML = 1.0211e+05  Scale est. = 1         n = 381109


# try to use generalized vif 


load(paste(datasets_dir, "train_reduced.RData", sep = "/"))
data <- get("train_reduced")

# splines and logarithm for Age, Annual_Premium and channels + region reduced | NO channels, id and vintage | RIDGE penalization----
gam_all_no_id_vintage_ridge <- gam(Response ~ Gender + Driving_License + Previously_Insured + 
                                  Vehicle_Damage + Channels_Reduced + s(log(Age)) +
                                  s(Annual_Premium) + Region_Reduced + Vehicle_Age,
                                  data = data, family = binomial(), 
                                  optimizer = "efs", select = TRUE)
summary(gam_all_no_id_vintage_ridge)


# splines for Age (logarithm), Annual_Premium and channels + region reduced | NO channels, id and vintage | oversample | penalization----

# Assuming 'target' is the binary target variable
data_oversampled_balanced <- ROSE(Response ~ ., data = data, seed = 42)$data

gam_reduced_oversampled <- gam(Response ~ Gender + Driving_License + Previously_Insured + 
                        Vehicle_Damage + Channels_Reduced + s(log(Age)) +
                        s(Annual_Premium) + Region_Reduced + Vehicle_Age, 
                        data = data_oversampled_balanced, family = binomial(), 
                        optimizer = 'efs', select = TRUE)

# help(gam)
summary(gam_reduced_oversampled)

# splines for Age (logarithm), Annual_Premium and channels + region reduced | NO channels, id and vintage | oversample + undersample | penalization----

data_balanced_sampled <- data_oversampled_balanced[sample(nrow(data_oversampled_balanced), 100000, replace = FALSE), ]

gam_sampled <- gam(Response ~ Gender + Driving_License + Previously_Insured + 
                        Vehicle_Damage + Channels_Reduced + s(log(Age)) +
                        s(Annual_Premium) + Region_Reduced + Vehicle_Age, 
                        data = data_balanced_sampled, family = binomial(), 
                        optimizer = 'efs', select = TRUE)

# help(gam)
summary(gam_sampled)











plot(gam_reduced_oversampled, pages=1)
plot(gam_reduced_oversampled, pages=1, scheme=1, unconditional=TRUE)
plot(gam_reduced_oversampled, pages=1, scheme=2)
plot(gam_reduced_oversampled, pages=1, residuals=TRUE)
plot(gam_reduced_oversampled, pages=2, residuals=TRUE)
plot(gam_reduced_oversampled, pages=1,seWithMean=TRUE)

# What does the p-value mean here?
gam.check(gam_sampled)


gam_sampledViz <- getViz(gam_reduced_oversampled)
print(plot(gam_sampledViz, allTerms = T), pages = 1)
plot(gam_sampledViz)

pl <- plot(gam_sampledViz, allTerms = T) + l_points() + l_fitLine(linetype = 3) + l_fitContour() + 
      l_ciLine(colour = 2) + l_ciBar() + l_fitPoints(size = 1, col = 2) + theme_get() + labs(title = NULL) #+ 
      # l_dens(type = "cond")
print(pl, pages = 1)

# What does the p-value mean here?
check(gam_sampledViz)

gam_sampledViz <- getViz(gam_sampled, nsim=100)
gridPrint(check1D(gam_sampledViz, "log(Age)") + l_gridCheck1D(gridFun = sd, showReps = TRUE), 
          check1D(gam_sampledViz, "Annual_Premium") + l_gridCheck1D(gridFun = sd, showReps = TRUE),
          check1D(gam_sampledViz, "Gender") + l_gridCheck1D(gridFun = sd, showReps = TRUE),
          check1D(gam_sampledViz, "Driving_License") + l_gridCheck1D(gridFun = sd, showReps = TRUE),
          check1D(gam_sampledViz, "Previously_Insured") + l_gridCheck1D(gridFun = sd, showReps = TRUE),
          check1D(gam_sampledViz, "Vehicle_Damage") + l_gridCheck1D(gridFun = sd, showReps = TRUE),
          check1D(gam_sampledViz, "Channels_Reduced") + l_gridCheck1D(gridFun = sd, showReps = TRUE),
          check1D(gam_sampledViz, "Region_Reduced") + l_gridCheck1D(gridFun = sd, showReps = TRUE),
          check1D(gam_sampledViz, "Vehicle_Age") + l_gridCheck1D(gridFun = sd, showReps = TRUE)
)
          #  ncol = 2)
