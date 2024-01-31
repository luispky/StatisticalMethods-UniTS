#### LOAD 
setwd("Desktop/uni/statistical_methods/final_project/stats_project")

load(file='datasets/test_data.RData')
load(file='datasets/train_data.RData')



#### VARIABLES IMPORTANCE ####

# sort var by importance with AIC
names <- colnames(df)
names <- names[names != 'Response']
models <- list()

predictors = c("Annual_Premium", "s(log(Age))")
for variable in predictors
model_predictos = predictos without variable
predictors_symols = sym(model_preditors)
gam(Response ~ sum(predcitors))

for (name in names) {
  print(name)
  formula_string <- paste("Response ~ . -", name)
  model_formula <- as.formula(formula_string)
  model <- glm(model_formula, data = df, family = binomial)
  print(model$aic)
  models[[name]] <- model
}



for (name in names) {
  print(name)
  if name is not a numerical var
  model = glm(Response ~ sum of categorical - name + log(numerical vars))
  else
    model = glm(Response ~ )
  
  formula_string <- paste("Response ~ . -", name)
  model_formula <- as.formula(formula_string)
  model <- glm(model_formula, data = df, family = binomial)
  print(model$aic)
  models[[name]] <- model
}


aic_values <- sapply(models, AIC)

# Create a data frame
df_aic <- data.frame(Model_Name = names, AIC = aic_values)
df_aic
# Assuming df is your DataFrame
df_sorted <- df_aic[order(df_aic$AIC, decreasing=TRUE), ]
df_sorted

# compute nested models
names_ord <- df_sorted$Model_Name
names_nested <- c()
nest_models <- list()

for (name in names_ord) {
  print(paste('name:', name))
  names_nested <- c(names_nested, name)
  print(names_nested)
  formula_str <- paste("Response", "~", paste(names_nested, collapse = " + "))
  formula <- as.formula(formula_str)
  model <- glm(formula, data = df, family = binomial)
  nest_models[[name]] <- model
}
aic_values2 <- sapply(nest_models, AIC)
df_aic2 <- data.frame(Model_Name = names_ord, AIC = aic_values2)
df_aic2
# Assuming df is your DataFrame
df_sorted2 <- df_aic2[order(df_aic2$AIC, decreasing=TRUE), ]
df_sorted2

#********************************************************
#* INCLUDE anova ANALYSIS TO CHECK FOR SIGNIFICANT VARS *
#* INCLUDE vif ANALYSIS TO CHECK FOR MULTICOLLINEARITY  *
#******************************************************** 
