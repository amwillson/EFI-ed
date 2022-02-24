## This script develops and assesses a model of the forecasting-adjacent courses,
## where the predictors are the topics related to forecasting and the response is the
## type of institution (i.e., simplified Carnegie classification)
## A multinomial regression was chosen to accommodate the categorical response variable
## and the multiple categories in the response variable

## Authors: AM Willson & H Gallo
## Date modified: 19 February 2022

rm(list = ls())

#### Load and Prepare Data ####

load('Data/cleaned_data.RData')

# Format data
data = data %>%
  group_by(Sub.topic, Carnegie.classification.2) %>%
  count() %>%
  spread(Sub.topic, freq)

# Change NA to 0
data = data %>%
  replace_na(list(`Basics of Coding` = 0,
                  `Basics of Ecology` = 0,
                  `Basics of Forecasting` = 0,
                  `Basics of Statistics` = 0,
                  `Data Manipulation` = 0,
                  `Data Sources` = 0,
                  `Data Visualization` = 0,
                  `Decision Science` = 0,
                  `Ethics` = 0,
                  `Machine Learning` = 0,
                  `Mechanistic Models` = 0,
                  `Model Assessment` = 0,
                  `Probability & Uncertainty` = 0,
                  `Science Communication` = 0,
                  `Statistical Models` = 0,
                  `Traditional Ecological Knowledge` = 0,
                  `Workflows & Open Science` = 0,
                  `Working with Data` = 0))

# Relevel data so the factor levels of the response are relative to R1 institutions
data$Carnegie.classification.2 = relevel(data$Carnegie.classification.2, ref = 7)

#### Null model ####

# Create an intercept only model that we can compare the full model to
int_mod = multinom(Carnegie.classification.2 ~ 1, data = data)

# View summary of model
summary(int_mod)

#### Full model ####

# Create full model
full_mod = multinom(Carnegie.classification.2 ~ 
                      `Basics of Coding` +
                      `Basics of Ecology` +
                      `Basics of Forecasting` +
                      `Basics of Statistics` +
                      `Data Manipulation` +
                      `Data Sources` +
                      `Data Visualization` +
                      `Decision Science` +
                      `Ethics` +
                      `Machine Learning` +
                      `Mechanistic Models` +
                      `Model Assessment` +
                      `Probability & Uncertainty` +
                      `Science Communication` +
                      `Statistical Models` +
                      `Traditional Ecological Knowledge` +
                      `Workflows & Open Science` +
                      `Working with Data`, data = data)

# View summary of model
summary(full_mod)

# Compute likelihood ratio test between full and null models
anova(full_mod, int_mod)

#### Find simpler, final model ####

## The full model has a higher AIC than the null model, indicating poor predictive power
## relative to the complexity of the model.
## However, likelihood ratio test indicates that the full model explains a significantly 
## greater amount of variability.
## Therefore, we stepwise drop terms from the model and find the model with the lowest AIC
best_mod = stepAIC(full_mod)

# View summary of model
summary(best_mod) # better AIC than null model

# Compute likelihood ratio test between best and full models
anova(best_mod, full_mod) # non-significant difference between best and full models

#### Coefficients ####

## Because the simpler model has a better AIC and comparable variance explained,
## we assume this model is better
## Now, we look at the coefficients to explain patterns in the availability of courses
## Interpretation is relative to R1 institution: positive means course is *more* likely
## to be available at a given institution type than at R1 and negative is the opposite
## We also consider standard error in coefficient values and compute p-values

# Summary of coefficients
coef_sum = summary(best_mod)$coefficients

# Log odds
coef_logodd = exp(summary(best_mod)$coefficients)

# Coefficient error
coef_se = summary(best_mod)$standard.error

# Compute p-values for individual coefficients
# Interpretation: p < 0.05 --> coefficient significantly different from 0 = 
# has a significant effort in predicting institution type
z = summary(best_mod)$coefficients / summary(best_mod)$standard.errors
p = (1 - pnorm(abs(z), 0, 1)) * 2
