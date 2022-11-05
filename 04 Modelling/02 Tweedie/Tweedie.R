#### Set Up ####

source("main.R")
source("00 source.R")
load("00 envr/Compulsory/policy_claims.Rda")
#source("03 Preparation/Data Split.R")
source("03 Preparation/Scripts/03 geo_code.R")
source("03 Preparation/03 split_by_LoI.R")

library(tweedie)
library(statmod)

training_data_prop <- training_data %>% filter(suminsured_prop > 0)
training_data_bi <- training_data %>% filter(suminsured_lossofinc > 0)
# dim(training_data)
# dim(training_data[which(training_data$LossofIncome_cover == TRUE), ])
# dim(training_data[which(training_data$suminsured_lossofinc > 0), ])
# dim(training_data[which(training_data$LossofIncome_cover == TRUE & training_data$suminsured_lossofinc > 0), ])

glm_tweedie <- function(formula, data = training_data, method = "series") {
  profile <- tweedie.profile(formula, data = data, 
                             xi.vec = seq(1.1, 1.9, length = 9), method = method, do.ci = F)
  print(profile$xi.max)
  model <- glm(formula, data = data, offset = log(exposure), weights = date_weights,
               family = tweedie(var.power = profile$xi.max, link.power = 0))
  return(model)
}

#### Models ####

# Property #
# formula = claimcount_prop ~ log(suminsured_prop) + geo_code +
#   building_age + building_type +
#   construction_walls + construction_floor +
#   sprinkler_type + occupation_risk,

formula_prop <- grossincurred_prop ~ log(suminsured_prop) +  
  building_age + building_type + construction_walls + construction_floor +
  sprinkler_type + occupation_risk + electoraterating +
  LossofIncome_cover
modelprop_tw <- glm_tweedie(formula_prop, data = training_data_prop)
summary(modelprop_tw)

# Business Interruption #
# x_lossofinc ~ log(suminsured_lossofinc) + indem_per_grp + occupation_risk
# x_lossofinc ~ log(suminsured_lossofinc) + indem_per_grp + occupation_risk + geo_code

formula_bi <- grossincurred_lossofinc ~ log(suminsured_lossofinc) + 
  building_age + building_type + construction_walls + construction_floor +
  sprinkler_type + occupation_risk + electoraterating #+indem_per_grp
modelbi_tw <- glm_tweedie(formula_bi, data = training_data_bi)
summary(modelbi_tw)

# Interaction #

prop_tw <- predict(modelprop_tw, newdata = test_data, type = "response")
bi_tw <- predict(modelbi_tw, newdata = test_data, type = "response")
actual <- test_data$grossincurred_prop + test_data$grossincurred_lossofinc

model_tw <- glm(actual ~ prop_tw + bi_tw + prop_tw:bi_tw, data = test_data, family = gaussian)
RMSE(mean(training_data$grossincurred_prop), test_data$grossincurred_prop)

#### Results (prop) ####

# Interpret the GLM coefficients and do diagnostic checking of the model.
plot(modelprop_tw$fitted.values, rstandard(modelprop_tw), 
     xlab = "Fitted values", ylab = "Standardized deviance residuals",
     main = "Diagnostic checking: SDR vs FV, Gamma")
abline(h = 0, col = "red", lty = 2)

qqnorm(rstandard(modelprop_tw))
qqline(rstandard(modelprop_tw), col = "red")

# Compare the prediction performance of models in the test set.
plot(predict(modelprop_tw, newdata = test_data, type = "link"), log(test_data$grossincurred_prop + 1),
     ylab = "Log Claims", xlab = "Log Tweedie Scores")
abline(0,1)

# test RMSE
tweedie_prop_pred <- predict(modelprop_tw, newdata = test_data, type = "response")
tweedie_prop_RMSE <-sqrt(sum((tweedie_prop_pred - test_data$grossincurred_prop)^2) / nrow(test_data))
RMSE(mean(training_data$grossincurred_prop), test_data$grossincurred_prop)

# R-squared
1 - sum((tweedie_prop_pred - test_data$grossincurred_prop)^2) / sum((test_data$grossincurred_prop - mean(test_data$grossincurred_prop))^2)

