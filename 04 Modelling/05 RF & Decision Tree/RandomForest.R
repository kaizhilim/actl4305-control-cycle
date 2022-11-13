source("00 source.R")
load("00 envr/Compulsory/policy_claims.Rda")
source("03 Preparation/Data Split.R")

#----------------- PACKAGES -----------------
# installations
#install.packages("randomForest")
#install.packages("PRROC")
#install.packages("ROCR")

# packages
library(doParallel)
library(PRROC)
library(tidyverse)
library(dplyr)
library(randomForest) 
library(caret) 
library(pROC)
library(ROCR)
library(tidyr)

#----------------- DATA PREP -----------------
# creating freq/sev for training data
training_data_trees_prop <- training_data %>% 
  dplyr:: mutate (
    frequency_prop = claimcount_prop / exposure,
    frequency_lossofinc = replace_na(claimcount_lossofinc / exposure, 0),
    severity_prop = replace_na(grossincurred_prop / claimcount_prop, 0),
    severity_lossofinc = replace_na(grossincurred_lossofinc/ claimcount_lossofinc, 0)
  ) %>%
  dplyr:: select (everything())

training_data_trees_loi <- training_data %>% 
  dplyr:: mutate (
    frequency_prop = claimcount_prop / exposure,
    frequency_lossofinc = replace_na(claimcount_lossofinc / exposure, 0),
    severity_prop = replace_na(grossincurred_prop / claimcount_prop, 0),
    severity_lossofinc = replace_na(grossincurred_lossofinc/ claimcount_lossofinc, 0)
  ) %>%
  dplyr:: filter(
    suminsured_lossofinc != 0
  ) %>%
  dplyr:: select (everything())

# creating freq/sev for test data
test_data_trees_prop <- test_data %>% 
  dplyr:: mutate (
    frequency_prop = claimcount_prop / exposure,
    frequency_lossofinc = replace_na(claimcount_lossofinc / exposure, 0),
    severity_prop = replace_na(grossincurred_prop / claimcount_prop, 0),
    severity_lossofinc = replace_na(grossincurred_lossofinc/ claimcount_lossofinc, 0)
  ) %>%
  dplyr:: select (everything())

test_data_trees_loi <- test_data %>% 
  dplyr:: mutate (
    frequency_prop = claimcount_prop / exposure,
    frequency_lossofinc = replace_na(claimcount_lossofinc / exposure, 0),
    severity_prop = replace_na(grossincurred_prop / claimcount_prop, 0),
    severity_lossofinc = replace_na(grossincurred_lossofinc/ claimcount_lossofinc, 0)
  ) %>%
  dplyr:: filter(
    suminsured_lossofinc != 0
  ) %>%
  dplyr:: select (everything())

# formula for each of the 4 model fits
formula_freqprop <- frequency_prop ~ log(suminsured_prop) + state + 
  building_age + building_type + construction_walls + construction_floor +
  sprinkler_type + occupation_risk
formula_freqloi <- frequency_lossofinc ~ log(suminsured_lossofinc) + state + 
  building_age + building_type + construction_walls + construction_floor +
  sprinkler_type + occupation_risk

formula_sevprop <- severity_prop ~ log(suminsured_prop) + state + 
  building_age + building_type + construction_walls + construction_floor +
  sprinkler_type + occupation_risk
formula_sevloi <- severity_lossofinc ~ log(suminsured_lossofinc) + state + 
  building_age + building_type + construction_walls + construction_floor +
  sprinkler_type + occupation_risk

#----------------- MODEL FIT -----------------
rfGrid <- expand.grid(mtry = 1:3)
ctrl <- trainControl(method = "cv", number = 3)

# property frequency fit
rfFreqProp <- train(
  formula_freqprop,
  data = training_data_trees_prop,
  method = "rf",
  trControl = ctrl,
  tuneGrid = rfGrid,
  ntree = 10,
  maxdepth = 3,
  importance = T,
  metric = "RMSE"
)

# loss of income frequency fit
rfFreqLOI <- train(
  formula_freqloi,
  data = training_data_trees_loi,
  method = "rf",
  trControl = ctrl,
  tuneGrid = rfGrid,
  ntree = 10,
  maxdepth = 3,
  importance = T,
  metric = "RMSE"
)

# property severity fit
rfSevProp <- train(
  formula_sevprop,
  data = training_data_trees_prop,
  method = "rf",
  trControl = ctrl,
  tuneGrid = rfGrid,
  ntree = 10,
  maxdepth = 3,
  importance = T,
  metric = "RMSE"
)

# loss of income severity fit
rfSevLOI <- train(
  formula_sevloi,
  data = training_data_trees_loi,
  method = "rf",
  trControl = ctrl,
  tuneGrid = rfGrid,
  ntree = 10,
  maxdepth = 3,
  importance = T,
  metric = "RMSE"
)

#----------------- PREDICTION ----------------

# Predicting and obtaining the RMSE for freq prop (test set)
rfFreqProp_pred <- predict(rfFreqProp, newdata = test_data_trees_prop)
(rfFreqProp_rmse <- sqrt(mean((test_data_trees_prop$frequency_prop - rfFreqProp_pred)^2)))

# Predicting and obtaining the RMSE for freq loi (test set)
rfFreqLOI_pred <- predict(rfFreqLOI, newdata = test_data_trees_loi)
(rfFreqLOI_rmse <- sqrt(mean((test_data_trees_loi$frequency_lossofinc - rfFreqLOI_pred)^2)))

# Predicting and obtaining the RMSE for sev prop (test set)
rfSevProp_pred <- predict(rfSevProp, newdata = test_data_trees_prop)
(rfSevProp_rmse <- sqrt(mean((test_data_trees_prop$severity_prop - rfSevProp_pred)^2)))

# Predicting and obtaining the RMSE for sev loi (test set)
rfSevLOI_pred <- predict(rfSevLOI, newdata = test_data_trees_loi)
(rfSevLOI_rmse <- sqrt(mean((test_data_trees_loi$severity_lossofinc - rfSevLOI_pred)^2)))

#---------------- TUNING GRAPH ---------------

plot(rfFreqProp, main = "Property Frequency - mtry Tuning")
plot(rfFreqLOI, main = "Loss of Income Frequency - mtry Tuning")
plot(rfSevProp, main = "Property Severity - mtry Tuning")
plot(rfSevLOI, main = "Loss of Income Severity - mtry Tuning")

#------------ VARIABLE IMPORTANCE ------------

varImp(rfFreqProp)
plot(varImp(rfFreqProp))
varImp(rfFreqLOI)
plot(varImp(rfFreqLOI))
varImp(rfSevProp)
plot(varImp(rfSevProp))
varImp(rfSevLOI)
plot(varImp(rfSevLOI))

#rfFreqProp_varimp <- randomForest(
#  formula = formula_freqprop, 
#  data = training_data, 
#  importance = TRUE
#)
#
#varImpPlot(rfFreqProp_varimp, main = "Feature Importance")