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
library(rpart)

#----------------- DATA PREP -----------------
set.seed(123)

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
test_data_trees <- test_data %>% 
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
treeGrid <- expand.grid(cp=seq(0, 0.005, 0.05))
ctrl <- trainControl(method = "cv", number = 3)

# property frequency fit
treeFreqProp <- train(
  formula_freqprop,
  data = training_data_trees_prop,
  method = "rpart",
  trControl = ctrl,
  tuneGrid = treeGrid,
  maximize=F,
  metric = "RMSE"
)

# loss of income frequency fit
treeFreqLOI <- train(
  formula_freqloi,
  data = training_data_trees_loi,
  method = "rpart",
  trControl = ctrl,
  tuneGrid = treeGrid,
  maximize=F,
  metric = "RMSE"
)

# property severity fit
treeSevProp <- train(
  formula_sevprop,
  data = training_data_trees_prop,
  method = "rpart",
  trControl = ctrl,
  tuneGrid = treeGrid,
  maximize=F,
  metric = "RMSE"
)

# loss of income severity fit
treeSevLOI <- train(
  formula_sevloi,
  data = training_data_trees_loi,
  method = "rpart",
  trControl = ctrl,
  tuneGrid = treeGrid,
  maximize=F,
  metric = "RMSE"
)

#----------------- PREDICTION ----------------

# Predicting and obtaining the RMSE for freq prop (test set)
treeFreqProp_pred <- predict(treeFreqProp, newdata = test_data_trees_prop)
(treeFreqProp_rmse <- sqrt(mean((test_data_trees_prop$frequency_prop - treeFreqProp_pred)^2)))

# Predicting and obtaining the RMSE for freq loi (test set)
treeFreqLOI_pred <- predict(treeFreqLOI, newdata = test_data_trees_loi)
(treeFreqLOI_rmse <- sqrt(mean((test_data_trees_loi$frequency_lossofinc - treeFreqLOI_pred)^2)))

# Predicting and obtaining the RMSE for sev prop (test set)
treeSevProp_pred <- predict(treeSevProp, newdata = test_data_trees_prop)
(treeSevProp_rmse <- sqrt(mean((test_data_trees_prop$severity_prop - treeSevProp_pred)^2)))

# Predicting and obtaining the RMSE for sev loi (test set)
treeSevLOI_pred <- predict(treeSevLOI, newdata = test_data_trees_loi)
(treeSevLOI_rmse <- sqrt(mean((test_data_trees_loi$severity_lossofinc - treeSevLOI_pred)^2)))

#------------ VARIABLE IMPORTANCE ------------

varImp(treeFreqProp)
varImp(treeFreqLOI)
varImp(treeSevProp)
varImp(treeSevLOI)