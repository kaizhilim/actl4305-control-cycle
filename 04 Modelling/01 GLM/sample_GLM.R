library(tidyverse)
source("03 Preparation/Data Split.R")

family_list = list(Gamma(link = "log"), 
                inverse.gaussian(link = "log"))

expand_grid_family <- expand_grid(
  prop = family_list,
  LoI = family_list,
  interaction = family_list
)

glm_prop_data <- training_data%>%
  mutate(across(where(is.factor), fct_drop))%>%
  mutate(across(where(is.logical), as.numeric))

glm_LoI_data <- training_data%>%
  filter(LossofIncome_cover)%>%
  filter(suminsured_lossofinc >0 )%>%
  mutate(across(where(is.factor), fct_drop))%>%
  mutate(across(where(is.logical), as.numeric))

fit_prop_freq <-glm(
  claimcount_prop ~ log(suminsured_prop) + geo_code + state + 
    building_age + building_type + construction_walls + construction_floor +
    sprinkler_type + occupation_risk,
  data = glm_prop_data,
  family = poisson(link = "log"),
  offset = log(exposure), weights = date_weights
)

fit_prop_severity <- glm(
  grossincurred_prop ~ log(suminsured_prop) + state + 
    building_age + building_type + construction_walls + construction_floor + 
    sprinkler_type + occupation_risk,
  data = glm_prop_data%>%
    filter(grossincurred_prop > 0),
  family = Gamma(link = "log"),
  offset = log(exposure), weights = date_weights
)

fit_LoI_freq <- glm(
  claimcount_lossofinc ~ log(suminsured_lossofinc) + geo_code + state + 
    building_age + building_type + construction_walls + construction_floor + 
    sprinkler_type + occupation_risk,
  data = glm_LoI_data, 
  family = poisson(link = "log"),
  offset = log(exposure), weights = date_weights
)

fit_LoI_severity <- glm(
  grossincurred_lossofinc ~ log(suminsured_lossofinc) + state + 
    building_age + building_type + construction_walls + construction_floor + 
    sprinkler_type + occupation_risk + 
    indem_per_grp,
  data = glm_LoI_data%>%
    filter(grossincurred_lossofinc > 0),
  family = Gamma(link = "log"),
  offset = log(exposure), weights = date_weights
)