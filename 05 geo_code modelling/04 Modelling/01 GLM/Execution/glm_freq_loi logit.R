source("05 geo_code modelling/main.R")

library(tidymodels)
library(themis)

### 1. Standardize Data ####
LoIClaims <- policy_claims%>%
  filter(LossofIncome_cover, suminsured_lossofinc > 0)%>%
  mutate(occupation_risk = fct_drop(occupation_risk),
         has_claim = factor(grossincurred_lossofinc > 0,
                            levels = c(T, F)),
         date_weights = parsnip::importance_weights(date_weights))

### 2. Preprocessing ####
set.seed(123)
data_split_loi <- initial_split(LoIClaims, prop = 0.8, strata = has_claim)

training_data_loi <- training(data_split_loi)
testing_data_loi <- testing(data_split_loi)

logit_freq_loi_rec <- 
  recipe(has_claim ~ suminsured_lossofinc + occupation_risk + 
           date_weights + exposure,
       data = training_data_loi)%>%
  
  ## Over-sampling for class imbalance
  step_downsample(has_claim, under_ratio = 10, seed = 123)%>%
  
  ## Final variable amendments
  step_log(suminsured_lossofinc)%>%
  
  update_role(exposure, new_role = "refactor")%>%
  step_mutate(occupation_risk = fct_reorder(
    fct_drop(occupation_risk), exposure,
    .fun = sum, .desc = TRUE
  ))%>%
  
  ## Dummy
  step_dummy(occupation_risk)

logit_freq_loi_spec<-
  logistic_reg()%>%
  set_engine("glm")

logit_freq_loi_wflw<-
  workflow()%>%
  add_model(logit_freq_loi_spec)%>%
  add_recipe(logit_freq_loi_rec)%>%
  add_case_weights(date_weights)

logit_freq_loi_res <- 
  logit_freq_loi_wflw%>%
  last_fit(data_split_loi,
           metrics = metric_set(sens, roc_auc))

### 3. Modelling Functions ####
logit_freq_loi_res%>%
  extract_fit_parsnip()%>%
  tidy()

logit_freq_loi_res%>%
  extract_fit_engine()%>%
  summary()
