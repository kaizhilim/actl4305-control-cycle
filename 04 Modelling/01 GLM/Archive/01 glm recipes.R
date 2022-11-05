library(tidymodels)
library(themis)

fit_resamples_glm_freq_prop<-function(data_split_prop) {
  
  ## 0. Validation Set ####
  training_data_prop <- training(data_split_prop)
  set.seed(123)
  validation_prop <- validation_split(training_data_prop, prop = 0.8,
                                      strata = has_claim)
  
  ## 1. Extract Split Components ####
  Analysis = analysis(validation_prop$splits[[1]])
  Assess = assessment(validation_prop$splits[[1]])
  
  ## 2. Manual Recipe ####
  glm_freq_prop_recipe <- 
    recipe(claimcount_prop ~ exposure + suminsured_prop + geo_code + 
             building_age + building_type + 
             construction_walls + construction_floor +
             sprinkler_type + occupation_risk + 
             has_claim + date_weights,
           data = Analysis)%>%
    update_role(exposure, new_role = "offset")%>%
    update_role(date_weights, new_role = "weights")%>%
    update_role(has_claim, new_role = "rebalancing")%>%
    step_log(suminsured_prop, exposure)%>%
    step_zv(all_predictors())%>%
    step_mutate(has_claim = as_factor(has_claim))%>%
    step_rose(has_claim, seed = 123)%>%
    step_rm(has_claim)
  
  glm_freq_prop_prep <- prep(glm_freq_prop_recipe)
  glm_freq_prop_baked <- bake(glm_freq_prop_prep, new_data = NULL)
  
  ## 3. Model Fitting ####

  ## Poisson
  glm_freq_prop_pois <- glm(
    formula = formula(glm_freq_prop_prep),
    data = glm_freq_prop_baked,
    family=poisson(link = "log"),
    offset = exposure, weights = date_weights)
  ## QuasiPoisson
  glm_freq_prop_quasipois <- glm(
    formula = formula(glm_freq_prop_prep),
    data = glm_freq_prop_baked,
    family=quasipoisson(link = "log"),
    offset = exposure, weights = date_weights)
  
  ## Negative Binomial
  glm_freq_prop_nb<- MASS::glm.nb(
    update(prep(glm_freq_prop_recipe)%>%formula(), ~. + offset(exposure)),
    data = glm_freq_prop_baked,
    weights = date_weights
  )
  
  ## 4. Model Prediction ####
  glm_freq_prop_assess = bake(glm_freq_prop_prep, new_data = Assess)
  glm_freq_prop_pred = glm_freq_prop_assess%>%
    insurancerating::add_prediction(
      glm_freq_prop_pois, glm_freq_prop_quasipois, glm_freq_prop_nb)
  
  chosen_family <- glm_freq_prop_pred%>%
    summarise(across(starts_with("pred"),
                     ~sqrt(
                       mean((.x - claimcount_prop)^2)
                       )))%>%
    as_tibble()%>%
    pivot_longer(cols = everything(),
                 names_to = "family", values_to = "rmse")%>%
    mutate(family = str_extract(family, "(?<=_)[^_]*$"))%>%
    arrange(rmse)%>%
    slice_head(n = 1)%>%
    deframe()
  
  return(chosen_family)
}
