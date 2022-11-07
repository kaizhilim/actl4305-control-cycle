library(tidymodels)
library(themis)

run_glm_freq_prop <- function(geo_code_vec) {
  ## 1. Creating Data Split for Modelling ####
  # Need to separate out Prop vs PropLoI
  source("03 Preparation/03 split_by_LoI.R")
  PropClaims <- split_by_LoI(policy_claims, LossofIncome_cover = FALSE,
                             geo_code_vec)
  
  set.seed(123)
  data_split_prop <- initial_split(PropClaims, prop = 0.8, strata = has_claim)
  
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
             has_claim + state + date_weights,
           data = Analysis)%>%
    update_role(exposure, new_role = "offset")%>%
    update_role(date_weights, new_role = "weights")%>%
    update_role(has_claim, new_role = "rebalancing")%>%
    update_role(state, new_role = "new_factors")%>%
    
    ## Logging predictors
    step_log(suminsured_prop, exposure)%>%
    step_zv(all_predictors())%>%
    
    ## Over-sampling for class imbalance
    step_mutate(has_claim = as_factor(has_claim))%>%
    step_rose(has_claim, seed = 123)%>%
    step_rm(has_claim)%>%
    
    ## Handling of new levels
    step_mutate(geo_code = fct_drop(geo_code))%>%
    step_novel(geo_code, new_level = "CHANGE_TO_STATE")%>%
    
    step_mutate(geo_code = as_factor(if_else(
      geo_code == "CHANGE_TO_STATE",
      as.character(state),
      as.character(geo_code)
    )))
  
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
  browser()
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
