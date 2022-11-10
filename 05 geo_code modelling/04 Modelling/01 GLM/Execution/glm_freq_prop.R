library(tidymodels)
library(themis)

run_glm_freq_prop <- function(training_data_prop, geo_code_vec) {
  browser()
  
  ## 0. Validation Set ####
  set.seed(123)
  validation_prop <- validation_split(training_data_prop, 
                                      prop = 0.8, strata = has_claim)
  
  ## 1. Extract Split Components ####
  Analysis = analysis(validation_prop$splits[[1]])
  Assess = assessment(validation_prop$splits[[1]])
  
  ## 2. Manual Recipe ####
  glm_freq_prop_recipe <- 
    recipe(claimcount_prop ~ exposure + suminsured_prop +
             building_age + building_type + 
             construction_walls + construction_floor +
             sprinkler_type + occupation_risk + 
             has_claim + 
             riskpostcode + state + 
             date_weights,
           data = Analysis)%>%
    update_role(exposure, new_role = "offset")%>%
    update_role(has_claim, new_role = "rebalancing")%>%
    update_role(riskpostcode, new_role = "gen_geo_code")%>%
    update_role(state, new_role = "new_factors")%>%
    
    ## Logging predictors
    step_log(suminsured_prop, exposure)%>%
    step_zv(all_predictors())%>%
    
    ## Over-sampling for class imbalance
    step_mutate(has_claim = as_factor(has_claim))%>%
    step_rose(has_claim, seed = 123)%>%
    
    ## Generating geo_code 
    step_mutate(geo_code = as_factor(geo_code_vec[riskpostcode]))%>%
    
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
      glm_freq_prop_pois, glm_freq_prop_nb)
  
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
