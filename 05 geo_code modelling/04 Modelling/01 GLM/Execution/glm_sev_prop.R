library(tidymodels)
library(themis)

run_glm_sev_prop <- function(training_data_propSev) {
  set.seed(123)
  validation_propSev <- validation_split(
    training_data_propSev, prop = 0.8)
  
  ## 1. Extract Split Components ####
  Analysis = analysis(validation_propSev$splits[[1]])
  Assess = assessment(validation_propSev$splits[[1]])
  
  ## 2. Manual Recipe ####
  glm_sev_prop_recipe <- 
    recipe(grossincurred_prop ~ exposure + suminsured_prop + state + 
             building_age + building_type + 
             construction_walls + construction_floor +
             sprinkler_type + occupation_risk + 
             date_weights,
           data = Analysis)%>%
    update_role(exposure, new_role = "offset")%>%
    update_role(date_weights, new_role = "weights")%>%
    
    ## Logging predictors
    step_log(suminsured_prop, exposure)%>%
    step_zv(all_predictors())%>%
    
    ## Dealing with small factor levels
    # Even state, as the dataset is very small
    step_other(all_nominal_predictors(),
               threshold = 0.25,
               other = "other_values")
  
  glm_sev_prop_prep <- prep(glm_sev_prop_recipe)
  glm_sev_prop_baked <- bake(glm_sev_prop_prep, new_data = NULL)
  
  ## 3. Model Fitting ####
  glm_sev_prop_gamma <- glm(
    formula = formula(glm_sev_prop_prep),
    data = glm_sev_prop_baked,
    family=stats::Gamma(link = "log"),
    offset = exposure, weights = date_weights)
  
  ## 4. Model Prediction ####
  glm_sev_prop_assess = bake(glm_sev_prop_prep, new_data = Assess)
  glm_sev_prop_pred = glm_sev_prop_assess%>%
    insurancerating::add_prediction(
      glm_sev_prop_gamma)
  
  return(glm_sev_prop_pred)
}
