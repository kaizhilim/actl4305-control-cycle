library(tidymodels)
library(themis)

run_glm_sev_prop <- function(geo_code_vec) {
  ## 1. Creating Data Split for Modelling ####
  # Need to separate out Prop vs PropLoI
  PropSeverity <- policy_claims%>%
    filter(!LossofIncome_cover, grossincurred_prop > 0)%>%
    select(-contains(c("lossofinc", "loi", "indem")))%>%
    filter(suminsured_prop > 0)
  
  set.seed(123)
  data_split_prop <- initial_split(PropSeverity, prop = 0.8)
  
  ## 0. Validation Set ####
  training_data_prop <- training(data_split_prop)
  set.seed(123)
  validation_prop <- validation_split(training_data_prop, prop = 0.8)
  
  ## 1. Extract Split Components ####
  Analysis = analysis(validation_prop$splits[[1]])
  Assess = assessment(validation_prop$splits[[1]])
  
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
  
  ## a. Gam
  
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
