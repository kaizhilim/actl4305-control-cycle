library(tidymodels)
library(themis)

fit_resamples_glm_freq_prop<-function(data_split_prop) {
  
  
  
}




## 2. Add geo_code
source("04 Modelling/00 Scripts/01 add_geo_code.R")
claims_finalized <- claims_finalized%>%
  add_geo_code(geo_code_encoding_vec)


training_data_prop <- training(data_split_prop)
testing_data_prop <- testing(data_split_prop)


# Property + Loi
PropLoIClaims<- split_by_LoI(policy_claims, LossofIncome_cover = TRUE, 
                             example_geo_code)

set.seed(123)
data_split_proploi <- initial_split(PropLoIClaims, prop = 0.8, 
                                    strata = has_claim)

training_data_proploi <- training(data_split_proploi)
testing_data_proploi <- testing(data_split_proploi)


#####
# RUN MODELSSSS
## 3. The Data Frames are now ready for modelling ####
# Of course, data splitting would be required
run_example = FALSE
if(run_example) {
  
  set.seed(123)
  validation_prop <- validation_split(training_data_prop, prop =0.8, 
                                      strata = has_claim)
  
  ## 1. Extract Split Components ####
  Analysis_prop = analysis(validation_prop$splits[[1]])
  Assess_prop = assessment(validation_prop$splits[[1]])
  
  glm_freq_prop_pois <- glm(
    formula = claimcount_prop ~ log(suminsured_prop) + geo_code +
      building_age + building_type +
      construction_walls + construction_floor +
      sprinkler_type + occupation_risk,
    data = Analysis_prop,
    family=poisson(link = "log"),
    offset = log(exposure), weights = date_weights)
  
  pred_glm_freq_prop_pois <- Assess_prop%>%
    add_prediction(glm_freq_prop_pois)
  
  run_proploi_example = FALSE
  if (run_proploi_example) {
    
    set.seed(123)
    validation_proploi <- validation_split(training_data_proploi, prop =0.5, 
                                           strata = has_claim)
    
    ## 1. Extract Split Components ####
    Analysis_proploi = analysis(validation_proploi$splits[[1]])
    Assess_proploi = assessment(validation_proploi$splits[[1]])
    
    glm_freq_proploi_pois <- glm(
      formula = claimcount_proploi ~ log(suminsured_proploi) + geo_code +
        building_age + building_type +
        construction_walls + construction_floor +
        sprinkler_type + occupation_risk,
      data = Analysis_proploi,
      family=poisson(link = "log"),
      offset = log(exposure), weights = date_weights)
    
    pred_glm_freq_proploi_pois <- Assess_proploi%>%
      add_prediction(glm_freq_proploi_pois)
  }
}

"
x_lossofinc ~ log(suminsured_lossofinc) + indem_per_grp + occupation_risk
x_lossofinc ~ log(suminsured_lossofinc) + indem_per_grp + occupation_risk + geo_code

set.seed(123)
halfhalf_proploi <- vfold_cv(training_data_proploi, v = 2, strata = has_claim)

"