source("05 geo_code modelling/main.R")
source("05 geo_code modelling/03 Preparation/Scripts/03 geo_code.R")
source("05 geo_code modelling/03 Preparation/03 split_by_LoI.R")


## 1. Example getting geo_code encoding vector ####
# riskpostcode = 10, sa3name = 20, sa4name = 100
# Generate using default parameters
default_geo_code = generate_geo_encoding(policy_claims)

## Or choosing from the parameter grid
example_geo_code = geo_code_grid$geo_code_encoding[[123]]

## 2. Creating Data Frames for Modelling ####
# Need to separate out Prop vs PropLoI
PropClaims <- split_by_LoI(policy_claims, LossofIncome_cover = FALSE, 
                           example_geo_code)

set.seed(123)
data_split_prop <- initial_split(PropClaims, prop = 0.8, strata = has_claim)

training_data_prop <- training(data_split_prop)
testing_data_prop <- testing(data_split_prop)


# Property + Loi
PropLoIClaims<- split_by_LoI(policy_claims, LossofIncome_cover = TRUE, 
                           example_geo_code)

set.seed(123)
data_split_proploiloi <- initial_split(PropLoIClaims, prop = 0.8, 
                                       strata = has_claim)

training_data_proploi <- training(data_split_proploi)
testing_data_proploi <- testing(data_split_proploi)


#####
# RUN MODELS
## 3. The Data Frames are now ready for modelling ####
# Of course, data splitting would be required
run_example = FALSE
if(run_example) {
  
  set.seed(123)
  validation_prop <- validation_split(training_data_prop, prop =0.8, 
                                      strata = has_claim)
  
  ## 1. Extract Split Components ####
  Analysis = analysis(validation_prop$splits[[1]])
  Assess = assessment(validation_prop$splits[[1]])

  glm_freq_prop_pois <- glm(
    formula = claimcount_prop ~ log(suminsured_prop) + geo_code +
      building_age + building_type +
      construction_walls + construction_floor +
      sprinkler_type + occupation_risk,
    data = Analysis,
    family=poisson(link = "log"),
    offset = log(exposure), weights = date_weights)

  pred_glm_freq_prop_pois <- Assess%>%
    add_prediction(glm_freq_prop_pois)

  run_proploi_example = FALSE
  if (run_proploi_example) {
    
    set.seed(123)
    validation_prop <- validation_split(training_data_prop, prop =0.8, 
                                        strata = has_claim)
    
    ## 1. Extract Split Components ####
    Analysis = analysis(validation_proploi$splits[[1]])
    Assess = assessment(validation_proploi$splits[[1]])

    glm_freq_proploi_pois <- glm(
      formula = claimcount_proploi ~ log(suminsured_proploi) + geo_code +
        building_age + building_type +
        construction_walls + construction_floor +
        sprinkler_type + occupation_risk,
      data = Analysis,
      family=poisson(link = "log"),
      offset = log(exposure), weights = date_weights)

    pred_glm_freq_proploi_pois <- Assess%>%
      add_prediction(glm_freq_proploi_pois)
  }
}
