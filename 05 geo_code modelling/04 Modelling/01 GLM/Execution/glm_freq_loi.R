source("05 geo_code modelling/main.R")
source("05 geo_code modelling/03 Preparation/03 split_by_LoI.R")
source("05 geo_code modelling/04 Modelling/00 Scripts/01 add_geo_code.R")
stopifnot(exists("geo_code_grid"))

library(tidymodels)
library(themis)

### 1. Standardize Data ####

geo_code_grid_loi<-geo_code_grid%>%
  filter(n_geo_code == 50 | n_geo_code == 120)

stopifnot("n_geo_code equality not found" = nrow(geo_code_grid_loi)==2)

LoIClaims <- policy_claims%>%
  filter(LossofIncome_cover, suminsured_lossofinc > 0)%>%
  mutate(occupation_risk = fct_drop(occupation_risk),
         has_claim = grossincurred_lossofinc > 0)

### 2. Preprocessing ####
set.seed(123)
data_split_loi <- initial_split(LoIClaims, prop = 0.8, strata = has_claim)

training_data_loi <- training(data_split_loi)
testing_data_loi <- testing(data_split_loi)

geo_code_grid_loi <- geo_code_grid_loi%>%
  mutate(recipe = map(
    geo_code_encoding,
    function(geo_code_vec) {
      recipe(has_claim ~ exposure + suminsured_lossofinc + 
               occupation_risk + 
               riskpostcode + state +
               date_weights,
             data = training_data_loi)%>%
        
        ## Over-sampling for class imbalance
        step_mutate(has_claim = as_factor(has_claim))%>%
        step_upsample(has_claim, over_ratio = 0.05, seed = 123)%>%
        
        ## Then update roles 
        update_role(exposure, new_role = "offset")%>%
        update_role(date_weights, new_role = "importance")%>%
        update_role(has_claim, new_role = "rebalancing")%>%
        update_role(riskpostcode, new_role = "gen_geo_code")%>%
        update_role(state, new_role = "new_factors")%>%
        
        ## Generating geo_code 
        step_mutate(geo_code = geo_code_vec[riskpostcode])%>%
        
        ## Handling of new levels
        step_novel(geo_code, new_level = "CHANGE_TO_STATE")%>%
        step_mutate(geo_code = as.character(geo_code))%>%
        
        step_mutate(geo_code = as_factor(if_else(
          geo_code == "CHANGE_TO_STATE",
          as.character(state),
          geo_code
        )))
    }))

use_tidymodels = FALSE
if(use_tidymodels){
  ### 3. Modelling Functions ####
  glm_freq_loi_fit <- function(glm_freq_loi_recipe) {
    glm_freq_loi_prep <- prep(glm_freq_loi_recipe)
    glm_freq_loi_train <- bake(glm_freq_loi_prep, new_data = NULL)
    
    stopifnot(!anyNA(glm_freq_loi_train))
    
    glm_freq_loi <- MASS::glm.nb(
      claimcount_lossofinc ~ offset(log(exposure)) + 
        log(suminsured_lossofinc) + 
        geo_code+ occupation_risk,
      data = glm_freq_loi_train,
      weights = date_weights
    )
    
    return(glm_freq_loi)
  }
  
  glm_freq_loi_pred <- function(recipe, model){
    glm_freq_loi_prep <- prep(recipe)
    glm_freq_loi_testing <- bake(glm_freq_loi_prep, 
                                 new_data = testing_data_loi)%>%
      select(-riskpostcode)
    
    stopifnot(!anyNA(glm_freq_loi_testing))
    
    pred_tbl<-glm_freq_loi_testing%>%
      insurancerating::add_prediction(model)%>%
      transmute(actual_vec = claimcount_lossofinc,
                pred_vec = pred_claimcount_lossofinc_model)
    
    return(pred_tbl)
  }
  
  glm_freq_loi_rmse <- function(df){
    mse = mean((df$pred_vec - df$actual_vec) ^ 2)
    rmse = sqrt(mse)
    return(rmse)
  }
  
  ### 4. Model GLM ####
  geo_code_grid_loi <- geo_code_grid_loi%>%
    mutate(fit = map(recipe, glm_freq_loi_fit))
  
  geo_code_grid_loi<-geo_code_grid_loi%>%
    mutate(pred = map2(recipe, fit, glm_freq_loi_pred))%>%
    mutate(rmse = map_dbl(pred, glm_freq_loi_rmse))
}

### 5. Benchmark ####
glm_freq_loi_strd <- MASS::glm.nb(
  claimcount_lossofinc ~ offset(log(exposure)) + log(suminsured_lossofinc) +
    geo_code+ occupation_risk,
  data = training_data_loi%>%
    add_geo_code(geo_code_grid_loi$geo_code_encoding[[2]]),
  weights = date_weights
)
glm_freq_loi_strd
summary(glm_freq_loi_strd)

glm_freq_loi_strd_eval <- testing_data_loi%>%
  add_geo_code(geo_code_grid_loi$geo_code_encoding[[2]])%>%
  insurancerating::add_prediction(glm_freq_loi_strd)%>%
  transmute(
    pred_vec = pred_claimcount_lossofinc_glm_freq_loi_strd,
    actual_vec = claimcount_lossofinc
  )

save(glm_freq_loi_strd,
     file = "05 geo_code modelling/04 Modelling/01 GLM/glm_freq_loi_strd.Rda")
