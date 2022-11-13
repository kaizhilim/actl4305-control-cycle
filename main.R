## 00. Environment Management ####
source("00 source.R")
init_dir = ls()

## 1. Data Cleaning ####
reload_ass_data = FALSE
if(reload_ass_data) {
  
  # Cleaning
  source("01 Data Cleaning/Data Cleaning Interface.R")
  rm(ass_data)

  ## Data Prep - Aggregation
  source("03 Preparation/01 Data Standardization Interface.R")

} else {
  load("00 envr/Compulsory/ass_rfct.Rda")
  load("00 envr/Compulsory/policy_claims.Rda")
}

## 2. Data Split ####
source("03 Preparation/02 Data Split.R")

## 3. Run Models ####
run_glm_ensemble = FALSE
if (run_glm_ensemble) {
  source("04 Modelling/01 GLM/GLM Ensemble.R")
} 

run_tweedie = FALSE
if (run_tweedie) {
  source("04 Modelling/02 Tweedie/Tweedie.R")
}

## 4. Geo Code Encoding in Detail ####
run_geo_code_grid = FALSE
if (run_geo_code_grid) {
  source("05 geo_code modelling/run_geo_code.R")
}

rm(list = setdiff(ls(), c(init_dir, 'ass_rfct', 'policy_claims', 'geo_code_grid')))
