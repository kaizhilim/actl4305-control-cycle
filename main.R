## 00. Environment Management ####
source("00 source.R")
init_dir = ls()

reload_ass_data = FALSE

if(reload_ass_data) {
  source("01 Data Cleaning/Data Cleaning Interface.R")
  source("03 Preparation/01 Data Standardization Interface.R")
  
  ## 1. Data Cleaning ####
  ass_data = read_csv("Assignment Data/Assignment Data.csv")
  ass_clean_na = data_cleaning_interface(ass_data)
  # save(ass_clean_na, file = "00 envr/Cleaning/ass_clean_na.Rda")
  rm(ass_data)
  
  ## 2. Data Standardisation ####
  policy_claims<- data_standardization_interface(ass_clean_na)
  
  save(policy_claims, file = "00 envr/Compulsory/policy_claims.Rda")
} else {
  load("00 envr/Compulsory/policy_claims.Rda")
}

## 3. Geo Code Preparation ####
reload_geo_code_grid = FALSE
if (reload_geo_code_grid) {
  source("03 Preparation/02 Geo Code Interface.R")
  geo_code_grid = 
    expand_grid(
      threshold_riskpostcode = seq(10, 50, 5),
      sa3name_multiplier = seq(1.5, 3, 0.5),
      sa4name_multiplier = seq(1.5, 3, 0.5)
    )%>%
    transmute(
      threshold_riskpostcode,
      threshold_sa3name = threshold_riskpostcode * sa3name_multiplier,
      threshold_sa4name = threshold_sa3name * sa4name_multiplier
    )
  
  geo_code_grid = geo_code_interface(geo_code_grid, 
                                     policy_claims = policy_claims)
  
  save(geo_code_grid, file = "00 envr/Compulsory/geo_code_grid.Rda")
} else {
  load("00 envr/Compulsory/geo_code_grid.Rda")
}


rm(list = setdiff(ls(), c(init_dir, 'policy_claims', 'geo_code_grid')))
