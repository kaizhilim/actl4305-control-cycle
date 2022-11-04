source("01 Data Cleaning/Data Cleaning Interface.R")
source("03 Preparation/01 Data Standardization Interface.R")
source("03 Preparation/02 Geo Code Interface.R")

## 1. Data Cleaning ####
ass_data = read_csv("Assignment Data/Assignment Data.csv")
ass_clean_na = data_cleaning_interface(ass_data)
# save(ass_clean_na, file = "00 envr/Cleaning/ass_clean_na.Rda")
rm(ass_data)

## 2. Data Standardisation ####
# Environment Management
init_dir = ls()

policy_claims<- data_standardization_interface(ass_clean_na)

rm(list = setdiff(ls(), c(init_dir, 'policy_claims')))
rm(init_dir)

save(policy_claims, file = "00 envr/Compulsory/policy_claims.Rda")

## 3. Geo Code Preparation ####
# load("00 envr/Compulsory/policy_claims.Rda")
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

policy_claims_grid = geo_code_interface(geo_code)