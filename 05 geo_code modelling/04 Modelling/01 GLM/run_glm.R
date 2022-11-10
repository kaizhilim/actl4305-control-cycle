source("05 geo_code modelling/main.R")
source("05 geo_code modelling/03 Preparation/03 split_by_LoI.R")
stopifnot(exists("geo_code_grid"))

library(tidymodels)

## 1. Property ####
PropClaims <- split_by_LoI(policy_claims, LossofIncome_cover = FALSE)
set.seed(123)
data_split_prop <- initial_split(PropClaims, prop = 0.8, strata = has_claim)

training_data_prop <- training(data_split_prop)
testing_data_prop <- testing(data_split_prop)

## (i) freq_prop ####
source("05 geo_code modelling/04 Modelling/01 GLM/Execution/glm_freq_prop.R")
glm_freq_prop_grid <- geo_code_grid%>%
  mutate(family_rmse = map(geo_code_encoding, 
                           run_glm_freq_prop,
                           training_data_prop = training_data_prop))%>%
  mutate(
    family = map_chr(family_rmse, ~names(.)),
    rmse = map_dbl(family_rmse, ~.))%>%
  arrange(rmse)

save(glm_freq_prop_grid, file = "05 geo_code modelling/00 envr/Modelling/GLM/glm_freq_prop_grid.Rda")

## (ii) sev_prop ####
PropSev <- split_by_LoI(policy_claims, LossofIncome_cover = FALSE)%>%
  filter(has_claim)

set.seed(123)
data_split_propSev <- initial_split(PropSev, prop = 0.8)

training_data_propSev <- training(data_split_propSev)
testing_data_propSev <- testing(data_split_propSev)

source("05 geo_code modelling/04 Modelling/01 GLM/Execution/glm_sev_prop.R")
glm_sev_prop <- run_glm_sev_prop(training_data_propSev)

# source("04 Modelling/01 GLM/Execution/glm_sev_prop.R")
