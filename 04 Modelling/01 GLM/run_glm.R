source("main.R")

stopifnot(exists("geo_code_grid"))

## Run for (freq, sev) of (prop, proploi, loi) ####

## (i) freq_prop ####
source("04 Modelling/01 GLM/Execution/glm_freq_prop.R")
glm_freq_prop_grid <- geo_code_grid%>%
  slice_head(n=1)%>%
  mutate(family_rmse = map(geo_code_encoding, run_glm_freq_prop))%>%
  mutate(
    family = map_chr(family_rmse, ~names(.)),
    rmse = map_dbl(family_rmse, ~.))%>%
  arrange(rmse, n_geo_code)

save(glm_freq_prop_grid, file = "00 envr/Modelling/GLM/glm_freq_prop_grid.Rda")

## (ii) sev_prop ####
# source("04 Modelling/01 GLM/Execution/glm_sev_prop.R")
