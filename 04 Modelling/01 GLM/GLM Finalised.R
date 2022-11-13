#### Set Up ####

# source("main.R")
source("00 source.R")
source("03 Preparation/Data Split.R")
load("00 envr/Compulsory/policy_claims.Rda")

library(tidymodels)
library(dplyr)
library(glmnet)
library(MASS)
library(boot)
require(statmod)
library(insurancerating)
library(caret)

training_data$state <- factor(training_data$state,
                              levels= c("WA","ACT","NSW","NT","QLD","SA","TAS","VIC"))

policy_freq_bi <- training_data %>% filter(suminsured_lossofinc > 0) # for bi frequency #
policy_freq_prop <- training_data %>% filter(suminsured_prop > 0) # for prop frequency #
policy_sev_bi <- training_data %>% filter(grossincurred_lossofinc > 0) # for bi severity #
policy_sev_prop <- training_data %>% filter(grossincurred_prop > 0) # for prop severity #

#### Models ####

# Frequency #

# Model 1- Poisson GLM #
freqmodelbi <- glm(
  claimcount_lossofinc ~ log(suminsured_lossofinc) + geo_code + 
    building_age + building_type + construction_walls + construction_floor +
    sprinkler_type + occupation_risk,
  data = policy_freq_bi,
  family=poisson(link = "log"),
  offset = log(exposure),
  weights = date_weights
)

freqmodelprop <- glm(
  claimcount_prop ~ log(suminsured_prop) + geo_code + 
    building_age + building_type + construction_walls + construction_floor +
    sprinkler_type + occupation_risk,
  data = policy_freq_prop,
  family=poisson(link = "log"),
  offset = log(exposure), 
  weights = date_weights
)

# Model 2- NB GLM #
freqmodelbi_NB<-glm.nb(
  claimcount_lossofinc ~ offset(log(exposure)) + log(suminsured_lossofinc) + geo_code + 
    building_age + building_type + construction_walls + construction_floor +
    sprinkler_type + occupation_risk,
  data = policy_freq_bi,
  weights = date_weights
)

freqmodelprop_NB<-glm.nb(
  claimcount_prop ~ offset(log(exposure)) + log(suminsured_prop) + geo_code +
    building_age + building_type + construction_walls + construction_floor +
    sprinkler_type + occupation_risk,
  data = policy_freq_prop,
  weights = date_weights,
  link = "log"
)

# Severity#

# Model 1- Gamma #
sevmodelprop <- glm(
  grossincurred_prop/claimcount_prop ~ log(suminsured_prop) + state + building_age +
    building_type + construction_walls + construction_floor +
    sprinkler_type + occupation_risk,
  data = policy_sev_prop,
  family=Gamma(link = "log"),
  weights = date_weights
)

sevmodelbi <- glm(
  grossincurred_lossofinc/claimcount_lossofinc ~ log(suminsured_lossofinc) + state + building_age +
    building_type + construction_floor + # Note that construction wall is removed
    sprinkler_type + occupation_risk,
  data = policy_sev_bi,
  family=Gamma(link = "log"),
  weights = date_weights
) 

#### Model Predictions ####

# Removing test data that contain factors not present in training data #
test_data <- test_data %>% 
  filter(!(state %in% c("ACT","NT"))) %>%
  filter(occupation_risk != "O_Other Community Services")

# Model predictions using each GLM model #
model_predictions <- test_data %>%
  add_prediction(freqmodelbi) %>%
  add_prediction(freqmodelbi_NB) %>%
  add_prediction(freqmodelprop) %>%
  add_prediction(freqmodelprop_NB) %>%
  left_join(
    test_data %>%
      mutate(prop_sev = predict(sevmodelprop, newdata = test_data, type = "response")) %>%
      dplyr::select(policyno, situation_num, policy_version, prop_sev),
    by=c("policyno" = "policyno",
         "situation_num" = "situation_num",
         "policy_version" = "policy_version")
  ) %>%
  left_join(
    test_data %>% 
      mutate(bi_sev = predict(sevmodelbi, newdata = test_data, type = "response")) %>%
      dplyr::select(policyno, situation_num, policy_version, bi_sev),
    by=c("policyno" = "policyno", "situation_num" = "situation_num", "policy_version" = "policy_version")
  ) %>%
  rename(
    bi_freq_pois = pred_claimcount_lossofinc_freqmodelbi,
    bi_freq_NB = pred_claimcount_lossofinc_freqmodelbi_NB,
    prop_freq_pois = pred_claimcount_prop_freqmodelprop,
    prop_freq_NB = pred_claimcount_prop_freqmodelprop_NB) %>%
  mutate(bi_freq_NB = if_else(LossofIncome_cover,bi_freq_NB,0), # BI frequency and severity is 0 if there is no Loss of Income cover #
         bi_freq_pois = if_else(LossofIncome_cover,bi_freq_pois,0),
         bi_sev = if_else(LossofIncome_cover,bi_sev,0))

loi_predictions <- model_predictions %>%
  filter(LossofIncome_cover)

no_loi_predictions <- model_predictions %>%
  filter(!LossofIncome_cover)

# Creating dataframes of different combinations of models for frequency and 
# severity to map for interaction modelling # 
df1 <- loi_predictions %>%
  mutate(diff = grossincurred_prop + grossincurred_lossofinc - 
           bi_freq_pois*bi_sev - prop_freq_pois*prop_sev) %>%
  dplyr::select(
    bi_freq_pois,
    prop_freq_pois,
    bi_sev,
    prop_sev,
    diff
  ) %>%
  rename(bi_freq = bi_freq_pois,
         prop_freq = prop_freq_pois)

df2 <- loi_predictions %>%
  mutate(diff = grossincurred_prop + grossincurred_lossofinc - 
           bi_freq_pois*bi_sev - prop_freq_NB*prop_sev) %>%
  dplyr::select(
    bi_freq_pois,
    prop_freq_NB,
    bi_sev,
    prop_sev,
    diff
  ) %>%
  rename(bi_freq = bi_freq_pois,
         prop_freq = prop_freq_NB)

df3 <- loi_predictions %>%
  mutate(diff = grossincurred_prop + grossincurred_lossofinc - 
           bi_freq_NB*bi_sev - prop_freq_pois*prop_sev) %>%
  dplyr::select(
    bi_freq_NB,
    prop_freq_pois,
    bi_sev,
    prop_sev,
    diff
  ) %>%
  rename(bi_freq = bi_freq_NB,
         prop_freq = prop_freq_pois)

df4 <- loi_predictions %>%
  mutate(diff = grossincurred_prop + grossincurred_lossofinc - 
           bi_freq_NB*bi_sev - prop_freq_NB*prop_sev) %>%
  dplyr::select(
    bi_freq_NB,
    prop_freq_NB,
    bi_sev,
    prop_sev,
    diff
  ) %>%
  rename(bi_freq = bi_freq_NB,
         prop_freq = prop_freq_NB)

df_list <- list(df1,df2,df3,df4)

#### Interaction Modelling ####

model_list <- df_list %>% map(~ glm(
  diff ~ bi_freq:prop_freq + bi_sev:prop_sev + bi_freq:bi_sev:prop_freq:prop_sev,
  data = .,
  family = gaussian)
)

model_summary = data.frame(model = c(1:4))

for (i in c(1:4)){
  model_summary$MSE[i] <- cv.glm(df_list[[i]],model_list[[i]],K=10)$delta[2]
  model_summary$RSq = 1- model_list[[i]]$deviance/model_list[[i]]$null.deviance
}

model_summary <- model_summary %>%
  mutate(RMSE = sqrt(MSE)) # Selected Model 4 as the best model #

#### Computing Test Error ####

final_pred <- no_loi_predictions %>%
  mutate(interaction = 0) %>%
  union(loi_predictions %>%
          mutate(interaction = predict(model_list[[4]], newdata= df4, type = "response"))
  )

final_pred %>% count(LossofIncome_cover,interaction != 0) # Check that number of LoI covers match dataset- True #

final_RMSE_int <- final_pred %>%
  mutate(error = 
           (grossincurred_prop + grossincurred_lossofinc -
           (prop_freq_NB * prop_sev +
           bi_freq_NB * bi_sev +
           interaction))^2) %>%
  summarise(MSE = mean(error),
            RMSE = sqrt(mean(error)))

final_RMSE_no_int <- final_pred %>%
  mutate(error = 
           (grossincurred_prop + grossincurred_lossofinc -
              (prop_freq_NB * prop_sev +
                 bi_freq_NB * bi_sev))^2) %>%
  summarise(MSE = mean(error),
            RMSE = sqrt(mean(error)))

final_RMSE_prop <- final_pred %>%
  mutate(error = 
           (grossincurred_prop -
              prop_freq_NB * prop_sev)^2) %>%
  summarise(MSE = mean(error),
            RMSE = sqrt(mean(error)))

final_RMSE_bi <- final_pred %>%
  mutate(error = 
           (grossincurred_lossofinc - bi_freq_NB * bi_sev)^2) %>%
  summarise(MSE = mean(error),
            RMSE = sqrt(mean(error)))

#### Variable Importance Plots ####

VIP_plot <- function(model, title){
  vimp <- varImp(model,scale=F)
  results <- data.frame(VariableName = row.names(vimp), Weight = vimp$Overall) %>%
    arrange(Weight) %>%
    slice_tail(n=10)
  results$VariableName <- factor(results$VariableName,
                                         levels = results$VariableName[order(results$Weight)])
  
  plot <- results %>%
    ggplot(aes(x=VariableName,y=Weight)) +
    geom_col(aes(fill=Weight)) +
    labs(title = paste0("Variable Importance Plot for ",title), x = "Importance") +
    scale_fill_gradient2(low = "purple",
                         high = "blue") +
    theme_classic() +
    theme(axis.text = element_text(size=12)) +
    coord_flip()
    
  return(plot)
}

par(mfrow=c(2,2))

VIP_plot(glm_freq_loi_strd, title = "LoI Frequency")
VIP_plot(freqmodelprop_NB,title="Property Frequency")
VIP_plot(sevmodelbi,title="LoI Severity")
VIP_plot(sevmodelprop,title="Property Severity")

#### Rating Factors ####

rating_factor_plots <- function(model, index1, index2) {
  model_subset <- model
  model_subset$coefficients <- model_subset$coefficients[c(1:(index1-1), (index1+1):(index2-1), (index2+1):length(model$coefficients))]
  model_ratingfactors <- rating_factors(model_subset) # just an exponential of the coefficient
  print(model_ratingfactors)
  autoplot(model_ratingfactors)
}

rating_factor_plots(freqmodelbi_NB, 133, 136)
rating_factor_plots(freqmodelprop_NB, 133, 136)
rating_factor_plots(sevmodelbi, 14, 20)
rating_factor_plots(sevmodelprop, 22, 25)

