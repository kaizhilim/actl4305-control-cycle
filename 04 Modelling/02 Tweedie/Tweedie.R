#### Set Up ####

library(tweedie)
library(statmod)

# this creates an interaction data set (i.e. the 20%) that can be used to build the interaction model later
set.seed(123)
data_split <- initial_split(training_data, prop = 0.75, strata = LossofIncome_cover)
training_data <- training(data_split)
training_data_interaction <- testing(data_split)

training_data_prop <- training_data %>% filter(suminsured_prop > 0) 
training_data_bi <- training_data %>% filter(suminsured_lossofinc > 0) 
# dim(training_data)
# dim(training_data[which(training_data$LossofIncome_cover == TRUE), ])
# dim(training_data[which(training_data$suminsured_lossofinc > 0), ])
# dim(training_data[which(training_data$LossofIncome_cover == TRUE & training_data$suminsured_lossofinc > 0), ])

glm_tweedie <- function(formula, data = training_data, method = "series") {
  profile <- tweedie.profile(formula, data = data, 
                             xi.vec = seq(1.1, 1.9, length = 9), method = method, do.ci = F)
  print(profile$xi.max)
  model <- glm(formula, data = data, weights = date_weights,
               family = tweedie(var.power = profile$xi.max, link.power = 0))
  return(model)
}

#### Models ####

# Property #

formula_prop <- grossincurred_prop ~ 
  log(suminsured_prop) + geo_code +
  building_age + building_type + 
  construction_walls + construction_floor +
  sprinkler_type + occupation_risk
modelprop_tw <- glm_tweedie(formula_prop, data = training_data_prop)
summary(modelprop_tw)

# Business Interruption #

formula_bi <- grossincurred_lossofinc ~ 
  log(suminsured_lossofinc) + geo_code +
  building_age + building_type + 
  construction_walls + construction_floor +
  sprinkler_type + occupation_risk
modelbi_tw <- glm_tweedie(formula_bi, data = training_data_bi)
summary(modelbi_tw)

# Interaction #

prop_tw <- predict(modelprop_tw, newdata = training_data_interaction, type = "response")
bi_tw <- predict(modelbi_tw, newdata = training_data_interaction, type = "response")

formula_tw <- grossincurred_prop + grossincurred_lossofinc ~ 
  prop_tw + bi_tw + 
  prop_tw:bi_tw
model_tw <- glm(formula_tw, data = training_data_interaction, family = gaussian)
summary(model_tw)

model_pred_tw <- predict(model_tw, newdata = test_data, type = "response")
sqrt(sum((model_pred_tw-(test_data$grossincurred_prop+test_data$grossincurred_lossofinc))^2)/nrow(test_data))


# Danny #

model_predictions <- test_data %>%
  add_prediction(modelprop_tw) %>%
  add_prediction(modelbi_tw) %>%
  rename(
    prop_pred = pred_grossincurred_prop_modelprop_tw,
    bi_pred = pred_grossincurred_lossofinc_modelbi_tw) %>%
  mutate(bi_pred = if_else(LossofIncome_cover,bi_pred,0),
         diff = grossincurred_prop + grossincurred_lossofinc - 
           prop_pred - bi_pred)

loi_predictions <- model_predictions %>%
  filter(LossofIncome_cover)
  

no_loi_predictions <- model_predictions %>%
  filter(!LossofIncome_cover)

interaction_model <- glm(
  diff ~ bi_pred:prop_pred,
  data = loi_predictions,
  family = gaussian)

final_pred_tw <- no_loi_predictions %>%
  mutate(interaction = 0) %>%
  union(loi_predictions %>%
          mutate(interaction = predict(interaction_model,
                                       newdata= loi_predictions,
                                       type = "response"))
  )

final_pred_tw %>%
  mutate(error = 
           (grossincurred_prop + grossincurred_lossofinc -
              (prop_pred * bi_pred + interaction))^2) %>%
  summarise(MSE = mean(error),
            RMSE = sqrt(mean(error)))
