#### Set Up ####

source("main.R")
source("00 source.R")
source("03 Preparation/Data Split.R")
load("00 envr/Compulsory/policy_claims.Rda")


library(tidymodels)
library(dplyr)
library(glmnet)
library(MASS)
library(boot)
library(pscl)
require(statmod)


policy_freq_bi <- training_data %>% filter(suminsured_lossofinc > 0)
policy_freq_prop <- training_data %>% filter(suminsured_prop > 0)
policy_sev_bi <- training_data %>% filter(grossincurred_lossofinc > 0)
policy_sev_prop <- training_data %>% filter(grossincurred_prop > 0)

#### Models ####

# Frequency #

freqmodelbi <- glm(
  claimcount_lossofinc ~ log(suminsured_lossofinc) + geo_code + occupation_risk,
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
  offset = log(exposure), weights = date_weights
)

freqmodelbi_NB<-glm.nb(
  claimcount_lossofinc ~ offset(log(exposure)) + log(suminsured_lossofinc) + geo_code + 
    occupation_risk,
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

# Severity #

sevmodelprop <- glm(
  grossincurred_prop/claimcount_prop ~ log(suminsured_prop) + state + building_age +
    building_type + construction_walls + construction_floor +
    sprinkler_type + occupation_risk,
  data = policy_sev_prop,
  family=Gamma(link = "log"),
  offset = log(exposure), weights = date_weights
)

sevmodelbi <- glm(
  grossincurred_lossofinc/claimcount_lossofinc ~ log(suminsured_lossofinc) +
    state + occupation_risk,
  data = policy_sev_bi,
  family=Gamma(link = "log"),
  offset = log(exposure), weights = date_weights
)

# Predictions #

test_data <- test_data %>%
  filter(!(state %in% c("ACT","NT"))) %>%
  filter(occupation_risk != "O_Other Community Services")

predict(freqmodelbi, newdata = test_data, type = "response") %>% summary()
predict(freqmodelprop, newdata = test_data, type = "response") %>% summary()

test_data$claimcount_prop %>% summary()
test_data$claimcount_lossofinc %>% summary()


# predict(freqmodelprop, newdata = test_data, type = "response")
# predict(freqmodelprop_NB, newdata = test_data, type = "response")
# predict(sevmodelbi,newdata = test_data, type = "response")
# predict(sevmodelprop, newdata = test_data, type = "response")

list_dfs <- list(
  predict(freqmodelbi, newdata = test_data, type = "response"),
  predict(freqmodelbi_NB, newdata = test_data, type = "response"),
  predict(freqmodelprop, newdata = test_data, type = "response"),
  predict(freqmodelprop_NB, newdata = test_data, type = "response"),
  predict(sevmodelbi, newdata = test_data, type = "response"),
  predict(sevmodelprop, newdata = test_data, type = "response")
)

nms <- c('bi_freq_pois', 'bi_freq_nb', 'prop_freq_pois', 'prop_freq_nb',
         'prop_sev', 'bi_sev')

list2env(setNames(list_dfs, nms), envir = .GlobalEnv)

df1 <- data.frame(
  bi_freq = bi_freq_pois,
  prop_freq = prop_freq_pois,
  bi_sev = bi_sev,
  prop_sev = prop_sev,
  actual = test_data$grossincurred_prop + test_data$grossincurred_lossofinc
)

df2 <- data.frame(
  bi_freq = bi_freq_nb,
  prop_freq = prop_freq_pois,
  bi_sev = bi_sev,
  prop_sev = prop_sev,
  actual = test_data$grossincurred_prop + test_data$grossincurred_lossofinc
)

df3 <- data.frame(
  bi_freq = bi_freq_pois,
  prop_freq = prop_freq_nb,
  bi_sev = bi_sev,
  prop_sev = prop_sev,
  actual = test_data$grossincurred_prop + test_data$grossincurred_lossofinc
)

df4 <- data.frame(
  bi_freq = bi_freq_nb,
  prop_freq = prop_freq_nb,
  bi_sev = bi_sev,
  prop_sev = prop_sev,
  actual = test_data$grossincurred_prop + test_data$grossincurred_lossofinc
)

df_list <- list(df1,df2,df3,df4)

model_list <- df_list %>% map(~ glm(
  actual ~ bi_freq:bi_sev + prop_freq:prop_sev + bi_freq:bi_sev:prop_freq:prop_sev,
  data = .,
  family = gaussian)
)

model_summary = data.frame(model = c(1:4))

for (i in c(1:4)){
  model_summary$MSE[i] <- cv.glm(df_list[[i]],model_list[[i]],K=10)$delta[2]
  model_summary$RSq = 1- model_list[[i]]$deviance/model_list[[i]]$null.deviance
}

model_summary

rmse(df1$bi_freq,test_data$claimcount_lossofinc) 
sqrt(mean((df1$bi_freq - test_data$claimcount_lossofinc)^2))
sqrt(mean((df1$prop_freq - test_data$claimcount_prop)^2))
sqrt(mean((df1$bi_sev - test_data$grossincurred_lossofinc)^2))
sqrt(mean((df1$prop_sev - test_data$grossincurred_prop)^2))

df1$prop_freq[test_data$claimcount_prop >= 2]
test_data$claimcount_prop[test_data$claimcount_prop >= 2]

mean(test_data$claimcount_prop[test_data$claimcount_prop > 0]/df1$prop_freq[test_data$claimcount_prop > 0])


# Zero-Inflated #

policy_freq_prop_test <- policy_freq_prop %>%
  mutate(suminsured_lossofinc = log(suminsured_lossofinc),
         suminsured_prop = log(suminsured_prop))

indexfreqprop<-c(which(names(policy_freq_prop_test)=="suminsured_prop"),
                 which(names(policy_freq_prop_test)=="geo_code"),
                 which(names(policy_freq_prop_test)=="building_age"),
                 which(names(policy_freq_prop_test)=="building_type"),
                 which(names(policy_freq_prop_test)=="construction_walls"),
                 which(names(policy_freq_prop_test)=="construction_floor"),
                 which(names(policy_freq_prop_test)=="sprinkler_type"),
                 which(names(policy_freq_prop_test)=="occupation_risk"))
indexzeroprop<-c(which(names(policy_freq_prop_test)=="suminsured_prop"),
                 which(names(policy_freq_prop_test)=="geo_code"),
                 which(names(policy_freq_prop_test)=="occupation_risk"))
n<-length(indexfreqprop)+1
m<-length(indexzeroprop)+1
x2<-cbind(1,policy_freq_prop_test[,indexfreqprop])
z<-cbind(1,policy_freq_prop_test[,indexzeroprop])

zeroBC<-zeroinfl(claimcount_prop~ suminsured_prop + geo_code + 
                   building_age + building_type + construction_walls + construction_floor +
                   sprinkler_type + occupation_risk | suminsured_prop + geo_code + 
                   occupation_risk,
                 data=policy_freq_prop_test,dist = c("poisson"),link = c("logit"))
gamma<-zeroBC$coefficients$zero
beta<-zeroBC$coefficients$count
pzeroBC<-exp(as.matrix(z)%*%gamma)/(1+exp(as.matrix(z)%*%gamma)) # probability of 0 claims #
meanpoissonBC<-exp(as.matrix(x2)%*%beta) # Poisson lambdas #
exp0<-rep(0,numrow)
exp0[1]<-sum(pzeroBC+(1-pzeroBC)*dpois(0,meanpoissonBC)) 
for(i in 2:numrow){
  exp0[i]<-sum((1-pzeroBC)*dpois((i-1),meanpoissonBC))
}

predict(zeroBC, newdata = test_data %>%
          mutate(suminsured_lossofinc = log(suminsured_lossofinc),
                 suminsured_prop = log(suminsured_prop)), type = "response") %>%
  summary()
