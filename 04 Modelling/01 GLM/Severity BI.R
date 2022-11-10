source("00 source.R")
load("00 envr/Compulsory/policy_claims.Rda")

library(tidymodels)
library(dplyr)
library(glmnet)
library(MASS)

#### Set Up ####

policy_sev_bi <- policy_claims %>% filter(grossincurred_lossofinc > 0)

#### Gamma ####

sevmodelbi <- glm(
  grossincurred_lossofinc/claimcount_lossofinc ~ log(suminsured_lossofinc) + state + building_age +
    building_type + construction_floor + # Note that construction wall is removed
    sprinkler_type + occupation_risk,
  data = policy_sev_bi,
  family=Gamma(link = "log"),
  offset = log(exposure), weights = date_weights
) 

beta_gam_bi<-sevmodelbi$coefficients #coefficients of GLM 1
mean_gam_bi<-sevmodelbi$fitted.values #fitted values of Lambda: E(Y)

# Anova #

anova(sevmodelbi,test="Chisq") #anova test

# SDR vs FV #

par(mfrow=c(1,2))
plot(sevmodelbi$fitted.values,rstandard(sevmodelbi),
     xlab="Fitted values",ylab="Standardized deviance residuals",
     ylim=c(-5,5),
     main="SDR vs FV, Poisson")
abline(h=0,col="red",lty=2)

# QQ Plot #

qqnorm(rstandard(sevmodelbi))
qqline(rstandard(sevmodelbi),col="red")


#### Inv Gaus ####

sevmodelbi_ig <- glm(
  grossincurred_lossofinc/claimcount_lossofinc ~ log(suminsured_lossofinc) + state + building_age +
    building_type + construction_walls + construction_floor +
    sprinkler_type + occupation_risk,
  data = policy_sev_bi,
  family=inverse.gaussian(link = "log"),
  offset = log(exposure), weights = date_weights
)

beta_ig_bi<-sevmodelbi_ig$coefficients #coefficients of GLM 1
mean_ig_bi<-sevmodelbi_ig$fitted.values #fitted values of Lambda: E(Y)

# SDR vs FV #

par(mfrow=c(1,2))
plot(sevmodelbi_ig$fitted.values,rstandard(sevmodelbi_ig),
     xlab="Fitted values",ylab="Standardized deviance residuals",
     ylim=c(-5,5),
     main="SDR vs FV, Poisson")
abline(h=0,col="red",lty=2)

# QQ Plot #

qqnorm(rstandard(sevmodelbi_ig))
qqline(rstandard(sevmodelbi_ig),col="red")