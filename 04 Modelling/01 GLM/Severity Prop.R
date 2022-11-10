source("00 source.R")
load("00 envr/Compulsory/policy_claims.Rda")

library(tidymodels)
library(dplyr)
library(glmnet)
library(MASS)

#### Set Up ####

policy_sev_prop <- policy_claims %>% filter(grossincurred_prop > 0)

#### Gamma ####

sevmodelprop <- glm(
  grossincurred_prop/claimcount_prop ~ log(suminsured_prop) + state + building_age +
    building_type + construction_walls + construction_floor +
    sprinkler_type + occupation_risk,
  data = policy_sev_prop,
  family=Gamma(link = "log"),
  offset = log(exposure), weights = date_weights
)

beta_gam_prop<-sevmodelprop$coefficients #coefficients of GLM 1
mean_gam_prop<-sevmodelprop$fitted.values #fitted values of Lambda: E(Y)

# Anova #

sevmodelprop_anova<-glm(claimcount_prop ~. -exposure, data = policy_sev_prop_anova,
                         family=poisson(link = "log"),offset = exposure)

anova(sevmodelprop,test="Chisq") #anova test

# SDR vs FV #

par(mfrow=c(1,2))
plot(sevmodelprop$fitted.values,rstandard(sevmodelprop),
     xlab="Fitted values",ylab="Standardized deviance residuals",
     ylim=c(-5,5),
     main="SDR vs FV, Poisson")
abline(h=0,col="red",lty=2)
plot(sevmodelprop_qp$fitted.values,rstandard(sevmodelprop_qp),
     xlab="Fitted values",ylab="Standardized deviance residuals",
     ylim=c(-5,5),
     main="SDR vs FV, Quasipoisson")
abline(h=0,col="red",lty=2)

# QQ Plot #

qqnorm(rstandard(sevmodelprop))
qqline(rstandard(sevmodelprop),col="red")


#### Inv Gaus ####

sevmodelprop_ig <- glm(
  grossincurred_prop/claimcount_prop ~ log(suminsured_prop) + state + building_age +
    building_type + construction_walls + construction_floor +
    sprinkler_type + occupation_risk,
  data = policy_sev_prop,
  family=inverse.gaussian(link = "log"),
  offset = log(exposure), weights = date_weights
)

beta_ig_prop<-sevmodelprop_ig$coefficients #coefficients of GLM 1
mean_ig_prop<-sevmodelprop_ig$fitted.values #fitted values of Lambda: E(Y)

# SDR vs FV #

par(mfrow=c(1,2))
plot(sevmodelprop_ig$fitted.values,rstandard(sevmodelprop_ig),
     xlab="Fitted values",ylab="Standardized deviance residuals",
     ylim=c(-5,5),
     main="SDR vs FV, Poisson")
abline(h=0,col="red",lty=2)

# QQ Plot #

qqnorm(rstandard(sevmodelprop_ig))
qqline(rstandard(sevmodelprop_ig),col="red")