source("00 source.R")
load("00 envr/Compulsory/policy_claims.Rda")

library(tidymodels)
library(dplyr)
library(glmnet)
library(MASS)

#### Set Up ####

policy_freq_bi <- policy_claims %>% filter(suminsured_lossofinc > 0)

#### Pois ####

freqmodelbi <- glm(
  claimcount_lossofinc ~ log(suminsured_lossofinc) + geo_code + state + 
    building_age + building_type + construction_walls + construction_floor +
    sprinkler_type + occupation_risk,
  data = policy_freq_bi,
  family=poisson(link = "log"),
  offset = log(exposure),
  weights = date_weights
)

beta_pos_bi<-freqmodelbi$coefficients #coefficients of GLM 1
mean_pos_bi<-freqmodelbi$fitted.values #fitted values of Lambda: E(Y)

# Quasipoisson #

freqmodelbi_qp <- glm(
  claimcount_lossofinc ~ log(suminsured_lossofinc) + geo_code + state + 
    building_age + building_type + construction_walls + construction_floor +
    sprinkler_type + occupation_risk + indem_per_grp,
  data = policy_freq_bi,
  family=quasipoisson(link = "log"),
  offset = log(exposure), weights = date_weights
)

beta_qp_bi<-freqmodelbi_qp$coefficients #coefficients of GLM 1
mean_qp_bi<-freqmodelbi_qp$fitted.values #fitted values of Lambda: E(Y)

freqmodelbi_qp %>% summary()

# Counts #

numrow<-max(policy_freq_bi$claimcount_bi)+1 #maximum number of claim

emp<-rep(0,numrow)

for(i in 1:numrow){
  emp[i]<-sum(policy_freq_bi$claimcount_bi==(i-1))
}

expp<-rep(0,numrow)->expp_off->expqp

for(i in 1:numrow){
  expp[i]<-sum(dpois((i-1),mean_pos_bi))
  expp_off[i]<-sum(dpois((i-1),mean_pos_bi_off))
  expqp[i]<-sum(dpois((i-1),mean_qp_bi))
}

summary(freqmodelbi_offset)

# Anova #

anova(freqmodelbi,test="Chisq") #anova test

# SDR vs FV #

par(mfrow=c(1,2))
plot(freqmodelbi$fitted.values,rstandard(freqmodelbi),
     xlab="Fitted values",ylab="Standardized deviance residuals",
     ylim=c(-5,5),
     main="SDR vs FV, Poisson")
abline(h=0,col="red",lty=2)
plot(freqmodelbi_qp$fitted.values,rstandard(freqmodelbi_qp),
     xlab="Fitted values",ylab="Standardized deviance residuals",
     ylim=c(-5,5),
     main="SDR vs FV, Quasipoisson")
abline(h=0,col="red",lty=2)

# QQ Plot #

qqnorm(rstandard(freqmodelbi))
qqline(rstandard(freqmodelbi),col="red")


#### NB ####

freqmodelbi_NB<-glm.nb(
  claimcount_lossofinc ~ offset(log(exposure)) + log(suminsured_lossofinc) + geo_code + state + 
    building_age + building_type + construction_walls + construction_floor +
    sprinkler_type + occupation_risk,
  data = policy_freq_bi,
  weights = date_weights
)

beta_nb_bi<-freqmodelbi_NB$coefficients #coefficients of GLM 1
mean_nb_bi<-freqmodelbi_NB$fitted.values #fitted values of Lambda: E(Y)

expnb<-rep(0,numrow)->expnb_off
for(i in 1:numrow){
  expnb[i]<-sum(dnbinom(i-1,mu=mean_nb_bi,size=summary(freqmodelbi_NB)$theta))
  expnb_off[i]<-sum(dnbinom(i-1,mu=mean_nb_bi_off,size=summary(freqmodelbi_offset_NB)$theta))
}

# SDR vs FV #

par(mfrow=c(1,2))
plot(freqmodelbi_offset_NB$fitted.values,rstandard(freqmodelbi_offset_NB),
     xlab="Fitted values",ylab="Standardized deviance residuals",
     ylim=c(-5,5),
     main="SDR vs FV, Poisson")
abline(h=0,col="red",lty=2)

# QQ Plot #

qqnorm(rstandard(freqmodelbi_offset_NB))
qqline(rstandard(freqmodelbi_offset_NB),col="red")

# Tables #

Table1_bi <-cbind(rbind(AIC(freqmodelbi),AIC(freqmodelbi_offset),
                          AIC(freqmodelbi_NB),AIC(freqmodelbi_offset_NB)),
                    rbind(BIC(freqmodelbi),BIC(freqmodelbi_offset),
                          BIC(freqmodelbi_NB),BIC(freqmodelbi_offset_NB)))

colnames(Table1_bi) <- c("AIC", "BIC")
rownames(Table1_bi) <- c("Poisson (no offset)","Poisson (with offset)",
                           "Negative Binomial (no offset)","Negative Binomial (with offset)")

Table1_bi


Table2_bi<-round(rbind(cbind(c(emp[1:4]), #observations
                               c(expp[1:4]),c(expp_off[1:4]),#Possion
                               c(expnb[1:4]),c(expnb_off[1:4])), #NB
                         colSums(cbind(emp,expp,expp_off,expnb,expnb_off)),
                         cbind(emp,expp,expp_off,expnb,expnb_off)[1,]/colSums(cbind(emp,expp,expp_off,expnb,expnb_off)),
                         cbind(emp,expp,expp_off,expnb,expnb_off)[2,]/
                           colSums(cbind(emp,expp,expp_off,expnb,expnb_off))),3)
colnames(Table2_bi) <- c("Observation", "Poisson (no offset)",
                           "Poisson (with offset)","Negative Binomial (no offset)",
                           "Negative Binomial (with offset)")
rownames(Table2_bi) <- c("N=0", "N=1","N=2","N=3",
                           "Sum","0 proportion","1 proportion")