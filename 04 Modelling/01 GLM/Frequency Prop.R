source("00 source.R")
load("00 envr/Compulsory/policy_claims.Rda")

library(tidymodels)
library(dplyr)
library(glmnet)
library(MASS)

#### Set Up ####

policy_freq_prop <- policy_claims %>% filter(suminsured_prop > 0)

#### Pois ####

freqmodelprop <- glm(
  claimcount_prop ~ log(suminsured_prop) + geo_code + state + 
    building_age + building_type + construction_walls + construction_floor +
    sprinkler_type + occupation_risk,
  data = policy_freq_prop,
  family=poisson(link = "log"),
  offset = log(exposure), weights = date_weights
)

beta_pos_prop<-freqmodelprop$coefficients #coefficients of GLM 1
mean_pos_prop<-freqmodelprop$fitted.values #fitted values of Lambda: E(Y)

# Quasipoisson #

freqmodelprop_qp <- glm(
  claimcount_prop ~ log(suminsured_prop) + geo_code + building_age +
     building_type + construction_walls + construction_floor +
    sprinkler_type + occupation_risk,
  data = policy_freq_prop,
  family=quasipoisson(link = "log"),
  offset = log(exposure), weights = date_weights
)

beta_qp_prop<-freqmodelprop_qp$coefficients #coefficients of GLM 1
mean_qp_prop<-freqmodelprop_qp$fitted.values #fitted values of Lambda: E(Y)

freqmodelprop_qp %>% summary()

# Counts #

numrow<-max(policy_freq_prop$claimcount_prop)+1 #maximum number of claim

emp<-rep(0,numrow)

for(i in 1:numrow){
  emp[i]<-sum(policy_freq_prop$claimcount_prop==(i-1))
}

expp<-rep(0,numrow)

for(i in 1:numrow){
  expp[i]<-sum(dpois((i-1),mean_pos_prop))
}

summary(freqmodelprop_offset)

# Anova #

freqmodelprop_anova<-glm(claimcount_prop ~. -exposure, data = policy_freq_prop_anova,
                       family=poisson(link = "log"),offset = exposure)

anova(freqmodelprop,test="Chisq") #anova test

# SDR vs FV #

par(mfrow=c(1,2))
plot(freqmodelprop$fitted.values,rstandard(freqmodelprop),
     xlab="Fitted values",ylab="Standardized deviance residuals",
     ylim=c(-5,5),
     main="SDR vs FV, Poisson")
abline(h=0,col="red",lty=2)
plot(freqmodelprop_qp$fitted.values,rstandard(freqmodelprop_qp),
     xlab="Fitted values",ylab="Standardized deviance residuals",
     ylim=c(-5,5),
     main="SDR vs FV, Quasipoisson")
abline(h=0,col="red",lty=2)

# QQ Plot #

qqnorm(rstandard(freqmodelprop))
qqline(rstandard(freqmodelprop),col="red")


#### NB ####

freqmodelprop_NB<-glm.nb(
  claimcount_prop ~ offset(log(exposure)) + log(suminsured_prop) + geo_code + state + 
    building_age + building_type + construction_walls + construction_floor +
    sprinkler_type + occupation_risk,
  data = policy_freq_prop,
  weights = date_weights
)

beta_nb_prop<-freqmodelprop_NB$coefficients #coefficients of GLM 1
mean_nb_prop<-freqmodelprop_NB$fitted.values #fitted values of Lambda: E(Y)

expnb<-rep(0,numrow)->expnb_off
for(i in 1:numrow){
  expnb[i]<-sum(dnbinom(i-1,mu=mean_nb_prop,size=summary(freqmodelprop_NB)$theta))
}

# SDR vs FV #

par(mfrow=c(1,2))
plot(freqmodelprop_offset_NB$fitted.values,rstandard(freqmodelprop_offset_NB),
     xlab="Fitted values",ylab="Standardized deviance residuals",
     ylim=c(-5,5),
     main="SDR vs FV, Poisson")
abline(h=0,col="red",lty=2)

# QQ Plot #

qqnorm(rstandard(freqmodelprop_offset_NB))
qqline(rstandard(freqmodelprop_offset_NB),col="red")

# Tables #

Table1_prop <-cbind(rbind(AIC(freqmodelprop),AIC(freqmodelprop_offset),
                     AIC(freqmodelprop_NB),AIC(freqmodelprop_offset_NB)),
               rbind(propC(freqmodelprop),propC(freqmodelprop_offset),
                     propC(freqmodelprop_NB),propC(freqmodelprop_offset_NB)))

colnames(Table1_prop) <- c("AIC", "propC")
rownames(Table1_prop) <- c("Poisson (no offset)","Poisson (with offset)",
                      "Negative Binomial (no offset)","Negative Binomial (with offset)")

Table1_prop


Table2_prop<-round(rbind(cbind(c(emp[1:4]), #observations
                          c(expp[1:4]),#Possion
                          c(expnb[1:4])), #NB
                    colSums(cbind(emp,expp,expnb)),
                    cbind(emp,expp,expnb)[1,]/colSums(cbind(emp,expp,expnb)),
                    cbind(emp,expp,expnb)[2,]/
                      colSums(cbind(emp,expp,expnb))),3)
colnames(Table2_prop) <- c("Observation", "Poisson","NB")
rownames(Table2_prop) <- c("N=0", "N=1","N=2","N=3",
                      "Sum","0 proportion","1 proportion")
