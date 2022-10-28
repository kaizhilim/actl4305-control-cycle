source("00 source.R")
load("00 envr/Compulsory/policy_claims.R")

library(tweedie)
library(statmod)


policy_claims_tw = policy_claims %>%
  mutate(policy)

tw_prop_profile <- tweedie.profile(grossincurred_prop ~ 
                                     riskpostcode + building_type + 
                                     construction_walls + construction_floor + sprinkler_type +
                                     occupation_risk + suminsured_prop + LossofIncome_cover,
                                   data = training_data, do.plot = F, do.ci = F,
                                   p.vec = seq(1.3, 1.7, by = 0.1), method = "series")

tw_prop <- glm(grossincurred_prop ~ 
                 riskpostcode + building_type + 
                 construction_walls + construction_floor + sprinkler_type +
                 occupation_risk + suminsured_prop + LossofIncome_cover,
               data = training_data, family = tweedie(var.power = 1.5, link.power = 0))

######################################################################################################################################
##### Task: Fit a GLM on the aggregate loss of BC line with assumptions of Tweedie. #####
# BC
out1 <- tweedie.profile(ClaimBC~CoverageBC+lnDeductBC+NoClaimCreditBC+
                          TypeCity+TypeCounty+TypeMisc+TypeSchool+TypeTown,
                        data=freqinBC, xi.vec=seq(1.1, 1.9, length=9), 
                        do.plot=FALSE)
twBC<-glm(ClaimBC~CoverageBC+lnDeductBC+NoClaimCreditBC+
            TypeCity+TypeCounty+TypeMisc+TypeSchool+TypeTown,
          data=freqinBC,family=tweedie(var.power=out1$xi.max, link.power=0))
#link.power: index of power link function. link.power=0 produces a log-link. Defaults to the canonical link, which is 1-var.power.



# FOR GLM INCLUDE WEIGHTS USING EXPOSURE 



summary(twBC)

#out1$phi.max
#out1$xi.max  


##### Task: Interpret the GLM coefficients and do diagnostic checking of the model. #####
plot(twBC$fitted.values,rstandard(twBC),xlab="Fitted values",
     ylab="Standardized deviance residuals",
     main="Diagnostic checking: SDR vs FV, Gamma")
abline(h=0,col="red",lty=2)

qqnorm(rstandard(twBC))
qqline(rstandard(twBC),col="red")


##### Task: Compare the prediction performance of models in the test set. #####
plot(predict(twBC,newdata=doutBC,type = "link"),log(doutBC$ClaimBC+1),
     ylab="Log Claims", xlab="Log Tweedie Scores",main="BC",
     xlim=c(0,15),ylim=c(0,15)); abline(0,1)

# test RMSE
predtw <-predict(twBC,newdata = doutBC,type =  "response")
tweRMSE   <-sqrt(sum((predtw-doutBC$ClaimBC)^2)/nrow(doutBC))
