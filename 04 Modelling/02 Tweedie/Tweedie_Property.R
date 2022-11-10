source("00 source.R")
load("00 envr/Compulsory/policy_claims.Rda")

library(tweedie)
library(statmod)

tw_prop_profile <- tweedie.profile(grossincurred_prop ~ log(suminsured_prop) +  
                                     building_age + building_type + construction_walls + construction_floor +
                                     sprinkler_type + occupation_risk + geo_code +
                                     LossofIncome_cover,
                                   data = training_data, 
                                   xi.vec = seq(1.1, 1.9, length = 9), method = "series", do.ci = F)
tw_prop <- glm(grossincurred_prop ~ log(suminsured_prop) +  
                 building_age + building_type + construction_walls + construction_floor +
                 sprinkler_type + occupation_risk + geo_code +
                 LossofIncome_cover,
               data = training_data, offset = log(exposure), weights = date_weights,
               family = tweedie(var.power = tw_prop_profile$xi.max, link.power = 0))
summary(tw_prop)

# Interpret the GLM coefficients and do diagnostic checking of the model.
plot(tw_prop$fitted.values, rstandard(tw_prop), 
     xlab = "Fitted values", ylab = "Standardized deviance residuals",
     main = "Diagnostic checking: SDR vs FV, Gamma")
abline(h = 0, col = "red", lty = 2)

qqnorm(rstandard(tw_prop))
qqline(rstandard(tw_prop), col = "red")

# Compare the prediction performance of models in the test set.
plot(predict(tw_prop, newdata = test_data, type = "link"), log(test_data$grossincurred_prop + 1),
     ylab = "Log Claims", xlab = "Log Tweedie Scores")
abline(0,1)

# test RMSE
tw_prop_pred <- predict(tw_prop, newdata = test_data, type = "response")
tw_prop_RMSE <-sqrt(sum((tw_prop_pred - test_data$grossincurred_prop)^2) / nrow(test_data))
RMSE(mean(training_data$grossincurred_prop), test_data$grossincurred_prop)

# R-squared
rss <- sum((tw_prop_pred - test_data$grossincurred_prop)^2)
tss <- sum((test_data$grossincurred_prop - mean(test_data$grossincurred_prop))^2)
(1 - rss/tss)











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
