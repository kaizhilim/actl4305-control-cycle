source("00 source.R")
load("00 envr/Compulsory/policy_claims.R")

set.seed(123)
data_split <- initial_split(policy_claims, prop = 0.8, 
                            strata = LossofIncome_cover)

training_data <- training(data_split)
test_data <- testing(data_split)

set.seed(123)
cv_set <- vfold_cv(training_data, v = 10, strata = LossofIncome_cover)
