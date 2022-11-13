source("00 source.R")
load("00 envr/Compulsory/policy_claims.Rda")

#----------------- PACKAGES -----------------
# installations
#install.packages("devtools")
# devtools::install_github("rstudio/tensorflow")
# install.packages("reticulate")
#devtools::install_github('EagerAI/kerastuneR')
#kerastuneR::install_kerastuner(python_path = 'paste python path')
#install_tensorflow()

# packages
library(devtools)
library(kerastuneR) 
library(caret)
library(keras)
library(reticulate)
library(tensorflow)


#----------------- DATA PREP -----------------
# turn logical to 1 or 0
policy_claims$LossofIncome_cover = as.integer(policy_claims$LossofIncome_cover)
# split dataset into training and testing set
train_ind <- createDataPartition(policy_claims$claimcount_prop, p = 0.8, list = FALSE)
train <- policy_claims[train_ind, ]
test <- policy_claims[-train_ind, ]

#----------------- PRE-PROCESSING -----------------
# functions to normalise
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

normalizetest <- function(x,y) {
  return ((x - min(y)) / (max(y) - min(y)))
}

# extract and normalise outputs
target_train <- train %>% 
  dplyr:: mutate (
    frequency_prop = claimcount_prop / exposure,
    frequency_lossofinc = replace_na(claimcount_lossofinc / exposure, 0),
    severity_prop = replace_na(grossincurred_prop / claimcount_prop, 0),
    severity_lossofinc = replace_na(grossincurred_lossofinc/ claimcount_lossofinc, 0)
  ) %>%
  dplyr:: select (
    frequency_prop,
    frequency_lossofinc,
    severity_prop,
    severity_lossofinc
  ) %>%
  dplyr:: mutate (
    frequency_prop = normalize(frequency_prop),
    frequency_lossofinc = normalize(frequency_lossofinc),
    severity_prop = normalize(severity_prop),
    severity_lossofinc = normalize(severity_lossofinc)
  )

target_test <- test %>% 
  dplyr:: mutate (
    frequency_prop = claimcount_prop / exposure,
    frequency_lossofinc = replace_na(claimcount_lossofinc / exposure, 0),
    severity_prop = replace_na(grossincurred_prop / claimcount_prop, 0),
    severity_lossofinc = replace_na(grossincurred_lossofinc/ claimcount_lossofinc, 0)
  ) %>%
  dplyr:: select (
    frequency_prop,
    frequency_lossofinc,
    severity_prop,
    severity_lossofinc
  ) %>%
  dplyr:: mutate (
    frequency_prop = normalize(frequency_prop),
    frequency_lossofinc = normalize(frequency_lossofinc),
    severity_prop = normalize(severity_prop),
    severity_lossofinc = normalize(severity_lossofinc)
  )

# one hot encoding for low-dimensional categorical variables (<=10 categories)
Dummy <- function(var1, short, dat2){
  names(dat2)[names(dat2) == var1] <- "V1"
  n2 <- ncol(dat2)
  dat2$X <- as.integer(dat2$V1)
  n0 <- length(unique(dat2$X))
  for (n1 in 2:n0){dat2[, paste(short, n1, sep="")] <- as.integer(dat2$X==n1)}
  names(dat2)[names(dat2) == "V1"] <- var1
  dat2[, c(1:n2,(n2+2):ncol(dat2))]
}

train <- Dummy("construction_walls", "wall", train)
test <- Dummy("construction_walls", "wall", test)

train <- Dummy("construction_floor", "floor", train)
test <- Dummy("construction_floor", "floor", test)

train <- Dummy("sprinkler_type", "sprinkler", train)
test <- Dummy("sprinkler_type", "sprinkler", test)

train <- Dummy("building_age", "age", train)
test <- Dummy("building_age", "age", test)

train <- Dummy("building_type", "building_type", train)
test <- Dummy("building_type", "building_type", test)

train <- Dummy("indem_per_grp", "indem_per_grp", train)
test <- Dummy("indem_per_grp", "building_type", test)

# extract features (excluding those to be embedded and dummy duplicates) and normalise numeric data
train_otherinputs <- train %>% 
  dplyr::select(
    -policyno,
    -situation_num,
    -exposure,
    -date_weights,
    -claimcount_prop,
    -claimcount_lossofinc,
    -geo_code,
    -state,
    -building_age,
    -building_type,
    -construction_walls,
    -construction_floor,
    -sprinkler_type,
    -occupation_risk,
    -indem_per_grp,
    -building_info_na
  ) %>%
  dplyr:: mutate(
    suminsured_prop = normalize(suminsured_prop),
    suminsured_lossofinc = normalize(suminsured_lossofinc)
  )

test_otherinputs <- test %>% 
  dplyr::select(
    -policyno,
    -situation_num,
    -exposure,
    -date_weights,
    -claimcount_prop,
    -claimcount_lossofinc,
    -geo_code,
    -state,
    -building_age,
    -building_type,
    -construction_walls,
    -construction_floor,
    -sprinkler_type,
    -occupation_risk,
    -indem_per_grp,
    -building_info_na
  ) %>%
  dplyr:: mutate (
    normalizetest(suminsured_prop, train$suminsured_prop),
    normalizetest(suminsured_lossofinc, train$suminsured_lossofinc)
  )

#----------------- ATTEMPT NO1 -----------------
# Embedding
occupation_risk_input = layer_input(shape = c(1), name = 'occupation_risk_input')
geo_code_input = layer_input(shape = c(1), name = 'geo_code_input')

occupation_risk_embedding = layer_embedding(output_dim = 2, input_dim = nlevels(train$occupation_risk))(occupation_risk_input) %>%
  layer_reshape(target_shape = c(2))
geo_code_embedding = layer_embedding(output_dim = 2, input_dim = nlevels(train$geo_code))(geo_code_input) %>%
  layer_reshape(target_shape = c(2))

# other inputs
other_inputs = layer_input(shape = c(ncol(train_otherinputs)), name = 'other_inputs')

# concatenate inputs
inputs_combined = layer_concatenate((c(occupation_risk_embedding, geo_code_embedding, other_inputs)))

# dense layers
x = inputs_combined %>%
  layer_dense(units = 20, activation = 'relu', name = 'Dense_layer') %>%
  layer_dropout(rate = 0.2) 


# output layers
frequency_prop_output = x %>% layer_dense(1, activation = 'relu', name = 'frequency_prop')
severity_prop_output = x %>% layer_dense(1, activation = 'relu', name = 'severity_prop')

# model definition
model = keras_model(inputs = c(occupation_risk_input, geo_code_input, other_inputs),
                    outputs = c(frequency_prop_output, severity_prop_output))

# compile model
model %>% compile(optimizer = "adam",
                  loss = list(
                    frequency_prop = 'mean_squared_error',
                    severity_prop = 'mean_squared_error',
                  ))

summary(model)
x_in = list(occupation_risk_input = train$occupation_risk,
            geo_code_input = train$geo_code,
            other_inputs = train_otherinputs)
y_in = list(frequency_prop = target_train$frequency_prop,
           frequency_lossofinc = target_train$frequency_lossofinc,
           severity_prop = target_train$severity_prop,
           severity_lossofinc = target_train$severity_lossofinc) 

model %>% fit(
  x = x_in,
  y = target_train,
  validation_split = 0.2,
  epochs = 100  
)

#----------------- ATTEMPT NO2 -----------------
train <- Dummy("geo_code", "geo_code", train)
test <- Dummy("geo_code", "geo_code", test)

train <- Dummy("occupation_risk", "occ", train)
test <- Dummy("occupation_risk", "occ", test)

traindata_prop_claimno <- train %>%
  dplyr::mutate(
    frequency_prop = replace_na(claimcount_prop / exposure, 0),
    severity_prop = replace_na(grossincurred_prop / claimcount_prop, 0)
  ) %>%
  dplyr::select(
    -policyno,
    -situation_num,
    -policy_version,
    -exposure,
    -date_weights,
    -claimcount_prop,
    -claimcount_lossofinc,
    -grossincurred_prop,
    -grossincurred_lossofinc,
    -geo_code,
    -state,
    -building_age,
    -building_type,
    -construction_walls,
    -construction_floor,
    -sprinkler_type,
    -occupation_risk,
    -indem_per_grp,
    -building_info_na
  ) %>%
  dplyr:: mutate(
    frequency_prop = normalize(frequency_prop),
    severity_prop = normalize(severity_prop),
    suminsured_prop = normalize(suminsured_prop),
    suminsured_lossofinc = normalize(suminsured_lossofinc)
  )

testdata_prop_claimno <- train %>%
  dplyr::mutate(
    frequency_prop = replace_na(claimcount_prop / exposure, 0),
    severity_prop = replace_na(grossincurred_prop / claimcount_prop, 0)
  ) %>%
  dplyr::select(
    -policyno,
    -situation_num,
    -policy_version,
    -exposure,
    -date_weights,
    -claimcount_prop,
    -claimcount_lossofinc,
    -grossincurred_prop,
    -grossincurred_lossofinc,
    -geo_code,
    -state,
    -building_age,
    -building_type,
    -construction_walls,
    -construction_floor,
    -sprinkler_type,
    -occupation_risk,
    -indem_per_grp,
    -building_info_na
  ) %>%
  dplyr:: mutate(
    frequency_prop = normalizetest(frequency_prop, traindata_prop_claimno$frequency_prop),
    severity_prop = normalizetest(severity_prop, traindata_prop_claimno$severity_prop),
    suminsured_prop = normalizetest(suminsured_prop, traindata_prop_claimno$suminsured_prop),
    suminsured_lossofinc = normalizetest(suminsured_lossofinc, traindata_prop_claimno$suminsured_lossofinc)
  )

# train the neural network
nn = neuralnet(frequency_prop + severity_prop ~ . , data = traindata_prop_claimno,
               hidden = c(2,2), linear.output = TRUE, threshold = 0.05, algorithm = 'rprop+')

plot(nn,rep = "best")

# test the accuracy of the model
Xtest <- dplyr::select(testdata_prop_claimno, c(-frequency_prop, -severity_prop))
nn.results <- neuralnet::compute(nn, Xtest)
original_nn_freq <- (nn.results$net.result[1]) * (max(traindata_prop_claimno$frequency_prop) - min(traindata_prop_claimno$frequency_prop))
mse_freq <- sum((original_nn_freq - testdata_prop_claimno$frequency_prop)^2) / length(testdata_prop_claimno$frequency_prop)
original_nn_sev <- (nn.results$net.result[2]) * (max(traindata_prop_claimno$severity_prop) - min(traindata_prop_claimno$severity_prop))
mse_sev <- sum((original_nn_sev - testdata_prop_claimno$severity_prop)^2) / length(testdata_prop_claimno$severity_prop)
