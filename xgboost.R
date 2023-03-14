# Load the necessary packages
library(xgboost)
library(caret)
library(shapper)
library(ggplot2)

# Load the data
df_prepared <- read.csv("/Users/melaniegroeneveld/Documents/Data in Sport and Health/DataInSportAndHealth/df_prepared.csv", stringsAsFactors = TRUE)
df_prepared <- df_prepared %>% 
  dplyr::select(-c(start_date, start_time, end_time)) %>% 
  dplyr::mutate_if(is.integer, as.numeric)

# Split the data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(df_prepared$training_RPE, p = .8, list=F)
train <- df_prepared[trainIndex, ]
test <- df_prepared[-trainIndex, ]

##################################### STEP 1 #####################################
nrounds <- 1000
tune_grid <- expand.grid(
  nrounds = seq(from = 200, to = nrounds, by = 50),
  eta = c(0.010, 0.020, 0.025, 0.05),
  max_depth = c(2, 3, 4, 5, 6),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

tune_control <- caret::trainControl(
  method = "cv", # cross-validation
  number = 10, # with n folds 
  verboseIter = FALSE, # no training log
  allowParallel = TRUE # FALSE for reproducible results, 
)

xgb_tune <- caret::train(
  training_RPE ~ ., 
  data = train, 
  trControl = tune_control,
  tuneGrid = tune_grid,
  method = "xgbTree",
  verbose = FALSE #no training log 
)

trellis.par.set(caretTheme())
plot(xgb_tune, metric="RMSE") #RMSE, MAE, R2; default is RMSE 
ggplot(xgb_tune)

xgb_tune$bestTune

# eta = 0.01 
##################################### STEP 2 #####################################
tune_grid2 <- expand.grid(
  nrounds = seq(from = 50, to = nrounds, by = 50),
  eta = xgb_tune$bestTune$eta,
  max_depth = ifelse(xgb_tune$bestTune$max_depth == 2,
                     c(xgb_tune$bestTune$max_depth:4),
                     xgb_tune$bestTune$max_depth - 1:xgb_tune$bestTune$max_depth + 1),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = c(1, 2, 3),
  subsample = 1
)

xgb_tune2 <- caret::train(
  training_RPE ~ ., 
  data = train, 
  trControl = tune_control,
  tuneGrid = tune_grid2,
  method = "xgbTree",
  verbose = FALSE
)

plot(xgb_tune2) #RMSE, MAE, R2; default is RMSE 
ggplot(xgb_tune2)
xgb_tune2$bestTune

##################################### STEP 3 #####################################
tune_grid3 <- expand.grid(
  nrounds = seq(from = 50, to = nrounds, by = 50),
  eta = xgb_tune$bestTune$eta,
  max_depth = xgb_tune2$bestTune$max_depth,
  gamma = 0,
  colsample_bytree = c(0.4, 0.6, 0.8, 1.0),
  min_child_weight = xgb_tune2$bestTune$min_child_weight,
  subsample = c(0.5, 0.75, 1.0)
)

xgb_tune3 <- caret::train(
  training_RPE ~ ., 
  data = train, 
  trControl = tune_control,
  tuneGrid = tune_grid3,
  method = "xgbTree",
  verbose = TRUE
)

plot(xgb_tune3) #RMSE, MAE, R2; default is RMSE 
ggplot(xgb_tune3)
xgb_tune3$bestTune

##################################### STEP 4 #####################################
tune_grid4 <- expand.grid(
  nrounds = seq(from = 50, to = nrounds, by = 50),
  eta = xgb_tune$bestTune$eta,
  max_depth = xgb_tune2$bestTune$max_depth,
  gamma = c(0, 0.05, 0.1, 0.5, 0.7),
  colsample_bytree = xgb_tune3$bestTune$colsample_bytree,
  min_child_weight = xgb_tune2$bestTune$min_child_weight,
  subsample = xgb_tune3$bestTune$subsample
)

xgb_tune4 <- caret::train(
  training_RPE ~ ., 
  data = train, 
  trControl = tune_control,
  tuneGrid = tune_grid4,
  method = "xgbTree",
  verbose = TRUE
)

plot(xgb_tune4) #RMSE, MAE, R2; default is RMSE 
ggplot(xgb_tune4)
xgb_tune4$bestTune

##################################### STEP 5 #####################################
tune_grid5 <- expand.grid(
  nrounds = seq(from = 100, to = 10000, by = 100),
  eta = c(0.001, 0.0015, 0.01),
  max_depth = xgb_tune2$bestTune$max_depth,
  gamma = xgb_tune4$bestTune$gamma,
  colsample_bytree = xgb_tune3$bestTune$colsample_bytree,
  min_child_weight = xgb_tune2$bestTune$min_child_weight,
  subsample = xgb_tune3$bestTune$subsample
)

xgb_tune5 <- caret::train(
  training_RPE ~ ., 
  data = train, 
  trControl = tune_control,
  tuneGrid = tune_grid5,
  method = "xgbTree",
  verbose = TRUE
)

plot(xgb_tune5) #RMSE, MAE, R2; default is RMSE 
ggplot(xgb_tune5)
xgb_tune5$bestTune

##################################### Final #####################################
final_grid <- expand.grid(
  nrounds = xgb_tune5$bestTune$nrounds,
  eta = xgb_tune5$bestTune$eta,
  max_depth = xgb_tune5$bestTune$max_depth,
  gamma = xgb_tune5$bestTune$gamma,
  colsample_bytree = xgb_tune5$bestTune$colsample_bytree,
  min_child_weight = xgb_tune5$bestTune$min_child_weight,
  subsample = xgb_tune5$bestTune$subsample
)

xgb_model <- caret::train(
  training_RPE ~ ., 
  data = train,
  trControl = train_control,
  tuneGrid = final_grid,
  method = "xgbTree",
  verbose = TRUE
)

plot(xgb_model)

train_pred <- predict(xgb_model, train)
test_pred <- predict(xgb_model, test)

# Evaluate the performance of the model
postResample(pred=test_pred, obs=test$training_RPE)
postResample(pred=train_pred, obs=train$training_RPE) # het klopt dat dit bijna helemaal goed is! 

################################## Importance ##################################
importance <- varImp(xgb_model) #scale = FALSE avoids the normalization step 
plot(importance)


################################# NOG WAT GEPROBEERD #########################################
grid_tune <- expand.grid( 
  nrounds = c(500, 1000, 1500), 
  max_depth = c(2, 4, 6), 
  eta = c(0.025, 0.05, 0.1, 0.3),  
  gamma = c(0, 0.05, 0.1, 0.5, 0.7, 0.9, 1.0), 
  colsample_bytree = c(0.4, 0.6, 0.8, 1.0), 
  min_child_weight = c(1, 2, 3), 
  subsample = c(0.5, 0.75, 1.0))

# Define the training control parameters for cross-validation
ctrl <- trainControl(method = "cv", number = 10, verboseIter = FALSE)

# Train the xgboost model using the grid of hyperparameters and cross-validation
xgb_model <- train(x = X_train, 
                   y = y_train, 
                   method = "xgbTree", 
                   trControl = ctrl, 
                   tuneGrid = grid_tune, 
                   verbose = TRUE,
                   metric = "RMSE")

tuneplot(xgb_model)
xgb_model$bestTune

# Make predictions on the test set
pred <- predict(xgb_model, X_test)

# Evaluate the performance of the model
rmse <- caret::RMSE(y_test, pred)
mse <- mean((y_test - pred)^2)
mae <- caret::MAE(y_test, pred)
print(paste0("RMSE: ", rmse))
print(paste0("mse: ", mse))
print(paste0("mae: ", mae))

# Look for overfitting: 
dtrain <- xgb.DMatrix(data = X_train, label = y_train)
xgb_model_final <- xgb.train(data = dtrain, 
                       nrounds = xgb_model$bestTune$nrounds, 
                       max_depth = xgb_model$bestTune$max_depth, 
                       eta = xgb_model$bestTune$eta, 
                       gamma = xgb_model$bestTune$gamma, 
                       colsample_bytree = xgb_model$bestTune$colsample_bytree, 
                       min_child_weight = xgb_model$bestTune$min_child_weight, 
                       subsample = xgb_model$bestTune$subsample, 
                       objective = "reg:squarederror")

#shap_values <- shapper::shap(X_test, xgb_model)
#shapper::plot_shap(shap_values, agaricus.train$data, plot_type = "dot")

#https://www.hackerearth.com/practice/machine-learning/machine-learning-algorithms/beginners-tutorial-on-xgboost-parameter-tuning-r/tutorial/
#https://www.projectpro.io/recipes/apply-xgboost-for-classification-r 