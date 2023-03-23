# Load the necessary packages
library(xgboost)
library(caret)
library(shapper)
library(ggplot2)
library(ROSE)

# Load the data
df_prepared <- read.csv("/Users/melaniegroeneveld/Documents/Data in Sport and Health/DataInSportAndHealth/df_prepared.csv", stringsAsFactors = TRUE)

########## Nu runnen zonder information about daypart & session ##########
df_prepared <- df_prepared %>%
  dplyr::select(-c(training_daypart_evening, training_daypart_morning, 
                   training_session_Ext.interval, training_session_Ext.tempo, 
                   training_session_Fartlek, training_session_Int.endurance, 
                   training_session_Int.interval, training_session_Int.tempo)) %>%
  dplyr::mutate_if(is.integer, as.numeric)

theme_ppt <- theme(plot.background = element_rect(fill = "#37474f"), 
                   axis.title = element_text(color = "white"), 
                   axis.ticks = element_line(color = "white"), 
                   axis.text = element_text(color = "white"), 
                   legend.background = element_rect(fill = "#37474f"), 
                   legend.text = element_text(color = "white"), 
                   legend.title = element_text(color = "white"))

# Split the data into training and testing sets
set.seed(42)
# Split data into 70% training and 30% testing
trainIndex <- createDataPartition(df_prepared$training_RPE, p = 0.7, list = FALSE)
training <- df_prepared[trainIndex,]
testing <- df_prepared[-trainIndex,]

# Split the training data into 85% training and 15% validation
trainIndex <- createDataPartition(training$training_RPE, p = 0.85, list = FALSE)

train <- training[trainIndex,]
validation <- training[-trainIndex,]

y_train <- train$training_RPE
y_val <- validation$training_RPE
y_test <- testing$training_RPE

################################### default ###################################
# train the default model on the training dataset
xgb_default <- caret::train(
  training_RPE ~ ., 
  data = train, 
  method = "xgbTree",
  verbose = FALSE #no training log 
)

############################# Check for overfitting #############################
train_pred_default <- predict(xgb_default, train)
val_pred_default <- predict(xgb_default, validation)
postResample(pred=train_pred_default, obs=train$training_RPE)
postResample(pred=val_pred_default, obs=validation$training_RPE)

################################## Grid search #################################
grid_tune <- expand.grid(
  nrounds = c(100, 500, 1000), 
  eta = c(0.025, 0.05, 0.1, 0.3),
  gamma = c(0, 0.05, 0.1, 0.5),
  max_depth = c(2, 4, 6, 8),
  min_child_weight = c(1, 3, 5, 7),
  subsample = c(0.5, 0.75, 1),
  colsample_bytree = c(0.4, 0.8, 0.9, 1)
)

tune_control <- caret::trainControl(
  method = "cv", # cross-validation
  number = 10, # with n folds 
  verboseIter = FALSE, # no training log
  allowParallel = TRUE # FALSE for reproducible results, 
)

xgb_tuned <- caret::train(training_RPE ~ ., 
                          data = train, 
                          method = "xgbTree",
                          trControl = tune_control, 
                          tuneGrid = grid_tune,
                          verbose = TRUE,
                          metric = "RMSE")

################################## L2 regularization #################################
# L2 regularization on weights. It is used to avoid overfitting.
grid_tune2 <- expand.grid(
  nrounds = xgb_tuned$bestTune$nrounds, 
  eta = xgb_tuned$bestTune$eta,
  gamma = xgb_tuned$bestTune$gamma,
  max_depth = xgb_tuned$bestTune$max_depth,
  min_child_weight = xgb_tuned$bestTune$min_child_weight,
  subsample = xgb_tuned$bestTune$subsample,
  colsample_bytree = xgb_tuned$bestTune$colsample_bytree
)

xgb_tuned_L2 <- caret::train(training_RPE ~ ., 
                             data = train, 
                             method = "xgbTree",
                             trControl = tune_control, 
                             tuneGrid = grid_tune2,
                             verbose = TRUE,
                             metric = "RMSE", 
                             lambda = 0.5, 
                             alpha = 0)

############################# Check for overfitting #############################
train_pred_tunedL2 <- predict(xgb_tuned_L2, train)
val_pred_tunedL2 <- predict(xgb_tuned_L2, validation)

postResample(pred=val_pred_tunedL2, obs=validation$training_RPE)

################### Evaluate best models on the test dataset ###################
# Make predictions on the test set
pred <- predict(xgb_tuned_L2, testing)

# Evaluate the performance of the model
postResample(pred=pred, obs=testing$training_RPE)
rmse <- caret::RMSE(y_test, pred)
mse <- mean((y_test - pred)^2)
mae <- caret::MAE(y_test, pred)
print(paste0("RMSE: ", rmse))
print(paste0("mse: ", mse))
print(paste0("mae: ", mae))


################################## Importance ##################################
importance <- varImp(xgb_final) #scale = FALSE avoids the normalization step 
plot(importance)

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

val_pred_tuned <- predict(xgb_tune, validation)
postResample(pred=val_pred_tuned, obs=validation$training_RPE)

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

val_pred_tuned2 <- predict(xgb_tune2, validation)
postResample(pred=val_pred_tuned2, obs=validation$training_RPE)

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

val_pred_tuned3 <- predict(xgb_tune3, validation)
postResample(pred=val_pred_tuned3, obs=validation$training_RPE)

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

val_pred_tuned4 <- predict(xgb_tune4, validation)
postResample(pred=val_pred_tuned4, obs=validation$training_RPE)

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
ggplot(xgb_tune5) + 
       theme_ppt
xgb_tune5$bestTune

val_pred_tuned4 <- predict(xgb_tune4, validation)
postResample(pred=val_pred_tuned4, obs=validation$training_RPE)

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
  trControl = tune_control,
  tuneGrid = final_grid,
  method = "xgbTree",
  verbose = TRUE
)

# Evaluate the performance of the model
test_pred <- predict(xgb_model, testing)
val_pred <- predict(xgb_model, validation)
train_pred <- predict(xgb_model, train)
postResample(pred=val_pred, obs=validation$training_RPE)
postResample(pred=train_pred, obs=train$training_RPE)
postResample(pred=test_pred, obs=testing$training_RPE)

################################## Importance ##################################
importance <- varImp(xgb_model) #scale = FALSE avoids the normalization step 
plot(importance)
ggplot(importance, top = 15)

######################## Plotting actual vs predicted ########################
options(repr.plot.width=8, repr.plot.height=4)
my_data = as.data.frame(cbind(predicted = test_pred,
                              observed = y_test))
# Plot predictions vs test data
ggplot(my_data,aes(predicted, observed)) + geom_point(color = "darkred", alpha = 0.5) + 
  geom_smooth(method=lm)+ ggtitle('Linear Regression ') + ggtitle("Extreme Gradient Boosting: Prediction vs Test Data") +
  xlab("Predicted ") + ylab("Observed") + 
  theme(plot.title = element_text(color="white",size=16,hjust = 0.5),
        axis.text.y = element_text(size=12), axis.text.x = element_text(size=12,hjust=.5),
        axis.title.x = element_text(size=14), axis.title.y = element_text(size=14)) + 
  theme_ppt


######################## Statistic ########################
# Calculate R-squared & RMSE values for the training & test datasets & perform a paired
train_r2 <- caret::R2(y_train, train_pred)
train_rmse <- caret::RMSE(y_train, train_pred)
test_r2 <- caret::R2(y_test, test_pred)
test_rmse <- caret::RMSE(y_test, test_pred)

t_test_result <- t.test(train_rmse, test_rmse, paired = TRUE)

# check if the p-value is less than 0.05
if (t_test_result$p.value < 0.05) {
  cat("The train to test difference in R-squared is significant at the 0.05 level.\n")
} else {
  cat("The train to test difference in R-squared is not significant at the 0.05 level.\n")
}
