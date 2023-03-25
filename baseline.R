# Load the necessary packages
library(caret)
library(shapper)
library(ggplot2)

df_prepared <- read.csv("/Users/melaniegroeneveld/Documents/Data in Sport and Health/DataInSportAndHealth/df_prepared.csv", stringsAsFactors = TRUE)

# Split the data into training and testing sets
set.seed(123)
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

train_dummy <- train %>% 
  mutate(dummy = sample(c(0, 1), nrow(train), replace = TRUE))

validation_dummy <- validation %>% 
  mutate(dummy = sample(c(0, 1), nrow(validation), replace = TRUE))

testing_dummy <- testing %>% 
  mutate(dummy = sample(c(0, 1), nrow(testing), replace = TRUE))

xgb_base <- caret::train(
  training_RPE ~ dummy, 
  data = train_dummy, 
  method = "xgbTree",
  verbose = FALSE #no training log 
)

val_pred_base <- predict(xgb_base, validation_dummy)
test_pred_base <- predict(xgb_base, testing_dummy)
train_pred_base <- predict(xgb_base, train_dummy)
postResample(pred=val_pred_base, obs= validation_dummy$training_RPE)
postResample(pred=test_pred_base, obs= testing_dummy$training_RPE)
postResample(pred=train_pred_base, obs= train_dummy$training_RPE)
