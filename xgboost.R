# Load the necessary packages
library(xgboost)
library(caret)

# Load the data
df_prepared <- read.csv("/Users/melaniegroeneveld/Documents/Data in Sport and Health/DataInSportAndHealth/df_prepared.csv", stringsAsFactors = TRUE)

# Split the data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(df_prepared$training_RPE, p = .8, list=F)
train <- df_prepared[trainIndex, ]
test <- df_prepared[-trainIndex, ]

X_train = data.matrix(train[, -which(names(train) == "training_RPE")])            
y_train = as.numeric(train$training_RPE)          

X_test = data.matrix(test[, -which(names(test) == "training_RPE")])       
y_test = as.numeric(test$training_RPE)      

xgboost_train <- xgb.DMatrix(data = X_train, label = y_train)
xgboost_test <- xgb.DMatrix(data = X_test, label = y_test)

grid_tune <- expand.grid( 
  nrounds = c(500, 1000, 1500), 
  max_depth = c(2, 4, 6), 
  eta = c(0.025, 0.05, 0.1, 0.3),  
  gamma = c(0, 0.05, 0.1, 0.5, 0.7, 0.9, 1.0), 
  colsample_bytree = c(0.4, 0.6, 0.8, 1.0), 
  min_child_weight = c(1, 2, 3), 
  subsample = c(0.5, 0.75, 1.0))

params <- list(
  objective = "reg:squarederror",
  num_class = 3,
  max_depth = 4,
  eta = 0.3,
  subsample = 0.7,
  colsample_bytree = 0.7
)

# Train the xgboost model
xgb_model <- xgb.train(
  params = params,
  data = xgboost_train,
  nrounds = 100,
  verbose = 0
)

# Make predictions on the test set
pred <- predict(xgb_model, xgboost_test)

# Evaluate the performance of the model
rmse <- caret::RMSE(xgboost_test$training_RPE, pred)
mse <- mean((xgboost_test$training_RPE - pred)^2)
mae <- caret::MAE(xgboost_test$training_RPE, pred)
print(paste0("RMSE: ", rmse))
print(paste0("mse: ", mse))
print(paste0("mae: ", mae))
