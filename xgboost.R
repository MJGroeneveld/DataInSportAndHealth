# Load the necessary packages
library(xgboost)
library(caret)
library(shapper)

# Load the data
df_prepared <- read.csv("/Users/melaniegroeneveld/Documents/Data in Sport and Health/DataInSportAndHealth/df_prepared.csv", stringsAsFactors = TRUE)

# Split the data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(df_prepared$training_RPE, p = .8, list=F)
train <- df_prepared[trainIndex, ]
test <- df_prepared[-trainIndex, ]

X_train = data.matrix(train[, -which(names(train) == "training_RPE")])            
y_train = train$training_RPE     

X_test = data.matrix(test[, -which(names(test) == "training_RPE")])       
y_test = test$training_RPE    

grid_tune <- expand.grid( 
  nrounds = c(500, 1000, 1500), 
  max_depth = c(2, 4, 6), 
  eta = c(0.025, 0.05, 0.1, 0.3),  
  gamma = c(0, 0.05, 0.1, 0.5, 0.7, 0.9, 1.0), 
  colsample_bytree = c(0.4, 0.6, 0.8, 1.0), 
  min_child_weight = c(1, 2, 3), 
  subsample = c(0.5, 0.75, 1.0))

# Define the training control parameters for cross-validation
ctrl <- trainControl(method = "cv", number = 5, verboseIter = TRUE)

# Train the xgboost model using the grid of hyperparameters and cross-validation
xgb_model <- train (
  x = X_train,
  y = y_train,
  method = "xgbTree",
  trControl = ctrl,
  tuneGrid = grid_tune,
  metric = "RMSE",
  verbose = TRUE
)

# Make predictions on the test set
pred <- predict(xgb_model, X_test)

# Evaluate the performance of the model
rmse <- caret::RMSE(y_test, pred)
mse <- mean((y_test - pred)^2)
mae <- caret::MAE(y_test, pred)
print(paste0("RMSE: ", rmse))
print(paste0("mse: ", mse))
print(paste0("mae: ", mae))

mat <- xgb_model.importance (feature_names = colnames(new_tr),model = xgb_model)
xgb.plot.importance (importance_matrix = mat[1:20]) 

#shap_values <- shapper::shap(X_test, xgb_model)
#shapper::plot_shap(shap_values, agaricus.train$data, plot_type = "dot")

https://www.hackerearth.com/practice/machine-learning/machine-learning-algorithms/beginners-tutorial-on-xgboost-parameter-tuning-r/tutorial/
https://www.projectpro.io/recipes/apply-xgboost-for-classification-r 