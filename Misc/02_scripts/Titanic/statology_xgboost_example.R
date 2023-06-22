library(xgboost, warn.conflicts = F)
library(readr, warn.conflicts = F)
library(stringr, warn.conflicts = F)
library(caret, warn.conflicts = F)
library(car, warn.conflicts = F)

data = MASS::Boston
str(data)

#make this example reproducible
set.seed(0)

#split into training (80%) and testing set (20%)
parts = createDataPartition(data$medv, p = .8, list = F)
train = data[parts, ]
test = data[-parts, ]

#define predictor and response variables in training set
train_x = data.matrix(train[, -14])
train_y = train[,14]

#define predictor and response variables in testing set
test_x = data.matrix(test[, -14])
test_y = test[, 14]

#define final training and testing sets
xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)

#define watchlist
watchlist = list(train=xgb_train, test=xgb_test)

#fit XGBoost model and display training and testing data at each round
model = xgb.train(data = xgb_train, max.depth = 3, watchlist=watchlist, nrounds = 70)

#define final model
final = xgboost(data = xgb_train, max.depth = 3, nrounds = 70, verbose = 0)

#use model to make predictions on test data
pred_y = predict(final, xgb_test)

mean((test_y - pred_y)^2) #mse
caret::MAE(test_y, pred_y) #mae
caret::RMSE(test_y, pred_y) #rmse

df = cbind(pred_y, test_y)