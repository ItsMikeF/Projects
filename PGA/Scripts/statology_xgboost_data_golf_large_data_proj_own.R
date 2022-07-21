library(xgboost, warn.conflicts = F)
library(readr, warn.conflicts = F)
library(stringr, warn.conflicts = F)
library(caret, warn.conflicts = F)
library(car, warn.conflicts = F)
library(tidyverse)

#split into training (80%) and testing set (20%)
data <- read_csv("./Results/golfers_results_no_odds.csv") %>% 
  drop_na()

data <- data %>% 
  select(proj_own, projected_ownership, ownership)

#split into training (80%) and testing set (20%)
parts = createDataPartition(data$ownership, p = .8, list = F)
train = data[parts, ]
test = data[-parts, ]

#define predictor and response variables in training set
train_x = data.matrix(train[,1:2])
train_y = train$ownership

#define predictor and response variables in testing set
test_x = data.matrix(test[,1:2])
test_y = test$ownership

#define final training and testing sets
xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)

#define watchlist
watchlist = list(train=xgb_train, test=xgb_test)

#fit XGBoost model and display training and testing data at each round
model = xgb.train(data = xgb_train, max.depth = 2, watchlist=watchlist, nrounds = 40, print_every_n = 1)

#define final model
final = xgboost(data = xgb_train, max.depth = 2, nrounds = 15, print_every_n = 5)

#use model to make predictions on test data
pred_y = predict(final, xgb_test)

#importance
importance <- xgb.importance(model = final)

#save to dataframe
df = cbind(pred_y, test_y)

#Create error chart
error_chart <- data.frame()

error_chart[1,1] <- round(mean((test_y - pred_y)^2), digits = 4) #mse
error_chart[2,1] <- round(caret::MAE(test_y, pred_y), digits = 4) #mae
error_chart[3,1] <- round(caret::RMSE(test_y, pred_y), digits = 4) #rmse

row.names(error_chart) <- c("mse", "mae", "rmse")
colnames(error_chart)[1] <- c("xgboost")
