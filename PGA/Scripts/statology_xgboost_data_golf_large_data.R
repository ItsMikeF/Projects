library(xgboost, warn.conflicts = F)
library(readr, warn.conflicts = F)
library(stringr, warn.conflicts = F)
library(caret, warn.conflicts = F)
library(car, warn.conflicts = F)
library(tidyverse)

#Set wd
setwd("C:/Users/mikef/Documents/GitHub/Projects/Golf/Results")

#Load sg data to be cleaned
dg <- read.csv("dg.csv")

#Filter training data set
data <- dg %>% 
  select(salary, close_odds, win, ownership) %>% 
  drop_na(close_odds) %>% 
  drop_na(win)

#Load testing data
golfers <- read.csv("golfers.csv")
golfers <- golfers %>% 
  select(Salary, odds_close, win, proj_own) 
names(golfers)[c(1,2,4)] <- c("salary","close_odds", "ownership")
golfers$ownership <- golfers$ownership / 100

#split into training (80%) and testing set (20%)
train <- data
test <- golfers

#define predictor and response variables in training set
train_x = data.matrix(train[,1:3])
train_y = train[,4]

#define predictor and response variables in testing set
test_x = data.matrix(test[,1:3])
test_y = test[,4]

#define final training and testing sets
xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)

#define watchlist
watchlist = list(train=xgb_train, test=xgb_test)

#fit XGBoost model and display training and testing data at each round
model = xgb.train(data = xgb_train, max.depth = 2, watchlist=watchlist, nrounds = 20, print_every_n = 1)

#define final model
final = xgboost(data = xgb_train, max.depth = 2, nrounds = 15, print_every_n = 1)

#use model to make predictions on test data
pred_y = round(predict(final, xgb_test), digits = 4)

#importance
importance <- xgb.importance(model = final)

#save to dataframe
df = cbind(pred_y, test_y)
write.csv(df, file = "pred_own.csv")

#Create error chart
error_chart <- data.frame()

error_chart[1,1] <- round(mean((test_y - pred_y)^2), digits = 4) #mse
error_chart[2,1] <- round(caret::MAE(test_y, pred_y), digits = 4) #mae
error_chart[3,1] <- round(caret::RMSE(test_y, pred_y), digits = 4) #rmse

row.names(error_chart) <- c("mse", "mae", "rmse")
colnames(error_chart)[1] <- c("xgboost")
