library(xgboost, warn.conflicts = F)
library(readr, warn.conflicts = F)
library(stringr, warn.conflicts = F)
library(caret, warn.conflicts = F)
library(car, warn.conflicts = F)
library(tidyverse)

#Set wd
setwd("C:/Users/mikef/Documents/GitHub/DFS_Data/Data_Golf/Results")

#Load sg data to be cleaned
dg <- read.csv("dg.csv")
data <- dg %>% 
  select(player_name, salary, ownership, open_odds, close_odds, odds_delta, odds_delta_per, make_cut, top_20, top_10, top_5, top_3, win, 
         total_pts) %>% 
  drop_na(close_odds) %>% 
  filter(odds_delta_per != 0)

data <- dg %>% 
  select(salary, close_odds, win, ownership) %>% 
  drop_na(close_odds) %>% 
  drop_na(win)

#make this example reproducible
set.seed(0)

#split into training (80%) and testing set (20%)
parts = createDataPartition(data$ownership, p = .8, list = F)
train = data[parts, ]
test = data[-parts, ]

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
model = xgb.train(data = xgb_train, max.depth = 2, watchlist=watchlist, nrounds = 50, print_every_n = 5)

#define final model
final = xgboost(data = xgb_train, max.depth = 3, nrounds = 50, print_every_n = 5)

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
