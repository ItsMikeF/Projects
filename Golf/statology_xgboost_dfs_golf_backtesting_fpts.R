library(tidyverse)  #Metapackage
library(xgboost, warn.conflicts = F)
library(readr, warn.conflicts = F)
library(stringr, warn.conflicts = F)
library(caret, warn.conflicts = F)
library(car, warn.conflicts = F)

setwd("C:/Users/mikef/Documents/GitHub/Projects/Golf/results")

#make this example reproducible
set.seed(0)

#split into training (80%) and testing set (20%)
data <- read.csv("golfers_results.csv") %>% 
  drop_na(total_pts)
parts = createDataPartition(data$total_pts, p = .8, list = F)
train = data[parts, ]
test = data[-parts, ]

#define predictor and response variables in training set
train_x <- data.matrix(train[,c(4:22,24,25)])
train_x <- data.matrix(train[,c(4:11,13:15,18,22,25)])
train_x <- data.matrix(train %>% 
                         select(ceil, 
                                floor, 
                                Salary, 
                                AvgPointsPerGame, 
                                residuals,
                                odds_delta_per, 
                                odds_close))
train_x <- data.matrix(train %>% 
                         select(ceil,
                                Salary, 
                                residuals,
                                AvgPointsPerGame, 
                                win, 
                                odds_delta_per))

train_y <- train[,29]

#define predictor and response variables in testing set
test_x <- data.matrix(test[,c(4:22,24,25)])
test_x <- data.matrix(test[,c(4:11,13:15,18,22,25)])
test_x <- data.matrix(test %>% 
                        select(ceil, 
                               floor, 
                               Salary, 
                               AvgPointsPerGame, 
                               residuals,
                               odds_delta_per, 
                               odds_close))
test_x <- data.matrix(test %>% 
                        select(ceil, 
                               Salary, 
                               residuals,
                               AvgPointsPerGame, 
                               win, 
                               odds_delta_per))
test_y <- test[,29]

#define final training and testing sets
xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)

#define watchlist
watchlist = list(train=xgb_train, test=xgb_test)

#fit XGBoost model and display training and testing data at each round
model <- xgb.train(data = xgb_train, max.depth = 3, watchlist=watchlist, nrounds = 20, print_every_n = 1)

#define final model
final <- xgboost(data = xgb_train, max.depth = 3, nrounds = 8, print_every_n = 1)

#use model to make predictions on test data
pred_y <- round(predict(final, xgb_test), digits = 1)

importance <- xgb.importance(feature_names = unlist(dimnames(train_x)[2]),
                             model = final)
head(importance)

mean((test_y - pred_y)^2) #mse
caret::MAE(test_y, pred_y) #mae
caret::RMSE(test_y, pred_y) #rmse

df = cbind(pred_y, test_y)
#write.csv(df, file = "df.csv")