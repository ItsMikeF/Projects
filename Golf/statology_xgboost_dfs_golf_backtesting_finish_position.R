library(tidyverse)  #Metapackage
library(xgboost, warn.conflicts = F)
library(readr, warn.conflicts = F)
library(stringr, warn.conflicts = F)
library(caret, warn.conflicts = F)
library(car, warn.conflicts = F)

setwd("C:/Users/mikef/Documents/GitHub/DFS_Data/Data_Golf/results")

#make this example reproducible
set.seed(0)

#split into training (80%) and testing set (20%)
data <- read.csv("golfers_results.csv") %>% 
  drop_na()
parts = createDataPartition(data$total_pts, p = .8, list = F)
train = data[parts, ]
test = data[-parts, ]

#define predictor and response variables in training set
train_x <- data.matrix(train %>% 
                         select(Salary, 
                                odds_close, 
                                odds_delta_per, 
                                win))

train_y <- train$observed_finish

#define predictor and response variables in testing set
test_x <- data.matrix(test %>% 
                        select(Salary, 
                                odds_close, 
                                odds_delta_per, 
                                win))
test_y <- test$observed_finish

#define final training and testing sets
xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)

#define watchlist
watchlist = list(train=xgb_train, test=xgb_test)

#fit XGBoost model and display training and testing data at each round
model <- xgb.train(data = xgb_train, max.depth = 2, watchlist=watchlist, nrounds = 20, print_every_n = 1)

#define final model
final <- xgboost(data = xgb_train, max.depth = 2, nrounds = 8, print_every_n = 1)

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