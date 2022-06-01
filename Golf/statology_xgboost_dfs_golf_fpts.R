library(tidyverse)  #Metapackage
library(xgboost, warn.conflicts = F)
library(readr, warn.conflicts = F)
library(stringr, warn.conflicts = F)
library(caret, warn.conflicts = F)
library(car, warn.conflicts = F)

setwd("C:/Users/mikef/Documents/GitHub/Projects/Golf/Results")

#make this example reproducible
set.seed(0)

#Define training and test data
train <- read.csv("golfers_results.csv") %>% 
  drop_na(total_pts) %>% 
  drop_na(observed_finish)

test <- read.csv("golfers.csv") %>% 
  drop_na()

#define predictor and response variables in training set
train_x <- data.matrix(train %>% select(ceil,Salary, residuals, AvgPointsPerGame, win, odds_close, odds_delta_per))
train_y <- train[,29]

#define predictor and response variables in testing set
test_x <- data.matrix(test %>% select(ceil,Salary, residuals, AvgPointsPerGame, win, odds_close, odds_delta_per))
test_y <- test[,25]

#define final training and testing sets
xgb_train <- xgb.DMatrix(data = train_x, label = train_y)
xgb_test <- xgb.DMatrix(data = test_x, label = test_y)

#define watchlist
watchlist <- list(train=xgb_train, test=xgb_test)

#define final model
final <- xgboost(data = xgb_train, max.depth = 2, nrounds = 34, print_every_n = 1)

#use model to make predictions on test data
pred_y <- predict(final, xgb_test)

#Create dataframe to copy
df = cbind(pred_y, test_y)

write.csv(df, file = "df.csv")
