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

test[test == "Inf"] <- 200000

#define predictor and response variables in training set
train_x <- data.matrix(train %>% 
                         select(Salary,
                                AvgPointsPerGame,
                                odds_open, 
                                odds_close,
                                odds_rank,
                                odds_delta_per,
                                fpts, 
                                ceil, 
                                floor, 
                                make_cut,
                                top_20, 
                                win, 
                                residuals))
#Based on importance matrix
train_x <- data.matrix(train %>% 
                         select(ceil, 
                                floor, 
                                Salary, 
                                residuals,
                                AvgPointsPerGame, 
                                win, 
                                odds_delta_per))
train_y <- train[,29]

#define predictor and response variables in testing set
test_x <- data.matrix(test %>% 
                        select(Salary,
                               AvgPointsPerGame,
                               odds_open, 
                               odds_close,
                               odds_rank,
                               odds_delta_per,
                               fpts, 
                               ceil, 
                               floor, 
                               make_cut,
                               top_20, 
                               win, 
                               residuals))
#Based on importance matrix
test_x <- data.matrix(test %>% 
                        select(ceil, 
                               floor, 
                               Salary, 
                               residuals,
                               AvgPointsPerGame, 
                               win, 
                               odds_delta_per))
test_y <- test[,25]

#define final training and testing sets
xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)

#define watchlist
watchlist = list(train=xgb_train, test=xgb_test)

#fit XGBoost model and display training and testing data at each round
model <- xgb.train(data = xgb_train, max.depth = 2, watchlist=watchlist, nrounds = 20, print_every_n = 5)

#define final model
final <- xgboost(data = xgb_train, max.depth = 2, nrounds = 13, print_every_n = 1)

#use model to make predictions on test data
pred_y <- predict(final, xgb_test)

#Create dataframe to copy
df = cbind(pred_y, test_y)

write.csv(df, file = "df.csv")
