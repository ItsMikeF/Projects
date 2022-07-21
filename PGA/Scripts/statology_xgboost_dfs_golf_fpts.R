#Import packages
library(tidyverse, warn.conflicts = F) #Metapackage
library(xgboost, warn.conflicts = F)
library(readr, warn.conflicts = F)
library(stringr, warn.conflicts = F)
library(caret, warn.conflicts = F)

#inputs
entries <- 20

#Define training and test data
train <- read.csv("./Results/golfers_results_no_odds.csv") %>% 
  drop_na(total_pts) %>% 
  drop_na(observed_finish)

golfers <- read.csv(paste0("./Results/golfers_",entries,".csv"))
test <- golfers %>% 
  drop_na() %>% 
  mutate(total_pts = 0)

#define predictor and response variables in training set
train_x <- data.matrix(train %>% select(ceil,Salary, residuals, AvgPointsPerGame, win, odds_close, odds_delta_per))
train_x <- data.matrix(train %>% select(ceil, Salary, residuals, AvgPointsPerGame, win, top_20, odds_close, odds_delta_per))
train_x <- data.matrix(train %>% select(ceil, Salary, proj_own, value, make_cut, win, course_fit_total_adj, final_prediction, total_points))
train_y <- train[,29]
train_y <- train$total_pts

#define predictor and response variables in testing set
test_x <- data.matrix(test %>% select(ceil,Salary, residuals, AvgPointsPerGame, win, odds_close, odds_delta_per))
test_x <- data.matrix(test %>% select(ceil, Salary, residuals, AvgPointsPerGame, win, top_20, odds_close, odds_delta_per))
test_x <- data.matrix(test %>% select(ceil, Salary, proj_own, value, make_cut, win, course_fit_total_adj, final_prediction, total_points))
test_y <- test[,25]
test_y <- test$total_pts

#define final training and testing sets
xgb_train <- xgb.DMatrix(data = train_x, label = train_y)
xgb_test <- xgb.DMatrix(data = test_x, label = test_y)

#define watchlist
watchlist <- list(train=xgb_train, test=xgb_test)

#define final model
final <- xgboost(data = xgb_train, max.depth = 2, nrounds = 24, print_every_n = 1)

#use model to make predictions on test data
pred_y <- predict(final, xgb_test)

#Create dataframe to copy
xgb_fpts = cbind(pred_y, test_y)
xgb_fpts[,1]

test$total_pts <- round(xgb_fpts[,1], digits = 2)

write.csv(test, file = paste0("./Results/golfers_",entries,".csv"), row.names = F)
write.csv(test, file = paste0(list.dirs()[20],"/golfers_",entries,".csv"), row.names = F)
