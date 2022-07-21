#load Packages
suppressMessages({
  library(tidyverse) #Metapackage
  library(xgboost) #extreme gradient boosting
  library(caret) #classification and regression training
})

#make this example reproducible
set.seed(0)

#split into training (80%) and testing set (20%)
data <- read_csv("./Results/golfers_results_no_odds.csv") %>% 
  drop_na(total_pts)
parts = createDataPartition(data$total_pts, p = .8, list = F)
train = data[parts, ]
test = data[-parts, ]

#define predictor and response variables in training set
train_x <- data.matrix(train %>% select(ceil, Salary, proj_own, value, make_cut, win, course_fit_total_adj, final_prediction, total_points))
train_y <- train$total_pts

#define predictor and response variables in testing set
test_x <- data.matrix(test %>% select(ceil, Salary, proj_own, value, make_cut, win, course_fit_total_adj, final_prediction, total_points))
test_y <- test$total_pts

#define final training and testing sets
xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)

#define watchlist
watchlist = list(train=xgb_train, test=xgb_test)

#fit XGBoost model and display training and testing data at each round
model <- xgb.train(data = xgb_train, max.depth = 2, watchlist=watchlist, nrounds = 40, print_every_n = 1)

#define final model
final <- xgboost(data = xgb_train, max.depth = 2, nrounds = 27, print_every_n = 1)

#use model to make predictions on test data
pred_y <- round(predict(final, xgb_test), digits = 1)

importance <- xgb.importance(feature_names = unlist(dimnames(train_x)[2]),
                             model = final)
xgb.plot.importance(importance)
head(importance)

mean((test_y - pred_y)^2) #mse
caret::MAE(test_y, pred_y) #mae
caret::RMSE(test_y, pred_y) #rmse

df = cbind(pred_y, test_y)
#write.csv(df, file = "df.csv")