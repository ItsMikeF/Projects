#lets calculate fpts with xgb

#load Packages
suppressMessages({
  library(tidyverse) #Metapackage
  library(xgboost) #extreme gradient boosting
  library(caret) #classification and regression training
})

#https://www.nflfastr.com/articles/beginners_guide.html
#fit2 <- lm(wins ~ prior_point_diff, data = data)

#define test and training data
data <- qbs_select %>% 
  ungroup() %>% 
  select(pressure_grades_pass, fpts)

train = data
test = nfl_qb %>% select(pressure_grades_pass) %>% 
  drop_na() %>% 
  mutate(fpts = 0)

names(data)

#define predictor and response variables in training set
train_x <- data.matrix(train %>% select(pressure_grades_pass))
train_y <- train$fpts

#define predictor and response variables in testing set
test_x <- data.matrix(test %>% select(pressure_grades_pass))
test_y <- test$fpts

#define final training and testing sets
xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)

#define watchlist
watchlist = list(train=xgb_train, test=xgb_test)

#fit XGBoost model and display training and testing data at each round
model <- xgb.train(data = xgb_train, max.depth = 2, watchlist=watchlist, nrounds = 20, print_every_n = 1)

#define final model
final <- xgboost(data = xgb_train, max.depth = 2, nrounds = 20, print_every_n = 1)

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

#now combine with nfl_qbs
df_xbg_qb <- as.data.frame(cbind(nfl_qb$Name, pred_y))
