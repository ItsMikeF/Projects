#load Packages
suppressMessages({
  library(tidyverse) #Metapackage
  library(xgboost) #extreme gradient boosting
  library(caret) #classification and regression training
})

#split into training (80%) and testing set (20%)
data <- qbs_select[5:dim(qbs_select)[2]]
parts = createDataPartition(data$fpts, p = .8, list = F)
train = data[parts, ]
test = data[-parts, ]

names(data)

#define predictor and response variables in training set
train_x <- data.matrix(train %>% select(grades_pass.x, grades_defense))
train_x <- data.matrix(train[,-c(which(colnames(train)=="fpts"))])
train_y <- train$fpts

#define predictor and response variables in testing set
test_x <- data.matrix(test %>% select(grades_pass.x, grades_defense))
test_x <- data.matrix(test[,-c(which(colnames(test)=="fpts"))])
test_y <- test$fpts

#define final training and testing sets
xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)

#define watchlist
watchlist = list(train=xgb_train, test=xgb_test)

#fit XGBoost model and display training and testing data at each round
model <- xgb.train(data = xgb_train, max.depth = 2, watchlist=watchlist, nrounds = 20, print_every_n = 1)

#define final model
final <- xgboost(data = xgb_train, max.depth = 2, nrounds = 12, print_every_n = 1)

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