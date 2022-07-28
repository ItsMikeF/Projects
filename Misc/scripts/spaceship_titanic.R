#goal is to submit an competition entry for kaggle comp on spaceship titanic

#load packages
library(tidyverse) #Metapackage
library(xgboost)
library(stringr)
library(caret)

#load csvs
test <- read.csv("./data/spaceship_titanic/test.csv")
train <- read.csv("./data/spaceship_titanic/train.csv")
sample_submission_example <- read.csv("./data/sample_submission.csv")

#eda
train <- train %>% 
  drop_na()

test <- test %>% 
  mutate(Transported = 0)

str(train)
summary(test)

train$CryoSleep <- as.integer(as.logical(train$CryoSleep))
train$VIP <- as.integer(as.logical(train$VIP))
train$Transported <- as.integer(as.logical(train$Transported))

test$CryoSleep <- as.integer(as.logical(test$CryoSleep))
test$VIP <- as.integer(as.logical(test$VIP))
test$Transported <- as.integer(as.logical(test$Transported))

#define predictor and response variables in training set
train_x <- data.matrix(train %>% select(CryoSleep, Age, VIP, FoodCourt, ShoppingMall, Spa, VRDeck))
train_y <- train$Transported

#define predictor and response variables in testing set
test_x <- data.matrix(test %>% select(CryoSleep, Age, VIP, FoodCourt, ShoppingMall, Spa, VRDeck))
test_y <- test$Transported

#define final training and testing sets
xgb_train <- xgb.DMatrix(data = train_x, label = train_y)
xgb_test <- xgb.DMatrix(data = test_x, label = test_y)

#define watchlist
watchlist <- list(train=xgb_train, test=xgb_test)

#fit XGBoost model and display training and testing data at each round
model = xgb.train(data = xgb_train, max.depth = 2, watchlist=watchlist, nrounds = 200, print_every_n = 5)

#define final model
final <- xgboost(data = xgb_train, max.depth = 2, nrounds = 10, print_every_n = 1)

#use model to make predictions on test data
pred_y <- predict(final, xgb_test)
pred_y <- as.numeric(pred_y > 0.5)

#Create dataframe to copy
xgb_fpts = cbind(pred_y, test_y)
test[,14] <- xgb_fpts[,1]
test[,14] <- as.logical(as.integer(test$Transported))

sample_submission <- test %>% 
  select(PassengerId, Transported)

sample_submission$Transported <- as.character(sample_submission$Transported)

sample_submission$Transported <- str_to_title(sample_submission$Transported)

write.csv(sample_submission, file = "sample_submission.csv", row.names = F)
