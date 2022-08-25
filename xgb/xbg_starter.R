library(tidyverse) # metapackage of all tidyverse packages
library(xgboost)

data(agaricus.train, package='xgboost')
data(agaricus.test, package='xgboost')
train <- agaricus.train
test <- agaricus.test

str(train)
dim(train$data)
dim(test$data)
class(train$data)[1]
class(train$label)

#basic training using the XGB boost
bstSparse <- xgboost(data = train$data,
                     label = train$label,
                     max.depth = 2,
                     eta = 1,
                     nthread = 2,
                     nrounds = 2,
                     objective = "binary:logistic")

#parametric variations 
#Dense matrix,we can do this alternatively
bstDense <- xgboost(data = as.matrix(train$data),
                    label = train$label,
                    max.depth = 2,
                    eta = 1,
                    nthread = 2,
                    nrounds = 2,
                    objective = "binary:logistic")

#this will be helpful when you discover the most advance data in it
dtrain <- xgb.DMatrix(data = train$data, 
                      label = train$label)
bstDMatrix <- xgboost(data = dtrain,
                      max.depth = 2,
                      eta = 1,
                      nthread = 2,
                      nrounds = 2,
                      objective = "binary:logistic")

#verbose help to view the learning process of the dataset
# verbose = 0, no message
bst <- xgboost(data = dtrain,
               max.depth = 2,
               eta = 1,
               nthread = 2,
               nrounds = 2,
               objective = "binary:logistic",
               verbose = 0)

#now we will going to perform the basic prediction using the xgboost
pred <- predict(bst, test$data)

# size of the prediction vector
print(length(pred))

#Measuring the model performance
err <- mean(as.numeric(pred > 0.5) != test$label)
print(paste("test-error=", err))

#advance features
#dataset prepration
dtrain <- xgb.DMatrix(data = train$data, label=train$label)
dtest <- xgb.DMatrix(data = test$data, label=test$label)

#machine learning progress wiht xgb.train
watchlist <- list(train=dtrain, test=dtest)

bst <- xgb.train(data=dtrain,
                 max.depth=2,
                 eta=1,
                 nthread = 2,
                 nrounds=2,
                 watchlist=watchlist,
                 objective = "binary:logistic")

bst <- xgb.train(data=dtrain,
                 max.depth=2,
                 eta=1,
                 nthread = 2,
                 nrounds=2,
                 watchlist=watchlist,
                 eval.metric = "error",
                 eval.metric = "logloss",
                 objective = "binary:logistic")

#linear boosting
bst <- xgb.train(data=dtrain,
                 booster = "gblinear",
                 nthread = 2,
                 nrounds=2,
                 watchlist=watchlist,
                 eval.metric = "error",
                 eval.metric = "logloss",
                 objective = "binary:logistic")

#manipulating xgb.DMatrix
xgb.DMatrix.save(dtrain, "dtrain.buffer")

# to load it in, simply call xgb.DMatrix
dtrain2 <- xgb.DMatrix("dtrain.buffer")

bst <- xgb.train(data=dtrain2,
                 max.depth=2,
                 eta=1,
                 nthread = 2,
                 nrounds=2,
                 watchlist=watchlist,
                 objective = "binary:logistic")

#information extraction,here we will be extracting the label dataset
label = getinfo(dtest, "label")
pred <- predict(bst, dtest)
err <- as.numeric(sum(as.integer(pred > 0.5) != label))/length(label)
print(paste("test-error=", err))

#View feature importance/influence from the learnt model
importance_matrix <- xgb.importance(model = bst)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)

#View the trees from a model
xgb.dump(bst, with_stats = TRUE)

xgb.plot.tree(model = bst)

#save and load the model
# save model to binary local file
xgb.save(bst, "xgboost.model")

# load binary model to R
bst2 <- xgb.load("xgboost.model")
pred2 <- predict(bst2, test$data)

# And now the test
print(paste("sum(abs(pred2-pred))=", sum(abs(pred2-pred))))

# save model to R's raw vector
rawVec <- xgb.save.raw(bst)

# print class
print(class(rawVec))

# load binary model to R
bst3 <- xgb.load(rawVec)
pred3 <- predict(bst3, test$data)

# pred3 should be identical to pred
print(paste("sum(abs(pred3-pred))=", sum(abs(pred3-pred))))
