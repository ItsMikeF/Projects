library(xgboost)
library(Matrix)
library(data.table)
library(vcd)

data(Arthritis)
df <- data.table(Arthritis, keep.rownames = FALSE)
str(df)

head(df[,AgeDiscret := as.factor(round(Age/10,0))])
head(df[,AgeCat:= as.factor(ifelse(Age > 30, "Old", "Young"))])

df[,ID:=NULL]
levels(df[,Treatment])

sparse_matrix <- sparse.model.matrix(Improved~.-1, data = df)
head(sparse_matrix)

output_vector = df[,Improved] == "Marked"

bst <- xgboost(data = sparse_matrix,
               label = output_vector,
               max.depth = 4,
               eta = 1,
               nthread = 2,
               nrounds = 10,
               objective = "binary:logistic")

importance <- xgb.importance(feature_names = sparse_matrix@Dimnames[[2]], model = bst)
head(importance)

importanceRaw <- xgb.importance(feature_names = sparse_matrix@Dimnames[[2]], model = bst, data = sparse_matrix, label = output_vector)

# Cleaning for better display
importanceClean <- importanceRaw[,`:=`(Cover=NULL, Frequency=NULL)]

head(importanceClean)

xgb.plot.importance(importance_matrix = importanceRaw)
