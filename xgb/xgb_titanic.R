library(tidyverse)  # metapackage of all tidyverse packages
library(imputeMissings)
library(glmnet)
library(pROC)
library(ggthemes)
library(randomForest)
library(caret)

#Data Import and Cleaning
test <- read_csv("C:/Users/mikef/Documents/GitHub/DFS_Data/Data/Titanic/test.csv", show_col_types = FALSE)
train <- read_csv("C:/Users/mikef/Documents/GitHub/DFS_Data/Data/Titanic/train.csv", show_col_types = FALSE)

#Check for Missing Values
train %>% summarise(across(everything(), ~sum(is.na(.))))
test %>% summarise(across(everything(), ~sum(is.na(.))))

#Clean Data and Create new Variables
train <- train %>% 
  mutate(cabin_flag = ifelse(Cabin == "", 0, 1)) %>% 
  mutate(Embarked = replace(Embarked, Embarked == "", NA)) %>% 
  mutate(parch_new = ifelse(Parch > 0, "Yes", "No"),
         sibsp_new = ifelse(SibSp > 0, "Yes", "No"))

test <- test %>% 
  mutate(cabin_flag = ifelse(Cabin == "", 0, 1)) %>% 
  mutate(Embarked = replace(Embarked, Embarked == "", NA)) %>% 
  mutate(parch_new = ifelse(Parch > 0, "Yes", "No"),
         sibsp_new = ifelse(SibSp > 0, "Yes", "No"))

#Impute Missing values
test2 <- impute(test)
train2 <- impute(train)

#Feature Selection with LASSO
#First create a dataset of just the variables 
#not the ones that definitely can't be used.
model_data <- train2 %>% 
  select(-PassengerId, -Name, -Ticket, -Cabin)

#Convert the data into a model matrix.
set.seed(123)

preds <- model.matrix(Survived ~., model_data)[,-1]

response <- model_data$Survived

#LASSO Output
set.seed(234)
cv.1 <- cv.glmnet(x=preds, y=response, alpha = 1, family = 'binomial')

lasso.1 <- glmnet(x=preds, y=response, alpha=1, family="binomial", lambda = cv.1$lambda.min)

coef(lasso.1)

#Binomial Regression Model
mod1 <- glm(Survived ~ Pclass + 
              Sex + 
              Age + 
              SibSp + 
              Parch + 
              Fare + 
              Embarked,
            data = train2, 
            family = "binomial")

summary(mod1)

#Test Alternate GLM
mod2 <- glm(Survived ~ Pclass + 
              Sex + 
              Age + 
              sibsp_new + 
              parch_new + 
              Fare + 
              Embarked, 
            data = train2, 
            family = "binomial")

summary(mod2)

#Model 1 Metrics
set.seed(345)

pred.p1 <- predict(mod1, newdata=train2, type = "response")

pred.class1 <- factor(ifelse(pred.p1 > 0.5, 1, 0))

cmat <- caret::confusionMatrix(pred.class1, as.factor(train2$Survived), positive = "1")
cmat

#ROC Plot for Model 1
roc.m1 <- roc(train2$Survived, pred.p1)
auc.m1 <- round(auc(train2$Survived, pred.p1),3)

ggroc(roc.m1, size = 1.5, color = "#008FD5")+
  annotate("text", x=.25, y=.75, 
           label= paste0("AUC = " , auc.m1))+
  xlab("Specificity (True Negative Rate)")+
  ylab("Sensitivity (True Positive Rate)")+
  coord_fixed()+
  theme_fivethirtyeight()

#Random Forest Model
train_fac <- train2 %>% mutate(Survived = factor(Survived, ordered = TRUE))
mod3 <- randomForest(Survived ~ Pclass + 
                       Sex + 
                       Age + 
                       SibSp + 
                       Parch + 
                       Fare + 
                       Embarked, data=train_fac)

#Confusion Matrix Model 3
p3 <- predict(mod3, train_fac)

confusionMatrix(p3, train_fac$Survived)

plot(mod3)

#Random Forest Prediction on Test Data
test_pred <- predict(mod3, test2)

final_preds <- test2 %>% 
  select(PassengerId) %>% 
  mutate(Survived = test_pred)

head(final_preds)
write_csv(final_preds, "submission.csv")
