# cfbfastr


# 0.0 load packages -------------------------------------------------------


# load packages
library(tidyverse) #
library(cfbfastR) # cfb pbp
library(caret) # classification and regression training
library(car) # companion applied regression


# 1.0 load and process data ----------------------------------------------


# schedule analysis
schedules <- load_cfb_schedules()

# look at this week
current_week <- schedules %>% 
  filter(week == 6) %>% 
  select(week, attendance, venue_id,
         away_team, away_conference, away_pregame_elo, 
         home_team, home_conference, home_pregame_elo) %>% 
  mutate(total_elo = home_pregame_elo + away_pregame_elo) %>% 
  arrange(-total_elo)

# train/test data for models
data <- schedules %>% 
  filter(week < 6) %>% 
  select(week, attendance, venue_id,
         away_team, away_conference, away_pregame_elo, 
         home_team, home_conference, home_pregame_elo) %>% 
  mutate(total_elo = home_pregame_elo + away_pregame_elo) %>% 
  arrange(-total_elo) %>% 
  drop_na()

# select columns for model training
model_data <- data %>% select(venue_id, home_pregame_elo, total_elo, attendance)

# current week for model predictions
current_data <- current_week %>% select(venue_id, home_pregame_elo, total_elo, attendance)

# average attendance by venue
avg_attendance <- model_data %>% 
  group_by(venue_id) %>% 
  summarise(avg_attendance = mean(attendance))


# 2.0 split data  ---------------------------------------------------------


# Assuming 'data' is your dataset
set.seed(123) # for reproducibility

# Splitting the data: 80% for training and 20% for testing
splitIndex <- createDataPartition(model_data$attendance, p = .80, list = FALSE, times = 1)
train_data <- model_data[splitIndex, ]
test_data  <- model_data[-splitIndex, ]


# 3.0 linear model --------------------------------------------------------


# Train a linear model using the training data
lm1_model <- lm(attendance ~ home_pregame_elo + total_elo + venue_id, data = train_data)
vif(lm1_model)

# Use the model to predict attendance in the test set
lm1_predictions <- predict(lm1_model, newdata = test_data)

# Evaluate the model
lm1_results <- data.frame(Actual = test_data$attendance, Predicted = lm1_predictions)

# Calculate Mean Absolute Error (MAE)
lm1_mae <- mean(abs(lm1_results$Actual - lm1_results$Predicted))

# Calculate Root Mean Squared Error (RMSE)
lm1_rmse <- sqrt(mean((lm1_results$Actual - lm1_results$Predicted)^2))

# Display results
print(paste("Mean Absolute Error: ", round(lm1_mae, 2)))
print(paste("Root Mean Squared Error: ", round(lm1_rmse, 2)))


# 4.0 liner regression with polynomial features ---------------------------


lm2_model <- lm(attendance ~ poly(home_pregame_elo, 2) + poly(total_elo, 2) + venue_id, data = model_data)

lm2_predictions <- predict(lm2_model, newdata = test_data)

lm2_results <- data.frame(Actual = test_data$attendance, Predicted = lm2_predictions)

lm2_mae <- mean(abs(lm2_results$Actual - lm2_results$Predicted))

lm2_rmse <- sqrt(mean((lm2_results$Actual - lm2_results$Predicted)^2))

# Display results
print(paste("Mean Absolute Error: ", round(lm2_mae, 2)))
print(paste("Root Mean Squared Error: ", round(lm2_rmse, 2)))


# 5.0 random forest -------------------------------------------------------


library(randomForest)

rf_model <- randomForest(attendance ~ home_pregame_elo + total_elo + venue_id, data = model_data)

rf_predictions <- predict(rf_model, newdata = test_data)

rf_results <- data.frame(Actual = test_data$attendance, Predicted = rf_predictions)

rf_mae <- mean(abs(rf_results$Actual - rf_results$Predicted))

rf_rmse <- sqrt(mean((rf_results$Actual - rf_results$Predicted)^2))

# Display results
print(paste("Mean Absolute Error: ", round(rf_mae, 2)))
print(paste("Root Mean Squared Error: ", round(rf_rmse, 2)))


# 6.0 compare model performance -------------------------------------------


outputs <- cbind(test_data, lm1_predictions, lm2_predictions, rf_predictions)
outputs

error <- data.frame(
  lm1 = c(lm1_mae, lm1_rmse), 
  lm2 = c(lm2_mae, lm2_rmse), 
  rf = c(rf_mae, rf_rmse)
)
error


# 7.0 select best model for current week predictions ----------------------

pred_attendance <- predict(rf_model, newdata = current_data)

current_data_pred <- current_data %>% 
  select(venue_id, home_pregame_elo, total_elo) %>% 
  cbind(pred_attendance) %>% 
  left_join(avg_attendance, by= c("venue_id"))

current_data_pred
