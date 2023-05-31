# predict team epa of nfl defenses based on pff grade of espn depth chart

# Load packages
library(dplyr)
library(tidyr)
library(caret)

# run dfs nfl defense script tp get pbp_def

# run depth chart script for nfl defense

# need to project every player

# load def

# 0.0 Define Inputs -------------------------------------------------------

week = 17
pbp <- load_pbp(2022)
wp_lower = 0.1
wp_upper = 0.9
half_seconds_remaining = 120

# 1.0 defense epa table -------------------------------------------------------

epa_def_team <- lapply(2014:2022, function(year){
  #define the values
  print(paste("Load pbp of the", year, "season"))
  pbp <- load_pbp(year)
  week = 17
  wp_lower = 0.1
  wp_upper = 0.9
  half_seconds_remaining = 120
  print_plot = "no"
  
  #def pass epa
  pbp_def_pass <- pbp %>% 
    filter(pass == 1 &
             wp > 0.1 &
             wp < 0.9 &
             half_seconds_remaining > 120) %>% 
    group_by(defteam) %>% 
    summarize(def_pass_epa = round(mean(epa), digits = 3),
              n_plays = n()) %>% 
    arrange(def_pass_epa) %>% 
    mutate(def_pass_epa_rank = round(rank(def_pass_epa), digits = 0), 
           def_pass_epa_sd = round((def_pass_epa - weighted.mean(def_pass_epa, n_plays, na.rm=T)) / sd(def_pass_epa, na.rm = T), digits = 2))
  
  #def rush epa
  pbp_def_rush <- pbp %>% 
    filter(rush == 1 &
             wp > 0.1 &
             wp < 0.9 &
             half_seconds_remaining > 120) %>% 
    group_by(defteam) %>% 
    summarize(def_rush_epa = round(mean(epa), digits = 3),
              n_plays = n()) %>% 
    arrange(def_rush_epa) %>% 
    mutate(def_rush_epa_rank = round(rank(def_rush_epa), digits = 0), 
           def_rush_epa_sd = round((def_rush_epa - weighted.mean(def_rush_epa, n_plays, na.rm=T)) / sd(def_rush_epa, na.rm = T), digits = 2))
  
  pbp_def <- pbp_def_pass %>% 
    left_join(pbp_def_rush, by = c('defteam')) %>% 
    mutate(total_plays = n_plays.x + n_plays.y,
           #defteam = gsub('JAX','JAC', defteam), 
           defteam = gsub('LA','LAR', defteam), 
           defteam = gsub('LARC','LAC', defteam), 
           year = year)
  
})

# bind data to a dataframe
epa_def_team_df <- bind_rows(epa_def_team)

# add blank df for 2023

teams <- data.frame(matrix(ncol = 11, nrow = 32))
teams$X1 <- unique(epa_def_team_df$defteam)
teams$X11 <- 2023
names(teams) <- names(epa_def_team_df)
teams <- replace(teams, is.na(teams), 0)

test <- rbind(epa_def_team_df, teams)

# Step 3: Split the data
set.seed(1)
train_set <- test[which(test$year < 2023),]
test_set  <- test[which(test$year == 2023),]

# Step 4 Choose a regression model
model <- train(def_pass_epa_rank ~., data = train_set, method = "lm")

# Step 5 Train the regression model
trained_model <- model$finalModel
trained_model

# Step 6 Evaluate the model
predictions <- predict(trained_model, newdata = test_set )

# Step 7 Predict 2023 rankings
input_features <- test_set[, !colnames(test_set) %in% c("year", "defteam", "def_pass_epa_rank")]

predicted_rankings <- predict(trained_model, newdata = input_features)

print(predicted_rankings) 
