# mahomes model

# load packages
library(nflverse)
library(tidyverse)
library(caret)
library(glue)

# load defnese data
load("./01_data/training_data/position_groups/team_def.RData")

# load pbp
pbp <- load_pbp(2014:2023)

# calc fpts by week
player <- pbp %>% 
  filter(pass == 1 & passer == "P.Mahomes" & week < 18) %>% 
  group_by(passer, posteam, defteam, week, game_date) %>% 
  summarize(pass_attempt = sum(pass_attempt, na.rm = T),
            passing_yards = sum(passing_yards, na.rm = T),
            pass_touchdown = sum(pass_touchdown, na.rm = T),
            interception = sum(interception, na.rm = T),
            
            rushing_yards = sum(rushing_yards, na.rm = T),
            rush_attempt = sum(rush_attempt, na.rm = T),
            rush_touchdown = sum(rush_touchdown, na.rm = T),
            fumble_lost = sum(fumble_lost, na.rm = T),
            
            epa = round(mean(qb_epa), digits = 3),
            cpoe = round(mean(cpoe, na.rm = T), digits = 2)
  ) %>% 
  mutate(big_py = ifelse(passing_yards > 300, 1,0), 
         fpts = 
           pass_touchdown * 4 +
           passing_yards * .04 +
           interception * -1 +
           big_py * 3 +
           
           rushing_yards * .1 +
           rush_touchdown * 6 +
           fumble_lost * -1, 
         year = year(game_date), 
         off_join = paste0(year, week, posteam), 
         def_join = paste0(year, week, defteam)) %>% 
  select(game_date, year, week, passer, fpts, epa, defteam, posteam, off_join, def_join) %>% 
  ungroup() %>% 
  arrange(desc(game_date))

# join pff team defense data
player <- player %>% 
  left_join(team_def %>% select(-c("week", "year")), by=c("def_join"))

# join offensive live
load("./01_data/training_data/position_groups/ol.RData")

team_ol <- ol %>% 
  group_by(team_name, year, week) %>% 
  summarise(grades_pass_block = weighted.mean(grades_pass_block, snap_counts_pass_block)) %>% 
  ungroup() %>% 
  mutate(off_join = paste0(year, week, team_name))

player <- player %>% 
  left_join(team_ol %>% select(off_join, grades_pass_block), by=c("off_join"))

# get model data
model_data <- player %>% 
  select(fpts, def, prsh, cov, grades_pass_block) %>% 
  drop_na()

# split data
split_index <- createDataPartition(model_data$fpts, 
                                   p = 0.8, 
                                   list = F, 
                                   times = 1)
train_data <- model_data[split_index, ]
test_data <- model_data[-split_index, ]


# train model -------------------------------------------------------------


# liner model
lm1_model <- lm(fpts ~ def + prsh + cov + grades_pass_block, 
                data = train_data)

lm1_proj <- predict(lm1_model, newdata = test_data)

lm1_results <- data.frame(Actual = test_data$fpts, 
                          Predicted = lm1_proj)

lm1_mae <- mean(abs(lm1_results$Actual - lm1_results$Predicted))

lm1_rmse <- sqrt(mean((lm1_results$Actual - lm1_results$Predicted)^2))

# Display results
print(paste("Mean Absolute Error: ", round(lm1_mae, 2)))
print(paste("Root Mean Squared Error: ", round(lm1_rmse, 2)))

# random forest model

library(randomForest)

rf_model <- randomForest(fpts ~ def + prsh + cov + grades_pass_block, 
                         data = train_data)

rf_proj <- predict(rf_model, 
                   newdata = test_data)

rf_results <- data.frame(Actual = test_data$fpts, 
                         Predicted = rf_proj)

rf_mae <- mean(abs(rf_results$Actual - rf_results$Predicted))

rf_rmse <- sqrt(mean((rf_results$Actual - rf_results$Predicted)^2))

# Display results
print(paste("Mean Absolute Error: ", round(rf_mae, 2)))
print(paste("Root Mean Squared Error: ", round(rf_rmse, 2)))

save(rf_model, file = glue("./04_models/qbs/model_mahomes.RData"))
load("./04_models/qbs/model_mahomes.RData")

# determine opponent
schedule <- load_schedules(2023) %>% 
  filter(away_team == first(player$posteam) | home_team == first(player$posteam)) %>% 
  filter(week == 7) %>% 
  mutate(opponent  = if_else(away_team == first(player$posteam), home_team, away_team))

schedule$opponent
