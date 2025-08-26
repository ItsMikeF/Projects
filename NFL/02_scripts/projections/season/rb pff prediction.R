# project rb pff grade

# ---- packages ----
library(tidyverse)
library(randomForest)
library(glue)

# ---- 0) Load & filter base data ----
seasons <- 2012:2024

rb_list <- lapply(seasons, function(season) {
  read.csv(glue("./01_data/season_grades/nfl/{season}/rushing_summary.csv")) %>%
    mutate(season = season)
})

rb_grades <- bind_rows(rb_list) %>%
  select(player, player_id, position, team_name, season, player_game_count,
         grades_run, grades_offense, grades_hands_fumble, grades_pass_block, grades_run_block,
         attempts, run_plays, yards, yards_after_contact, first_downs, touchdowns, fumbles, 
         breakaway_attempts, breakaway_percent, breakaway_yards, 
         avoided_tackles, elusive_rating) %>%
  filter(position %in% c("HB")) %>%
  arrange(-grades_run) %>% 
  filter(attempts > 54) # median of dataset

# save for reuse
save(rb_grades, file = "./01_data/season_grades/nfl/rb_grades.RDS")
