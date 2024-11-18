# calc game spread based on epa

# 0.0 load packages -------------------------------------------------------


#load packages
suppressMessages({
  options(nflreadr.verbose = FALSE)
  library(nflfastR) # pbp data
  library(nflreadr) # nfl schedule
  library(tidyverse) # ggplot2 dplyr tibble tidyr purrr forecats 
  library(glue) # interpreted literal strings
  library(caret) # data partition
  library(randomForest) # rf model
  library(xgboost) # xgb model
})

# define year
nfl_year <- year(Sys.Date())

# load pbp data
pbp <- load_pbp(2022:nfl_year)

# load odds
odds <- function(year){
  odds <<- load_schedules(year) %>% 
    select(season, week, away_team, home_team, spread_line, total_line) %>% 
    mutate(away_spread = spread_line, 
           home_spread = spread_line * -1, 
           away_team = str_replace_all(away_team, "LA", "LAR"), 
           away_team = str_replace_all(away_team, "LARC", "LAC"),
           home_team = str_replace_all(home_team, "LA", "LAR"),
           home_team = str_replace_all(home_team, "LARC", "LAC"),
           week = sprintf("%02d", week), 
           week = as.numeric(week),
           game_id = paste(season, week, away_team, home_team, sep = "_"), 
           away_join = paste(season, week, away_team, sep = "_"), 
           home_join = paste(season, week, home_team, sep = "_")
    ) %>% 
    select(-spread_line)
}
odds(2022:nfl_year)

# define game week for model prediction
game_week = 10

# define all game weeks
game_weeks <- expand_grid(year = 2022:2024, week = 1:18)

# calculate team epa per play by week
team_d_epa <- lapply(1:nrow(game_weeks), function(week) {
  
  # define game year
  game_year = game_weeks$year[week]
  game_week = game_weeks$week[week]
  
  # def pass epa
  pbp_def_pass <- pbp %>% 
    filter(pass == 1 &
             week < game_week &
             season == game_year) %>% 
    group_by(defteam) %>% 
    summarize(def_pass_epa = round(mean(epa, na.rm = T), digits = 3),
              n_plays = n()) %>% 
    arrange(def_pass_epa) %>% 
    mutate(def_pass_epa_rank = round(rank(def_pass_epa), digits = 0), 
           def_pass_epa_sd = round((def_pass_epa - weighted.mean(def_pass_epa, n_plays, na.rm=T)) / sd(def_pass_epa, na.rm = T), digits = 2))
  
  #def rush epa
  pbp_def_rush <- pbp %>% 
    filter(rush == 1 &
             week < game_week &
             season == game_year) %>% 
    group_by(defteam) %>% 
    summarize(def_rush_epa = round(mean(epa, na.rm = T), digits = 3),
              n_plays = n()) %>% 
    arrange(def_rush_epa) %>% 
    mutate(def_rush_epa_rank = round(rank(def_rush_epa), digits = 0), 
           def_rush_epa_sd = round((def_rush_epa - weighted.mean(def_rush_epa, n_plays, na.rm=T)) / sd(def_rush_epa, na.rm = T), digits = 2))
  
  pbp_def <<- pbp_def_pass %>% 
    left_join(pbp_def_rush, by = c('defteam')) %>% 
    mutate(total_plays = n_plays.x + n_plays.y,
           defteam = gsub('LA','LAR', defteam), 
           defteam = gsub('LARC','LAC', defteam), 
           avg_rank = (def_rush_epa_rank + def_pass_epa_rank) /2, 
           season = game_year, 
           game_week = game_week)
})

team_d_epa_df <- bind_rows(team_d_epa)
