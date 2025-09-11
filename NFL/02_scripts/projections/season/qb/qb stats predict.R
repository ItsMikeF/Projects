# predict season long stats for QB

load_all <- function() {
  
  #load packages
  suppressMessages({
    library(nflfastR) # pbp data
    library(nflreadr) # nfl schedule and cleaning
    library(tidyverse) # ggplot2 dplyr tibble tidyr purrr forcats 
    library(glue) # interpreted literal strings
    library(caret) # data partition
    library(randomForest) # rf model
    library(googlesheets4) # google sheet
  })
  
  # load all contest files
  files <- function(start_year){
    
    # define year
    data_start <<- start_year
    nfl_year <<- year(Sys.Date()) - 1
    
    # define contests 
    contest_files <- list.files(path = "./01_data/contests/")
    contest_files
    
    # remove 2021 weeks bc they dont have all the needed files
    indices <- which(!grepl("2021", contest_files))
    contest_files[indices]
    contest_files <- contest_files[indices]
    
    # remove week 8 for missing files
    contest_files <- contest_files[-which(grepl("2022_w08", contest_files))]
    contest_files
    
    # remove week 1s
    contest_files <<- contest_files[-which(grepl("w01", contest_files))]
    
    # remove unnecessary objects
    rm(indices)
    
  }
  files(2022)
  
  folder <<- tail(contest_files, 1)
  
  # load schedule
  schedule <<- load_schedules(nfl_year)
  
  # set today as target date
  target_date <- Sys.Date()
  
  if (weekdays(target_date) %in% c("Tuesday", "Wednesday")) {
    target_row <- which.min(abs((as.Date(schedule$gameday) - target_date))) + 1
  } else {
    target_row <- which.min(abs((as.Date(schedule$gameday) - target_date)))
  }
  
  contest_week <<- as.numeric(schedule$week[target_row])
  
  # load spreads and totals
  odds <<- function(year){
    odds <<- load_schedules(year) %>% 
      select(season, week, away_team, home_team, spread_line, total_line) %>% 
      mutate(away_spread = spread_line, 
             home_spread = spread_line * -1, 
             week = sprintf("%02d", week), 
             week = as.numeric(week),
             game_id = paste(season, week, away_team, home_team, sep = "_"), 
             away_join = paste(season, week, away_team, sep = "_"), 
             home_join = paste(season, week, home_team, sep = "_")
      ) %>% 
      select(-spread_line)
  }
  odds(nfl_year)
  
  #load injuries
  inj <<- load_injuries(data_start:nfl_year) %>% 
    mutate(inj_join = paste(season, week, team, full_name, sep = "_"))
  
  # load pbp
  pbp <<- load_pbp(data_start:nfl_year) %>% 
    mutate(weather = as.character(weather))
  
}
load_all()

qb_fpts_pbp <- function(){
  
  # Load regular season data
  qb_pbp <- pbp %>% 
    group_by(game_id, passer, passer_id, posteam) %>% 
    summarize(
      
      epa = round(sum(qb_epa, na.rm = T), digits = 2),
      snaps = n(),
      epa_per_play = round(epa/snaps, digits = 2),
      
      pass_attempt = sum(pass_attempt, na.rm = T), 
      passing_yards = sum(passing_yards, na.rm = T), 
      pass_touchdown = sum(pass_touchdown, na.rm = T), 
      interception = sum(interception, na.rm = T), 
      
      qb_scramble = sum(qb_scramble, na.rm = T),
      rush_attempt = sum(rush_attempt, na.rm = T),
      rushing_yards = sum(rushing_yards, na.rm = T),
      rush_touchdown = sum(rush_touchdown, na.rm = T),
      
      fumble = sum(fumble, na.rm = T),
      
      # use to get last non-missing values
      season_type = last(season_type), 
      temp = last(temp), 
      wind = last(wind), 
      spread_line = last(spread_line), 
      total_line = last(total_line), 
      posteam = last(posteam), 
      week = last(week), 
      season = last(season)
      
    ) %>% 
    ungroup() %>% 
    mutate(join = paste(game_id, passer_id, sep = "_"))
  
  # Get rusher rushing stats
  rusher_pbp <- pbp %>% 
    group_by(game_id, rusher, rusher_id) %>% 
    summarise(
      
      rush_attempt = sum(rush_attempt, na.rm = T),
      rushing_yards = sum(rushing_yards, na.rm = T),
      rush_touchdown = sum(rush_touchdown, na.rm = T),
      
      fumble = sum(fumble, na.rm = T)) %>% 
    
    drop_na() %>% 
    ungroup() %>% 
    mutate(join = paste(game_id, rusher_id, sep = "_")) %>% 
    select(-c("game_id"))
  
  # join stats and calc fpts, dk scoring
  qb_fpts <<- qb_pbp %>% 
    left_join(rusher_pbp, by=c("join")) %>% # add passer + rusher stat 
    
    # add zeros so that the columns can be added
    mutate(across(c(rush_attempt.x, rush_attempt.y, 
                    rushing_yards.x, rushing_yards.y, 
                    rush_touchdown.x, rush_touchdown.y, 
                    fumble.x, fumble.y), 
                  ~ replace_na(., 0))) %>% 
    
    # add the columns from scrambles + designed rushes
    mutate(rush_attempt = rush_attempt.x + rush_attempt.y, 
           rushing_yards = rushing_yards.x + rushing_yards.y,
           rush_touchdown = rush_touchdown.x + rush_touchdown.y,
           fumble = fumble.x + fumble.y) %>% 
    select(-c("rush_attempt.x", "rush_attempt.y", "rushing_yards.x", "rushing_yards.y", 
              "rush_touchdown.x", "rush_touchdown.y", "fumble.x", "fumble.y", "join")) %>% 
    
    relocate(c("rush_attempt", "rushing_yards", "rush_touchdown", "fumble"), .after = qb_scramble) %>% 
    
    drop_na(passer) %>% 
    mutate(
      big_rush = ifelse(rushing_yards > 100, 1,0), 
      big_pass = ifelse(passing_yards > 300, 1,0), 
      fpts = 
        
        big_pass * 3 +
        big_rush * 3 +
        
        pass_touchdown * 4 +
        passing_yards * .04 +
        interception * -1 +
        
        rushing_yards * .1 +
        rush_touchdown * 6 +
        fumble * -1, 
      
      fpts_ntile = ntile(fpts, 100)
    ) %>% 
    arrange(-fpts) %>% 
    
    mutate(join = tolower(paste(season, week, posteam, passer, sep = "_"))) %>% 
    
    # add field data from schedule
    left_join(schedule %>% select(game_id, roof), by=c("game_id")) %>% 
    mutate(temp = if_else(roof == "closed" | roof == "dome", 70, temp), 
           wind = if_else(is.na(wind), 0, wind), 
           weather_check = if_else(temp == 0 & wind == 0, 0, 1), 
           dome_games = if_else(roof == "dome" | roof == "closed",1,0)) %>% 
    relocate(weather_check, .after = wind) %>% 
    
    relocate(c("fpts", "fpts_ntile", "spread_line", "total_line"), .after = posteam) %>% 
    relocate(c("snaps", "epa", "epa_per_play"), .after = big_pass)
  
}
qb_fpts_pbp()

# Define the stats columns to predict
stats_cols <- c("pass_attempt", "passing_yards", "pass_touchdown", "interception",
                "rush_attempt", "rushing_yards", "rush_touchdown", "fumble")

# Create lagged dataset for training
lagged_df <- qb_fpts %>%
  select(passer_id, passer, posteam, season, all_of(stats_cols)) %>%
  group_by(passer_id) %>%
  arrange(season) %>%
  mutate(across(all_of(stats_cols), ~lag(.x), .names = "prev_{col}")) %>%
  ungroup()

# Training data: exclude rows without previous season data
train_data <- lagged_df %>% filter(!is.na(prev_pass_attempt))

# Train a randomForest model for each stat
set.seed(123)  # For reproducibility
models <- list()
for (stat in stats_cols) {
  prev_vars <- paste("prev", stats_cols, sep = "_")
  formula_str <- paste(stat, "~", paste(prev_vars, collapse = " + "))
  formula <- as.formula(formula_str)
  models[[stat]] <- randomForest(formula, data = train_data, ntree = 100)
}

# Prepare input for 2025 predictions (using 2024 as previous)
max_season <- max(qb_fpts$season)
pred_input <- qb_fpts %>%
  filter(season == max_season) %>%
  select(passer_id, passer, posteam, all_of(stats_cols)) %>%
  rename_with(~paste0("prev_", .x), all_of(stats_cols)) %>%
  mutate(season = max_season + 1)

# Make predictions for each stat
predictions <- pred_input
for (stat in stats_cols) {
  predictions[[stat]] <- predict(models[[stat]], newdata = predictions)
}

# Calculate predicted fpts (matching original scoring, with big bonuses turned off for season totals)
# Also add big_rush and big_pass for consistency, though not used in fpts
predictions <- predictions %>%
  mutate(
    big_rush = ifelse(rushing_yards > 100, 1, 0),
    big_pass = ifelse(passing_yards > 300, 1, 0),
    fpts = 
      pass_touchdown * 4 +
      passing_yards * 0.04 +
      interception * -1 +
      rushing_yards * 0.1 +
      rush_touchdown * 6 +
      fumble * -1,
    fpts_ntile = ntile(fpts, 100),
    join = paste(season, posteam, passer, sep = "_")
  ) %>%
  arrange(desc(fpts)) %>%
  relocate(c("fpts", "fpts_ntile"), .after = passer_id) %>%
  relocate(c("season", "join"), .after = fumble)

# Output structure of predictions
str(predictions)

# 3.0 write to sheets -----------------------------------------------------

library(googlesheets4)

# sheet id
sheet_id <- "https://docs.google.com/spreadsheets/d/1S5x6UyGtP63vVKvXsAARhkklzYDaknFyfAeMvmLmoRU/edit?gid=0#gid=0"

# overwrite an entire sheet
sheet_write(predictions, ss = sheet_id, sheet = "qb")