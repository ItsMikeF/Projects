# predict season long stats for wr

load_all <- function() {
  #load packages
  suppressMessages({
    library(nflfastR) # pbp data
    library(nflreadr) # nfl schedule and cleaning
    library(tidyverse) # ggplot2 dplyr tibble tidyr purrr forecats 
    library(glue) # interpreted literal strings
    library(caret) # data partition
    library(randomForest) # rf model
    library(openxlsx) # write xlsx files
    library(googlesheets4) # google sheet
  })
  
  # load all contest files
  files <- function(start_year){
    
    # define year
    data_start <<- start_year
    nfl_year <<- year(Sys.Date())
    
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
    
    # remove unncessary objects
    rm(indices)
    
  }
  files(2018)
  
  folder <<- tail(contest_files, 1)
  
  # define year
  nfl_year <<- year(Sys.Date())-1
  
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
             
             #away_team = str_replace_all(away_team, "LA", "LAR"), 
             #away_team = str_replace_all(away_team, "LARC", "LAC"),
             
             #home_team = str_replace_all(home_team, "LA", "LAR"),
             #home_team = str_replace_all(home_team, "LARC", "LAC"),
             
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
  inj <<- load_injuries(2022:nfl_year) %>% 
    mutate(inj_join = paste(season, week, team, full_name, sep = "_"))
  
  # load pbp
  pbp <<- load_pbp(data_start:nfl_year) %>% 
    mutate(weather = as.character(weather))
  
}
load_all()

# calc wr fpts by game week
wr_fpts_pbp <- function(){
  
  # Load regular season data
  wr_pbp <- pbp %>% 
    filter(season_type == "REG") %>% 
    group_by(season, receiver, receiver_id) %>% 
    summarize(
      
      receptions = sum(complete_pass, na.rm = T),
      receiving_yards = sum(receiving_yards, na.rm = T),
      targets = sum(pass_attempt, na.rm = T),
      pass_touchdown = sum(pass_touchdown, na.rm = T),
      fumble_lost = sum(fumble_lost, na.rm = T), 
      
      posteam = last(posteam), 
      week = last(week), 
      season = last(season),
      
      .groups = "drop"
      
    ) %>% 
    drop_na() %>% 
    mutate(join = paste(season, receiver_id, sep = "_"))
  
  # Get receiver rushing stats
  rusher_pbp <- pbp %>% 
    filter(season_type == "REG") %>% 
    group_by(season, rusher, rusher_id) %>% 
    summarise(
      
      rush_attempt = sum(rush_attempt, na.rm = T),
      rushing_yards = sum(rushing_yards, na.rm = T),
      rush_touchdown = sum(rush_touchdown, na.rm = T),
      fumble = sum(fumble, na.rm = T), 
      
      posteam = last(posteam), 
      week = last(week), 
      season = last(season),
      
      .groups = "drop"
      
    ) %>% 
    drop_na() %>% 
    mutate(join = paste(season, rusher_id, sep = "_")) %>% 
    select(join, rush_attempt, rushing_yards, rush_touchdown, fumble)
  
  # join stats and calc fpts, dk scoring
  wr_fpts <<- wr_pbp %>% 
    left_join(rusher_pbp, by=c("join")) %>% 
    replace(is.na(.),0) %>% 
    mutate(
      #big_rush = ifelse(rushing_yards > 100, 1,0), # turn off for season stats
      #big_rec = ifelse(receiving_yards > 100, 1,0), # turn off for season stats
      fpts = 
        
        #big_rush * 3 + # turn off for season stats
        #big_rec * 3 + # turn off for season stats
        
        pass_touchdown * 6 +
        receiving_yards * .1 +
        receptions * 1 +
        
        rushing_yards * .1 +
        rush_touchdown * 6 +
        fumble * -1, 
      
      fpts_ntile = ntile(fpts, 100)
    ) %>% 
    arrange(-fpts) %>% 
    mutate(join = paste(season, posteam, receiver, sep = "_")) %>% 
    relocate(c("fpts", "fpts_ntile"), .after = receiver_id) %>% 
    relocate(c("season","join"), .after = fumble)
  
}
wr_fpts_pbp()

str(wr_fpts)

# Added code to predict 2025 season stats using randomForest

# Define the stats columns to predict
stats_cols <- c("receptions", "receiving_yards", "targets", "pass_touchdown", "fumble_lost",
                "rush_attempt", "rushing_yards", "rush_touchdown", "fumble")

# Create lagged dataset for training
lagged_df <- wr_fpts %>%
  select(receiver_id, receiver, posteam, season, all_of(stats_cols)) %>%
  group_by(receiver_id) %>%
  arrange(season) %>%
  mutate(across(all_of(stats_cols), ~lag(.x), .names = "prev_{col}")) %>%
  ungroup()

# Training data: exclude rows without previous season data
train_data <- lagged_df %>% filter(!is.na(prev_receptions))

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
max_season <- max(wr_fpts$season)
pred_input <- wr_fpts %>%
  filter(season == max_season) %>%
  select(receiver_id, receiver, posteam, all_of(stats_cols)) %>%
  rename_with(~paste0("prev_", .x), all_of(stats_cols)) %>%
  mutate(season = max_season + 1)

# Make predictions for each stat
predictions <- pred_input
for (stat in stats_cols) {
  predictions[[stat]] <- predict(models[[stat]], newdata = predictions)
}

# Calculate predicted fpts (matching original scoring, with big bonuses turned off for season totals)
# Also add big_rush and big_rec for consistency, though not used in fpts
predictions <- predictions %>%
  mutate(
    big_rush = ifelse(rushing_yards > 100, 1, 0),
    big_rec = ifelse(receiving_yards > 100, 1, 0),
    fpts = 
      pass_touchdown * 6 +
      receiving_yards * 0.1 +
      receptions * 1 +
      rushing_yards * 0.1 +
      rush_touchdown * 6 +
      fumble * -1,
    fpts_ntile = ntile(fpts, 100),
    join = paste(season, posteam, receiver, sep = "_")
  ) %>%
  arrange(desc(fpts)) %>%
  relocate(c("fpts", "fpts_ntile"), .after = receiver_id) %>%
  relocate(c("season", "join"), .after = fumble)

# Output structure of predictions
str(predictions)


# 3.0 write to sheets -----------------------------------------------------

library(googlesheets4)

# sheet id
sheet_id <- "https://docs.google.com/spreadsheets/d/1S5x6UyGtP63vVKvXsAARhkklzYDaknFyfAeMvmLmoRU/edit?gid=0#gid=0"

# overwrite an entire sheet
sheet_write(predictions, ss = sheet_id, sheet = "wr")
