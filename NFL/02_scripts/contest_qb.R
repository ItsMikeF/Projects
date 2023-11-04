# create a list of all the qb slates

# 0.0 load packages -------------------------------------------------------

#load packages
suppressMessages({
  library(nflfastR) # pbp data
  library(nflreadr) # nfl schedule
  library(tidyverse) # ggplot2 dplyr tibble tidyr purrr forecats 
  library(glue) # interpreted literal strings
  library(caret) # data partition
  library(randomForest) # rf model
  library(xgboost) # xgb model
})

# 1.0 ---------------------------------------------------------------------

files <- function(){
  
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
files()

# 2.0 load pbp and calc fpts ----------------------------------------------

# load pbp
pbp <- load_pbp(2022:2023)

# calc rb fpts by game week
qb_fpts_pbp <- function(){
  
  # Load regular season data
  qb_pbp <- pbp %>% 
    group_by(passer, passer_id, posteam, week, season) %>% 
    summarize(
      
      passing_yards = sum(passing_yards, na.rm = T), 
      pass_attempt = sum(pass_attempt, na.rm = T), 
      pass_touchdown = sum(pass_touchdown, na.rm = T), 
      interception = sum(interception, na.rm = T)
      
    ) %>% 
    drop_na() %>% 
    ungroup() %>% 
    mutate(join = paste(season, week, passer_id, sep = "_"))
  
  # Get passer rushing stats
  passer_pbp <- pbp %>% 
    group_by(passer, passer_id, posteam, week, season) %>% 
    summarise(
      
      qb_scramble = sum(qb_scramble, na.rm = T),
      rush_attempt = sum(rush_attempt, na.rm = T),
      rushing_yards = sum(rushing_yards, na.rm = T),
      rush_touchdown = sum(rush_touchdown, na.rm = T),
      
      fumble = sum(fumble, na.rm = T)) %>% 
    drop_na() %>% 
    ungroup() %>% 
    mutate(join = paste(season, week, passer_id, sep = "_"))
  
  # Get rusher rushing stats
  rusher_pbp <- pbp %>% 
    group_by(rusher, rusher_id, posteam, week, season) %>% 
    summarise(
      
      rush_attempt = sum(rush_attempt, na.rm = T),
      rushing_yards = sum(rushing_yards, na.rm = T),
      rush_touchdown = sum(rush_touchdown, na.rm = T),
      
      fumble = sum(fumble, na.rm = T)) %>% 
    drop_na() %>% 
    ungroup() %>% 
    mutate(join = paste(season, week, rusher_id, sep = "_"))
  
  qb_rush <- passer_pbp %>%
    left_join(rusher_pbp, by = "join") %>%
    mutate(across(c(rush_attempt.x, rush_attempt.y, 
                    rushing_yards.x, rushing_yards.y, 
                    rush_touchdown.x, rush_touchdown.y, 
                    fumble.x, fumble.y), 
                  ~ replace_na(., 0))) %>% 
    mutate(rush_attempt = rush_attempt.x + rush_attempt.y, 
           rushing_yards = rushing_yards.x + rushing_yards.y,
           rush_touchdown = rush_touchdown.x + rush_touchdown.y,
           fumble = fumble.x + fumble.y) %>% 
    select(passer_id, rush_attempt, rushing_yards, rush_touchdown, fumble, join)
  
  # join stats and calc fpts, dk scoring
  qb_fpts <<- qb_pbp %>% 
    left_join(qb_rush, by=c("join")) %>% 
    replace(is.na(.),0) %>% 
    mutate(
      big_rush = ifelse(rushing_yards > 100, 1,0), 
      big_pass = ifelse(passing_yards > 100, 1,0), 
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
    mutate(join = paste(season, week, posteam, passer, sep = "_"))
  
  # remove objects
  rm(qb_pbp, rb_pbp)
}
qb_fpts_pbp()

# 2.1 load spreads and totals ---------------------------------------------

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
           game_id = paste(season, week, away_team, home_team, sep = "_")
    ) %>% 
    select(-spread_line)
}
odds(2022:2023)

# 2.2 load injuries -------------------------------------------------------

inj <- load_injuries(2022:2023) %>% 
  mutate(inj_join = paste(season, week, team, full_name, sep = "_"))
