# create a list of all the wr slates

# 0.0 load packages -------------------------------------------------------


# load packages
suppressMessages({
  
  
  #nflverse packages
  options(nflreadr.verbose = FALSE)
  library(nflfastR) # pbp data
  library(nflreadr) # nfl schedule
  library(tidyverse) # ggplot2 dplyr tibble tidyr purrr forecats 
  
  # data pacakges
  library(glue) # interpreted literal strings
  
  # modeling packages
  library(caret) # data partition
  library(randomForest) # rf model
  library(ranger) # fast implementation of random forest
  #library(MultivariateRandomForest) # models multivariate cases using random forests
  
  
})


# 1.0 ---------------------------------------------------------------------


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
files(2022)


# 2.0 nflverse data ---------------------------------------------


# load spreads and totals
odds <- function(year){
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
odds(2022:nfl_year)

#load injuries
inj <- load_injuries(2022:nfl_year) %>% 
  mutate(inj_join = paste(season, week, team, full_name, sep = "_"))

# load schedule
schedule <- load_schedules(2022:nfl_year)


# 2.1 load pbp and calc fpts ----------------------------------------------


# load pbp
pbp <- load_pbp(data_start:nfl_year) %>% 
  mutate(weather = as.character(weather))