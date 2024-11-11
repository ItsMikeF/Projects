# create qb fpts model v2

# 0.0 load packages -------------------------------------------------------

#load packages
suppressMessages({
  library(nflfastR) # pbp data
  library(nflreadr) # nfl schedule and cleaning
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


# 2.1 load schedule -------------------------------------------------------

# define year
nfl_year <- year(Sys.Date())

# load schedule
schedule <- load_schedules(nfl_year)

# set today as target date
target_date <- Sys.Date()
target_row <- which.min(abs((as.Date(schedule$gameday)-target_date)))

game_week = as.numeric(schedule$week[target_row])

# load game week schedule
game_week_schedule <- schedule %>% filter(week == game_week)

depth_charts <- load_depth_charts(seasons = nfl_year) %>% 
  filter(week == game_week) %>%
  filter(position == "RB") %>% 
  filter(depth_position == "RB")
