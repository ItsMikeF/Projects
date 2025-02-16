# get nhl schedule

# packages
library(openxlsx)
library(lubridate)
library(tidyverse)
library(fastRhockey)

# load teams logos colors
teams <- fastRhockey::espn_nhl_teams()

# load and process schedule
schedule <- read.xlsx("./01_data/schedule/2024-25 Official NHL Schedule.xlsx") %>% 
  rename_with(tolower) %>% 
  mutate(date = as_date(date, origin = "1899-12-30"),
         
         eastern = as.numeric(eastern)* 24, 
         local = as.numeric(local)*24, 
         
         away = str_replace_all(away, "N.Y. ", ""),
         home = str_replace_all(home, "N.Y. ", ""), 
         
         away = str_replace_all(away, "Rangers", "NYR"), 
         home = str_replace_all(home, "Rangers", "NYR"), 
         
         away = str_replace_all(away, "Islanders", "NYI"), 
         home = str_replace_all(home, "Islanders", "NYI")) %>% 


  left_join(teams %>% select(team, abbreviation), 
            by= c("away" = "team")) %>% 
  left_join(teams %>% select(team, abbreviation), 
            by= c("home" = "team")) %>% 
  
  rename(away_abbr = abbreviation.x) %>% 
  rename(home_abbr = abbreviation.y) %>% 
  
  mutate(game_id = paste(date, away_abbr, home_abbr, sep = "_"))

slate_date <- Sys.Date()

schedule %>% filter(date == slate_date)

schedule %>% distinct(game_id)
