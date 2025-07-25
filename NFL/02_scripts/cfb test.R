# college football data

# Load libraries
suppressMessages({

})
# load packages
library(nflverse)
library(cfbfastR)
library(tidyverse)
library(tidymodels)

# load pbp
cfb_pbp <- load_cfb_pbp(seasons = 2024)

# find column names
unique(cfb_pbp$pos_team)


# team level plays
plays <- cfb_pbp %>% 
  group_by(pos_team) %>% 
  summarize(epa = round(mean(EPA, na.rm = T), digits = 3),
            n = n(), 
            game_count = n_distinct(game_id)) %>% 
  arrange(epa) %>% 
  filter(game_count > max(game_count)*0.5) 

# qb
qb_pbp <- cfb_pbp %>% 
  group_by(passer_player_name, pos_team) %>% 
  summarize(
    
    epa = round(sum(EPA, na.rm = T), digits = 2),
    snaps = n(),
    epa_per_play = round(epa/snaps, digits = 2),
    
    pass_attempt = sum(pass_attempt, na.rm = T), 
    pass_yards = sum(yds_receiving, na.rm = T), 
    pass_touchdown = sum(pass_td, na.rm = T), 
    interception = sum(int, na.rm = T)) %>% 
  arrange(-pass_yards) %>% 
  drop_na(passer_player_name) %>% 
  filter(snaps > 200)

unique(cfb_pbp$rush_yds)

# rb
rb_pbp <- cfb_pbp %>% 
  #filter(rush == 1) %>% 
  group_by(rusher_player_name, pos_team) %>% 
  summarise(
    
    epa = round(sum(EPA, na.rm = T), digits = 2),
    snaps = n(),
    epa_per_play = round(epa/snaps, digits = 2),
    
    rush_attempt = sum(rush, na.rm = T),
    rush_yards = sum(yards_gained, na.rm = T),
    rush_touchdown = sum(rush_td, na.rm = T)
    ) %>% 
  mutate(ypa = round(rush_yards / rush_attempt, digits = 2)) %>% 
  drop_na(rusher_player_name) %>% 
  arrange(-rush_yards) %>% 
  filter(snaps > 100)
