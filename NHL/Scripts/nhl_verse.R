#explore the nhlverse

#Load packages
library(tidyverse)
library(fastRhockey)
library(hockeyR)

#load data
box_score <- load_nhl_player_box(2021)
logos <- nhl_team_logos
teams <- espn_nhl_teams

#fastRhockey
pbp <- load_nhl_pbp(2022)
names(pbp)

#hockeyR
pbp <- load_pbp(2021)
names(pbp)

pbp %>% 
  filter(event_type %in% c("SHOT", "MISSED_SHOT", "GOAL")) %>% 
  group_by(player = event_player_1_name, id= event_player_1_id) %>% 
  summarize(
    team = last(event_team_abbr), 
    goals = sum(event_type == "GOAL"), 
    xg = round(sum(xg, na.rm = T),1), 
    .groups = "drop"
  ) %>% 
  arrange(-xg) %>% 
  slice_head(n=10)
