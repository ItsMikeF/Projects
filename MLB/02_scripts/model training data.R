# training data for projection system

# 1.0 load packages and data ----------------------------------------------

# load packages
library(baseballr)
library(mlbplotR)
library(tidyverse)
library(glue)

# load mlb team data
mlb_teams <- mlbplotR::load_mlb_teams()

#load fangraphs leaderboard data

season = year(Sys.Date())

# load pitcher ids
pitchers <- fg_pitcher_leaders(startseason = season, endseason = season)
pitcher_ids <- pitchers %>% select(PlayerName, playerid)

# load batters
batters <- fg_batter_leaders(startseason = season, endseason = season)

# Load opponent batter K rates 
team_batters <- fg_team_batter(startseason = season, endseason = season)

# load savant data
savant_pitchers <- statcast_leaderboards(year = season, 
                                         leaderboard = "expected_statistics",
                                         player_type = "pitcher", 
                                         min_pa = "q") %>% 
  separate(`last_name, first_name`, 
           into = c("last_name","first_name"), 
           sep = ", ") %>% 
  unite("fullName", first_name, last_name, sep = " ") %>% 
  filter(pa >= max(pa) * 0.375) %>% 
  arrange(est_woba)

# load savant data
savant_batters <- statcast_leaderboards(year = season, 
                                         leaderboard = "expected_statistics",
                                         player_type = "batter", 
                                         min_pa = "q") %>% 
  separate(`last_name, first_name`, 
           into = c("last_name","first_name"), 
           sep = ", ") %>% 
  unite("fullName", first_name, last_name, sep = " ") %>% 
  filter(pa >= max(pa) * 0.5) %>% 
  arrange(est_woba)
