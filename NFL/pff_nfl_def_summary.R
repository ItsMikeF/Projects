#load packages
library(tidyverse, warn.conflicts = F) #metapackage

#load pff merge files
def <- read.csv("def.csv")
names(def)

#rushing defense summary
def_rush <- def %>% 
  group_by(team_name, year, week) %>% 
  summarise(
    avdt = round(weighted.mean(avg_depth_of_tackle, tackles.y, na.rm = T), digits = 2),
    stops = round(weighted.mean(stops.y / player_game_count, snap_counts_run, na.rm = T), digits = 2),
    missed_tackles = round(weighted.mean(missed_tackles.y / player_game_count, snap_counts_run, na.rm = T), digits = 4), 
    forced_fumbles = round(weighted.mean(forced_fumbles.y / player_game_count, snap_counts_run, na.rm = T), digits = 4)
    )

def_rush$merge <- paste0(def_rush$team_name.x, def_rush$year, def_rush$week)

rbs <- read.csv("rbs.csv")
