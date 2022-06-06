#Load packages
library(tidyverse, warn.conflicts = F)
library(fastRhockey, warn.conflicts = F)

#load data
box_score <- load_nhl_player_box(2021)
logos <- nhl_team_logos
teams <- espn_nhl_teams

#pbp
pbp <- load_nhl_pbp(2022)
names(pbp)
try(nhl_draft(2021))
