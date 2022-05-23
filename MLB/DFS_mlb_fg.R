library(tidyverse, warn.conflicts = F)
library(ggrepel)
library(lubridate, warn.conflicts = F)
library(utils)
library(filesstrings, warn.conflicts = F)
library(xtable)
library(tictoc)
library(lpSolve)
library(stats)
library(XML)
library(binr)
library(httr)
library(xgboost, warn.conflicts = F)

### Inputs ###
date <- c("2022-04-23")

### Set Working Directory ###
setwd(paste0("C://Users//",unlist(strsplit(getwd(), "/"))[3],"//Documents//GitHub//DFS_Data//Data_MLB//", date))

### Import CSVs ###
mlb_batting <- read.csv("FanGraphs Leaderboard batting.csv")
mlb_pitching <- read.csv("FanGraphs Leaderboard pitching.csv")
mlb_salaries <- read.csv("DKSalaries.csv")

fg_team_vs_lhp <- read.csv("FanGraphs Leaderboard team v LHP.csv")
fg_team_vs_rhp <- read.csv("FanGraphs Leaderboard team v RHP.csv")

fg_team_batting <- read.csv("FanGraphs Leaderboard team batting.csv")

### Adjustments ###
names(mlb_batting)[1] <- "Name"
names(mlb_pitching)[1] <- "Name"

names(fg_team_batting)[1] <- "Team"
names(fg_team_vs_lhp)[1] <- "Team"
names(fg_team_vs_rhp)[1] <- "Team"

fg_team_batting <- replace(fg_team_batting, fg_team_batting == 'SFG', 'SF')
fg_team_vs_lhp <- replace(fg_team_vs_lhp, fg_team_vs_lhp == 'SFG', 'SF')
fg_team_vs_rhp <- replace(fg_team_vs_rhp, fg_team_vs_rhp == 'SFG', 'SF')

fg_team_batting <- replace(fg_team_batting, fg_team_batting == 'WSN', 'WAS')
fg_team_vs_lhp <- replace(fg_team_vs_lhp, fg_team_vs_lhp == 'WSN', 'WAS')
fg_team_vs_rhp <- replace(fg_team_vs_rhp, fg_team_vs_rhp == 'WSN', 'WAS')

### Split Salary Data ###

mlb_salaries <- mlb_salaries %>%
  separate(Game.Info, c("Away", "String"), sep = "@") %>%
  separate(String, c("Home", "Date", "Time"), sep = " ")

mlb_salaries$Opponent <- if_else(mlb_salaries$Home == mlb_salaries$TeamAbbrev, mlb_salaries$Away, mlb_salaries$Home)

### SP vs Hitters ###
sp <- mlb_salaries %>% 
  filter(Position == "SP")

hitters <- mlb_salaries %>% 
  filter(Position != "SP")

### SP ###
sp <- sp %>% 
  left_join(mlb_pitching, by = c("Name")) %>% 
  left_join(fg_team_batting, by = c("Opponent" = "Team")) 
  #left_join(fg_team_vs_lhp, by = c("Opponent" = "Team")) %>% 
  #left_join(fg_team_vs_rhp, by = c("Opponent" = "Team"))

sp_slate <- sp %>% 
  select(Name, ID, Salary, TeamAbbrev, Opponent, BABIP.x, vFA..pi., ERA, xERA, FIP, xFIP, WAR.x, K., ISO, BABIP.y, wOBA, wRC., WAR.y) %>% 
  filter(Salary > 4000)
