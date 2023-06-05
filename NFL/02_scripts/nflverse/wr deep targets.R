#lets look at wr data

# Load packages
suppressMessages({
  library(nflreadr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(nflfastR)
  library(nflplotR)
  library(lubridate)
})

pbp 

wr <- read.csv("./01_data/training_data/position_groups/wrs.csv")

wr <- wr %>% filter(week == 17)
names(wr)

wr %>% select(player, team_name, receptions, targets, year) %>% arrange(-targets)

wr %>% 
  select(player, team_name, targets, yprr, deep_targets, year) %>% 
  filter(year > 2021) %>%
  arrange(-deep_targets)
  group_by(player) %>% 
  summarise(deep_targets = sum(deep_targets)) %>% 
  

# Get receiving stats
wr_pbp <- pbp %>% 
  group_by(receiver, receiver_id) %>% 
  summarise(
    fumble = sum(fumble, na.rm = T), 
    receptions = sum(complete_pass, na.rm = T),
    receiving_yards = sum(receiving_yards, na.rm = T), 
    rec_touchdown = sum(pass_touchdown, na.rm = T)) %>% 
  drop_na() %>% 
  ungroup() %>% 
  mutate(
    fpts = 
      fumble * -1 +
      receptions * 0.5 +
      rec_touchdown * 6 +
      receiving_yards * .1, 
    fpts_ntile = ntile(fpts, 100))
    