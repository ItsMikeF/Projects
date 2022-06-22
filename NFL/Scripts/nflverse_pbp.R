#load packages
library(tidyverse) #metapackage
library(nflverse) #functions to efficiently access NFL pbp data

#load play by play data
pbp <- load_pbp(seasons = 2014:2021)
pbp$year <- as.numeric(substr(pbp$game_date, 1, 4))

names <- tibble(names(pbp))

#qb historical stats and fpts
qb_pbp <- pbp %>% 
  group_by(passer ,posteam, game_date, defteam, week) %>% 
  summarize(
    pass_attempt = sum(pass_attempt, na.rm = T),
    passing_yards = sum(passing_yards, na.rm = T),
    pass_touchdown = sum(pass_touchdown, na.rm = T),
    interception = sum(interception, na.rm = T),
    
    rushing_yards = sum(rushing_yards, na.rm = T),
    rush_attempt = sum(rush_attempt, na.rm = T),
    rush_touchdown = sum(rush_touchdown, na.rm = T),
    fumble_lost = sum(fumble_lost, na.rm = T),
    
    epa = round(mean(qb_epa), digits = 2),
    cpoe = round(mean(cpoe, na.rm = T), digits = 2)) %>% 
  filter(pass_attempt > 5) %>% 
  mutate("300py" = ifelse(passing_yards > 300, 1,0), 
         fpts = 
           pass_touchdown * 4 +
           passing_yards * .04 +
           interception * -1 +
           rushing_yards * .1 +
           rush_touchdown * 6 +
           fumble_lost * -1 +
           "300py" * 3, 
         year = as.numeric(substr(game_date, 1, 4)), 
         week_minus1 = week - 1, 
         merge = paste0(unlist(strsplit(passer, "[.]"))[2], year, week_minus1))

qbs <- read.csv("./Training_Data/position_groups/qbs.csv") %>%
  mutate(name = player) %>% 
  separate(name, into = c("first_name", "last_name"), sep=" ") %>% 
  mutate(merge = paste0(last_name, year, week)) %>% 
  left_join(qb_pbp, by = c("merge"))
