library(nflfastR)
library(tidyverse)
library(ggrepel)

setwd("C://Users//Mike Francis//Documents")

###Defense EPA Table###

nfl_2021 <- load_pbp(2021)

###Def Pass EPA###

nfl_2021_def_pass_weekly <- nfl_2021 %>% 
  filter(pass == 1 &
           wp > .05 &
           wp < .95 &
           half_seconds_remaining > 120) %>% 
  group_by(defteam, week) %>% 
  summarize(def_pass_epa = round(mean(epa), digits = 3),
            n_plays = n()) %>% 
  arrange(def_pass_epa)

nfl_2021_def_pass_weekly$def_pass_epa_rank <- round(rank(nfl_2021_def_pass_weekly$def_pass_epa), digits = 0)

nfl_2021_def_pass_weekly$def_pass_epa_multiplier <- round(nfl_2021_def_pass_weekly$def_pass_epa / mean(nfl_2021_def_pass_weekly$def_pass_epa), digits = 2)
nfl_2021_def_pass_weekly$def_pass_epa_sd <- round((nfl_2021_def_pass_weekly$def_pass_epa - weighted.mean(nfl_2021_def_pass_weekly$def_pass_epa, nfl_2021_def_pass_weekly$n_plays, na.rm=T)) / sd(nfl_2021_def_pass_weekly$def_pass_epa, na.rm = T), digits = 2)

###Def Rush EPA###

nfl_2021_def_rush_weekly <- nfl_2021 %>% 
  filter(rush == 1 &
           wp > .05 &
           wp < .95 &
           half_seconds_remaining > 120) %>% 
  group_by(defteam, week) %>% 
  summarize(def_rush_epa = round(mean(epa), digits = 3),
            n_plays = n()) %>% 
  arrange(def_rush_epa)

nfl_2021_def_rush$def_rush_epa_rank <- round(rank(nfl_2021_def_rush$def_rush_epa), digits = 0)

nfl_2021_def_rush$def_rush_epa_multiplier <- round(nfl_2021_def_rush$def_rush_epa / mean(nfl_2021_def_rush$def_rush_epa), digits = 2)
nfl_2021_def_rush$def_rush_epa_sd <- round((nfl_2021_def_rush$def_rush_epa - weighted.mean(nfl_2021_def_rush$def_rush_epa, nfl_2021_def_rush$n_plays, na.rm=T)) / sd(nfl_2021_def_rush$def_rush_epa, na.rm = T), digits = 2)

nfl_2021_def <- nfl_2021_def_pass %>% 
  left_join(nfl_2021_def_rush, by = c('defteam')) 

nfl_2021_def$total_plays <- nfl_2021_def$n_plays.x + nfl_2021_def$n_plays.y

nfl_2021_def <- replace(nfl_2021_def, nfl_2021_def == 'LA', 'LAR')

###Game Log###
nfl_2021_qb <- nfl_2021 %>%
  group_by(passer, week, defteam, game_date) %>%
  summarize(
    mean_epa = round(mean(epa), digits = 3), 
    passing_yards = sum(passing_yards, na.rm = T), 
    rushing_yards = sum(rushing_yards, na.rm = T),
    tds = sum(touchdown),
    ints = sum(interception),
    fumbles_lost = sum(fumble_lost),
    plays = n()
  ) %>%
  arrange(-mean_epa) %>%
  filter(plays > 20) %>% 
  drop_na() %>% 
  view(title = "2021 QBs")

nfl_2021_qb$three_hundred_pass <- if_else(nfl_2021_qb$passing_yards >= 300,1,0)
nfl_2021_qb$one_hundred_rush <- if_else(nfl_2021_qb$rushing_yards >= 100,1,0)
nfl_2021_qb$fpts <- 
  (0.04 * nfl_2021_qb$passing_yards) + 
  (0.1 * nfl_2021_qb$rushing_yards) +
  (4 * nfl_2021_qb$tds) - 
  (1 * nfl_2021_qb$fumbles_lost) - 
  (1 * nfl_2021_qb$ints) + 
  (3 * nfl_2021_qb$three_hundred_pass) +
  (3 * nfl_2021_qb$one_hundred_rush)

nfl_2021_qb$passer_week <- paste(nfl_2021_qb$passer, nfl_2021_qb$week)

nfl_2021_qb <- nfl_2021_qb %>% 
  left_join(nfl_qb, by = c("passer_week" = "Name_week"))

nfl_2021_qb <- nfl_2021_qb %>% 
  left_join(nfl_qb, by = c("Name_week" = "passer_week"))

nfl_2021_qb %>% 
  select()