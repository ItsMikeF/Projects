#lets play some cfb dfs

#load packages
suppressMessages({
  library(tidyverse) #ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, forcats
  library(glue) #interpreted string literals
  library(rvest) #easily harvest (scrape) web pages
  library(httr) #tools for working with URLs and HTTP
  library(cfbfastR) #access cfb pbp data
  library(ggimage) #use image in ggplot2
})

#cfbfastr teams
cfb_teams <- cfbd_team_info()
cfb_teams_merge <- cfb_teams %>% 
  select(school, mascot, abbreviation, alt_name2) %>% 
  mutate(school_mascot = paste(school, mascot))

#read csv
week <- 2
dks <- read.csv(glue("./contests/2022_w{week}/DKSalaries.csv"))

#merge with team abbrev
dks_merge <- dks %>% 
  left_join(cfb_teams_merge, by=c("TeamAbbrev"="abbreviation"))

#load cfb schools
cfb_schools <- read.csv("./data/cfb_schools.csv")

dks_merge <- dks %>% 
  left_join(cfb_schools, by=c("TeamAbbrev"="Abbrev"))

#add dk odds
dks_merge <- dks_merge %>% 
  left_join(dk_odds, by=c("draftkings"="teams"))

#slate eda
teams <- unique(dks$TeamAbbrev)
games <- length(teams)/2
print(glue("{length(teams)/2} game slate"))

#qbs
qbs <- dks_merge %>% filter(Position=="QB") %>% 
  left_join(read.csv(glue("./contests/2022_w{week}/passing_summary.csv")), 
                          by=c("Name"="player"))

qbs_select <- qbs %>% 
  filter(Salary > min(Salary)) %>% 
  select(Name, ID, Salary, TeamAbbrev, lines, totals, grades_pass, btt_rate, twp_rate, avg_depth_of_target, 
         avg_time_to_throw, grades_run, pressure_to_sack_rate, player_game_count, attempts, yards, ypa) %>% 
  mutate(ttt_run_grade = round(avg_time_to_throw*grades_run, digits = 1)) %>% 
  view(title = "QBs")

#rbs
rbs <- dks_merge %>% filter(Position=="RB") %>% 
  left_join(read.csv(glue("./contests/2022_w{week}/rushing_summary.csv")), 
            by=c("Name"="player"))

rbs_select <- rbs %>% 
  filter(Salary > min(Salary)) %>% 
  select(Name, ID, Salary, TeamAbbrev, lines, totals, grades_offense, grades_run, designed_yards, elusive_rating, breakaway_attempts, explosive, elu_yco, first_downs, attempts,
         designed_yards, rec_yards, targets, total_touches, player_game_count) %>% 
  mutate(#mtf_per_att = round(elu_rush_mtf/attempts, digits = 2),
         breakaway_attempts = round(breakaway_attempts/attempts),
         explosive = round(explosive/attempts, digits = 1),
         elu_yco = round(elu_yco/attempts, digits = 1),
         first_downs = round(first_downs/attempts,  digits = 1),
         attempts = round(attempts/player_game_count,  digits = 1),
         designed_yards = round(designed_yards/player_game_count, digits = 1),
         rec_yards = round(rec_yards/player_game_count, digits = 1),
         targets = round(targets/player_game_count, digits = 1),
         total_touches = round(total_touches/player_game_count, digits = 1)) %>% 
  view(title = "RBs")

#wrs
wrs <- dks_merge %>% filter(Position=="WR") %>% 
  left_join(read.csv(glue("./contests/2022_w{week}/receiving_summary.csv")), 
            by=c("Name"="player"))
wrs %>% 
  select(Name, team_name, lines, totals, Salary, pass_plays, grades_offense, yprr) %>% 
  arrange(-yprr) %>% 
  view(title = "WRs")
