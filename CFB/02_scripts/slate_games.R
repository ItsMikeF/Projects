# display upcoming games

library(tidyverse)
library(lubridate)
library(cfbfastR)

contest_week = 3
cfb_season = 2025

keep.conf <- c("SEC", "Moutain West", "American Athletic", "Pac-12", 
               "Mid-American", "Sun Belt", "ACC", "Big 12", "Big Ten", 
               "FBS Independents", "Conference USA")

scores <- load_cfb_schedules(2025) %>% 
  filter(week < contest_week) %>% 
  filter(home_division == "fbs") %>% 
  filter(away_division == "fbs") %>% 
  select(game_id, 
         home_id, home_team, home_pregame_elo, home_points,
         away_id, away_team, away_pregame_elo, away_points)

rosters_qb <- load_cfb_rosters(2025) %>% 
  select(2:6,8,9) %>% 
  mutate(player = paste(first_name, last_name)) %>% 
  select(-c(first_name, last_name)) %>% 
  filter(position == "QB") %>% 
  relocate(player, .after = team)

pbp <- load_cfb_pbp(2025)

pbp_clean <- pbp %>% 
  filter(offense_conference %in% keep.conf | defense_conference %in% keep.conf)

# calc cfb team def epa
epa_def <- function(game_week, game_year){
    
    # def pass epa
    pbp_def_pass <- pbp_clean %>% 
      filter(season == game_year) %>% 
      filter(pass == 1 &
               week < game_week) %>% 
      group_by(def_pos_team) %>% 
      summarize(def_pass_epa = round(mean(EPA), digits = 3),
                n_plays = n()) %>% 
      arrange(def_pass_epa) %>% 
      mutate(def_pass_epa_rank = round(rank(def_pass_epa), digits = 0), 
             def_pass_epa_sd = round((def_pass_epa - weighted.mean(def_pass_epa, n_plays, na.rm=T)) / sd(def_pass_epa, na.rm = T), digits = 2))
    
    # def rush epa
    pbp_def_rush <- pbp_clean %>% 
      filter(season == game_year) %>% 
      filter(rush == 1 &
               week < game_week) %>% 
      group_by(def_pos_team) %>% 
      summarize(def_rush_epa = round(mean(EPA), digits = 3),
                n_plays = n()) %>% 
      arrange(def_rush_epa) %>% 
      mutate(def_rush_epa_rank = round(rank(def_rush_epa), digits = 0), 
             def_rush_epa_sd = round((def_rush_epa - weighted.mean(def_rush_epa, n_plays, na.rm=T)) / sd(def_rush_epa, na.rm = T), digits = 2))
    
    # combine def rush and pass
    epa_def <<- pbp_def_pass %>% 
      left_join(pbp_def_rush, by = c('def_pos_team')) %>% 
      mutate(total_plays = n_plays.x + n_plays.y,
             avg_rank = (def_rush_epa_rank + def_pass_epa_rank) /2) %>% 
      select(def_pos_team, avg_rank,
             def_rush_epa, def_rush_epa_rank, def_rush_epa_sd, 
             def_pass_epa, def_pass_epa_rank, def_pass_epa_sd) %>% 
      arrange(avg_rank)
  }
epa_def(contest_week, cfb_season)
  
# calc cfb team off epa
epa_off <- function(game_week, game_year){
    
    # off pass epa
    pbp_off_pass <- pbp_clean %>% 
      filter(season == game_year) %>% 
      filter(pass == 1 &
               week < game_week) %>% 
      group_by(pos_team) %>% 
      summarize(off_pass_epa = round(mean(EPA), digits = 3),
                n_plays = n()) %>% 
      arrange(off_pass_epa) %>% 
      mutate(off_pass_epa_rank = round(rank(-off_pass_epa), digits = 0), 
             off_pass_epa_sd = round((off_pass_epa - weighted.mean(off_pass_epa, n_plays, na.rm=T)) / sd(off_pass_epa, na.rm = T), digits = 2))
    
    # off rush epa
    pbp_off_rush <- pbp_clean %>% 
      filter(season == game_year) %>% 
      filter(rush == 1 &
               week < game_week) %>% 
      group_by(pos_team) %>% 
      summarize(off_rush_epa = round(mean(EPA), digits = 3),
                n_plays = n()) %>% 
      arrange(off_rush_epa) %>% 
      mutate(off_rush_epa_rank = round(rank(-off_rush_epa), digits = 0), 
             off_rush_epa_sd = round((off_rush_epa - weighted.mean(off_rush_epa, n_plays, na.rm=T)) / sd(off_rush_epa, na.rm = T), digits = 2))
    
    epa_off <<- pbp_off_pass %>% 
      left_join(pbp_off_rush, by = c('pos_team')) %>% 
      mutate(total_plays = n_plays.x + n_plays.y, 
             avg_rank = (off_rush_epa_rank + off_pass_epa_rank) /2) %>% 
      select(pos_team, avg_rank, 
             off_rush_epa, off_rush_epa_rank, off_rush_epa_sd, 
             off_pass_epa, off_pass_epa_rank, off_pass_epa_sd) %>% 
      arrange(avg_rank)
  }
epa_off(contest_week, cfb_season)

# def slate games
slate <- load_cfb_schedules(2025) %>% 
  filter(week == contest_week) %>% 
  filter(home_division == "fbs") %>% 
  filter(away_division == "fbs") %>% 
  select(game_id, start_date, 
         home_team, away_team) %>%   #home_id, home_pregame_elo, #away_id, away_pregame_elo
   mutate(
    date_time = ymd_hms(start_date, tz = "America/New_York"), # Parse to date-time
    date = as_date(date_time),                  # Extract date from parsed date-time
    time = format(date_time, "%H:%M:%S")        # Extract time from parsed date-time
  ) %>% 
  select(-c("start_date", "date_time")) %>% 
  relocate(c("date", "time"), .after = game_id) %>% 
  left_join(epa_off, by = c("home_team"="pos_team"))

# Load required packages
library(dplyr)

# Join slate and odds data frames
merged_data <- slate %>%
  left_join(
    odds,
    by = c("home_team" = "home_team", "away_team" = "away_team")
  ) %>%
  # Select and reorder columns for clarity (optional)
  select(
    game_id.x, date, time, home_team, away_team,
    home_spread, away_spread, game_total,
    avg_rank, off_rush_epa, off_rush_epa_rank, off_rush_epa_sd,
    off_pass_epa, off_pass_epa_rank, off_pass_epa_sd,
    target_date
  ) %>%
  # Rename game_id.x to game_id for consistency
  rename(game_id = game_id.x)

# Print structure of merged data
str(merged_data)

# Optional: Check for unmatched games (rows where odds data is missing)
unmatched <- merged_data %>%
  filter(is.na(home_spread) | is.na(away_spread) | is.na(game_total))
if (nrow(unmatched) > 0) {
  cat("Number of games in slate without odds data:", nrow(unmatched), "\n")
  print(unmatched[, c("game_id", "home_team", "away_team", "date")])
}

library(googlesheets4)
gs4_auth()
sheet_id <- "https://docs.google.com/spreadsheets/d/1zTmxMOxGUuY5bYIiEA9lgbagUgmwXmYB3e7EVoF1LeA/edit?gid=0#gid=0"
write_sheet(merged_data, ss = sheet_id, sheet = "slate")
