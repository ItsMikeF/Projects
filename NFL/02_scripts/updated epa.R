# load 

load_all <- function() {
  
  #load packages
  suppressMessages({
    library(nflfastR) # pbp data
    library(nflreadr) # nfl schedule and cleaning
    library(tidyverse) # ggplot2 dplyr tibble tidyr purrr forecats 
    library(glue) # interpreted literal strings
    library(caret) # data partition
    library(randomForest) # rf model
    library(googlesheets4) # google sheet
  })
  
  # load all contest files
  files <- function(start_year){
    
    # define year
    data_start <<- start_year
    nfl_year <<- year(Sys.Date())
    
    # define contests 
    contest_files <- list.files(path = "./01_data/contests/")
    contest_files
    
    # remove 2021 weeks bc they dont have all the needed files
    indices <- which(!grepl("2021", contest_files))
    contest_files[indices]
    contest_files <- contest_files[indices]
    
    # remove week 8 for missing files
    contest_files <- contest_files[-which(grepl("2022_w08", contest_files))]
    contest_files
    
    # remove week 1s
    contest_files <<- contest_files[-which(grepl("w01", contest_files))]
    
    # remove unncessary objects
    rm(indices)
    
  }
  files(2022)
  
  folder <<- "2025_w01"
  #folder <<- tail(contest_files, 1)
  
  # define year
  nfl_year <<- year(Sys.Date())
  
  # load schedule
  schedule <<- load_schedules(nfl_year)
  
  # set today as target date
  target_date <- Sys.Date()
  
  if (weekdays(target_date) %in% c("Tuesday", "Wednesday")) {
    target_row <- which.min(abs((as.Date(schedule$gameday) - target_date))) + 1
  } else {
    target_row <- which.min(abs((as.Date(schedule$gameday) - target_date)))
  }
  
  contest_week <<- as.numeric(schedule$week[target_row]) # hard code for playoffs
  
  # load spreads and totals
  odds <<- function(year){
    odds <<- load_schedules(year) %>% 
      select(season, week, away_team, home_team, spread_line, total_line) %>% 
      mutate(away_spread = spread_line, 
             home_spread = spread_line * -1, 
             
             #away_team = str_replace_all(away_team, "LA", "LAR"), 
             #away_team = str_replace_all(away_team, "LARC", "LAC"),
             
             #home_team = str_replace_all(home_team, "LA", "LAR"),
             #home_team = str_replace_all(home_team, "LARC", "LAC"),
             
             week = sprintf("%02d", week), 
             week = as.numeric(week),
             
             game_id = paste(season, week, away_team, home_team, sep = "_"), 
             away_join = paste(season, week, away_team, sep = "_"), 
             home_join = paste(season, week, home_team, sep = "_")
      ) %>% 
      select(-spread_line)
  }
  odds(nfl_year)
  
  #load injuries
  inj <<- load_injuries(data_start:nfl_year-1) %>% # add -1 for w01
    mutate(inj_join = paste(season, week, team, full_name, sep = "_"))
  
  # load pbp
  pbp <<- load_pbp(data_start:nfl_year) %>% # add -1 for w01
    mutate(weather = as.character(weather))
  
}
load_all()

# calc def epa
epa_def <- function(game_year){
  
  pbp_def_pass <- pbp %>%
    filter(season_type == "REG") %>%
    filter(season == game_year) %>%
    filter(qb_dropback == 1) %>%  # Includes passes, sacks, and scrambles
    group_by(defteam) %>%
    summarize(def_pass_epa = round(mean(epa), digits = 3),
              def_pass_sr = round(mean(success) * 100, digits = 1),  # Success rate as percentage
              n_plays = n()) %>%
    arrange(def_pass_epa) %>%
    mutate(def_pass_epa_rank = round(rank(def_pass_epa), digits = 0),
           def_pass_epa_sd = round((def_pass_epa - weighted.mean(def_pass_epa, n_plays, na.rm = T)) / sd(def_pass_epa, na.rm = T), digits = 2),
           def_pass_sr_rank = round(rank(-def_pass_sr), digits = 0))  # Lower SR is better for defense, so rank inversely
  
  # def rush epa
  pbp_def_rush <- pbp %>% 
    filter(season_type == "REG") %>% 
    filter(season == game_year) %>% 
    filter(rush == 1 & qb_dropback == 0) %>% 
    group_by(defteam) %>% 
    summarize(def_rush_epa = round(mean(epa), digits = 3),
              n_plays = n()) %>% 
    arrange(def_rush_epa) %>% 
    mutate(def_rush_epa_rank = round(rank(def_rush_epa), digits = 0), 
           def_rush_epa_sd = round((def_rush_epa - weighted.mean(def_rush_epa, n_plays, na.rm=T)) / sd(def_rush_epa, na.rm = T), digits = 2))
  
  # combine def rush and pass
  pbp_def <<- pbp_def_pass %>% 
    left_join(pbp_def_rush, by = c('defteam')) %>% 
    mutate(total_plays = n_plays.x + n_plays.y,
           #defteam = gsub('LA','LAR', defteam), 
           #defteam = gsub('LARC','LAC', defteam), 
           avg_rank = (def_rush_epa_rank + def_pass_epa_rank) /2) %>% 
    select(defteam, avg_rank,
           def_rush_epa, def_rush_epa_rank, def_rush_epa_sd, 
           def_pass_epa, def_pass_epa_rank, def_pass_epa_sd) %>% 
    arrange(avg_rank)
}
epa_def(2024)

pbp_def_pass_rush <- pbp %>%
  filter(season_type == "REG") %>%
  filter(season == 2024) %>%
  # Pass plays (dropbacks)
  filter(qb_dropback == 1) %>%
  group_by(defteam) %>%
  summarize(
    def_pass_epa = round(mean(epa, na.rm = TRUE), digits = 3),
    n_pass_plays = n(),
    pass_success_rate = round(mean(epa >= 0, na.rm = TRUE) * 100, digits = 1),
    .groups = 'drop'
  ) %>%
  # Rush plays (excluding QB scrambles)
  bind_rows(
    pbp %>%
      filter(season_type == "REG") %>%
      filter(season == 2024) %>%
      filter(rush == 1 & qb_dropback == 0) %>%
      group_by(defteam) %>%
      summarize(
        def_rush_epa = round(mean(epa, na.rm = TRUE), digits = 3),
        n_rush_plays = n(),
        rush_success_rate = round(mean(epa >= 0, na.rm = TRUE) * 100, digits = 1),
        .groups = 'drop'
      )
  ) %>%
  arrange(defteam) %>%
  mutate(
    def_pass_epa_rank = rank(-def_pass_epa, ties.method = "min"),
    def_rush_epa_rank = rank(-def_rush_epa, ties.method = "min")
  )

# View the result
print(pbp_def_pass_rush)

# calc off epa
epa_off <- function(game_year){
  
  # off pass epa
  pbp_off_pass <- pbp %>% 
    filter(season_type == "REG") %>% 
    filter(season == game_year) %>% 
    filter(pass == 1) %>% 
    group_by(posteam) %>% 
    summarize(off_pass_epa = round(mean(epa), digits = 3),
              n_plays = n()) %>% 
    arrange(off_pass_epa) %>% 
    mutate(off_pass_epa_rank = round(rank(-off_pass_epa), digits = 0), 
           off_pass_epa_sd = round((off_pass_epa - weighted.mean(off_pass_epa, n_plays, na.rm=T)) / sd(off_pass_epa, na.rm = T), digits = 2))
  
  # off rush epa
  pbp_off_rush <- pbp %>% 
    filter(season_type == "REG") %>% 
    filter(season == game_year) %>% 
    filter(rush == 1) %>% 
    group_by(posteam) %>% 
    summarize(off_rush_epa = round(mean(epa), digits = 3),
              n_plays = n()) %>% 
    arrange(off_rush_epa) %>% 
    mutate(off_rush_epa_rank = round(rank(-off_rush_epa), digits = 0), 
           off_rush_epa_sd = round((off_rush_epa - weighted.mean(off_rush_epa, n_plays, na.rm=T)) / sd(off_rush_epa, na.rm = T), digits = 2))
  
  pbp_off <<- pbp_off_pass %>% 
    left_join(pbp_off_rush, by = c('posteam')) %>% 
    mutate(total_plays = n_plays.x + n_plays.y, 
           #posteam = gsub('LA','LAR', posteam), 
           #posteam = gsub('LARC','LAC', posteam),
           avg_rank = (off_rush_epa_rank + off_pass_epa_rank) /2) %>% 
    select(posteam, avg_rank, 
           off_rush_epa, off_rush_epa_rank, off_rush_epa_sd, 
           off_pass_epa, off_pass_epa_rank, off_pass_epa_sd) %>% 
    arrange(avg_rank)
}
epa_off(2024)
