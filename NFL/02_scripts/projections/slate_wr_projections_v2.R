# create qb fpts model v2

# 1.0 load packages and data--------------------------------------------------

load_all <- function() {
  #load packages
  suppressMessages({
    library(nflfastR) # pbp data
    library(nflreadr) # nfl schedule and cleaning
    library(tidyverse) # ggplot2 dplyr tibble tidyr purrr forecats 
    library(glue) # interpreted literal strings
    library(caret) # data partition
    library(randomForest) # rf model
    library(openxlsx) # write xlsx files
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
  
  folder <<- tail(contest_files, 1)
  
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
  
  contest_week <<- as.numeric(schedule$week[target_row])
  
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
  inj <<- load_injuries(2022:nfl_year) %>% 
    mutate(inj_join = paste(season, week, team, full_name, sep = "_"))
  
  # load pbp
  pbp <<- load_pbp(data_start:nfl_year) %>% 
    mutate(weather = as.character(weather))
  
}
load_all()

# calc wr fpts by game week
wr_fpts_pbp <- function(){
  
  # Load regular season data
  wr_pbp <- pbp %>% 
    group_by(game_id, receiver, receiver_id) %>% 
    summarize(
      
      receptions = sum(complete_pass, na.rm = T),
      receiving_yards = sum(receiving_yards, na.rm = T),
      targets = sum(pass_attempt, na.rm = T),
      pass_touchdown = sum(pass_touchdown, na.rm = T),
      fumble_lost = sum(fumble_lost, na.rm = T), 
      
      posteam = last(posteam), 
      week = last(week), 
      season = last(season),
      
      .groups = "drop"
      
    ) %>% 
    drop_na() %>% 
    mutate(join = paste(season, week, receiver_id, sep = "_"))
  
  # Get receiver rushing stats
  rusher_pbp <- pbp %>% 
    group_by(game_id, rusher, rusher_id) %>% 
    summarise(
      
      rush_attempt = sum(rush_attempt, na.rm = T),
      rushing_yards = sum(rushing_yards, na.rm = T),
      rush_touchdown = sum(rush_touchdown, na.rm = T),
      fumble = sum(fumble, na.rm = T), 
      
      posteam = last(posteam), 
      week = last(week), 
      season = last(season),
      
      .groups = "drop"
      
    ) %>% 
    drop_na() %>% 
    mutate(join = paste(season, week, rusher_id, sep = "_")) %>% 
    select(join, rush_attempt, rushing_yards, rush_touchdown, fumble)
  
  # join stats and calc fpts, dk scoring
  wr_fpts <<- wr_pbp %>% 
    left_join(rusher_pbp, by=c("join")) %>% 
    replace(is.na(.),0) %>% 
    mutate(
      big_rush = ifelse(rushing_yards > 100, 1,0), 
      big_rec = ifelse(receiving_yards > 100, 1,0), 
      fpts = 
        
        big_rush * 3 +
        big_rec * 3 +
        
        pass_touchdown * 6 +
        receiving_yards * .1 +
        receptions * 1 +
        
        rushing_yards * .1 +
        rush_touchdown * 6 +
        fumble * -1, 
      
      fpts_ntile = ntile(fpts, 100)
    ) %>% 
    arrange(-fpts) %>% 
    mutate(join = paste(season, week, posteam, receiver, sep = "_")) %>% 
    relocate(c("fpts", "fpts_ntile"), .after = receiver_id) %>% 
    relocate(c("season","week","join"), .after = big_rec)
  
}
wr_fpts_pbp()

# calc def epa
epa_def <- function(game_week, game_year){
  
  # def pass epa
  pbp_def_pass <- pbp %>% 
    filter(season == game_year) %>% 
    filter(pass == 1 &
             week < game_week) %>% 
    group_by(defteam) %>% 
    summarize(def_pass_epa = round(mean(epa), digits = 3),
              n_plays = n()) %>% 
    arrange(def_pass_epa) %>% 
    mutate(def_pass_epa_rank = round(rank(def_pass_epa), digits = 0), 
           def_pass_epa_sd = round((def_pass_epa - weighted.mean(def_pass_epa, n_plays, na.rm=T)) / sd(def_pass_epa, na.rm = T), digits = 2))
  
  # def rush epa
  pbp_def_rush <- pbp %>% 
    filter(season == game_year) %>% 
    filter(rush == 1 &
             week < game_week) %>% 
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
epa_def(contest_week, nfl_year)

# calc off epa
epa_off <- function(game_week, game_year){
  
  # off pass epa
  pbp_off_pass <- pbp %>% 
    filter(season == game_year) %>% 
    filter(pass == 1 &
             week < game_week) %>% 
    group_by(posteam) %>% 
    summarize(off_pass_epa = round(mean(epa), digits = 3),
              n_plays = n()) %>% 
    arrange(off_pass_epa) %>% 
    mutate(off_pass_epa_rank = round(rank(-off_pass_epa), digits = 0), 
           off_pass_epa_sd = round((off_pass_epa - weighted.mean(off_pass_epa, n_plays, na.rm=T)) / sd(off_pass_epa, na.rm = T), digits = 2))
  
  # off rush epa
  pbp_off_rush <- pbp %>% 
    filter(season == game_year) %>% 
    filter(rush == 1 &
             week < game_week) %>% 
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
epa_off(contest_week, nfl_year)

# define pff def table
def_table <- function() {
  def <- read.csv(glue("./01_data/contests/{folder}/pff/defense_summary.csv"))
  
  def_table <- def %>%
    group_by(team_name) %>%
    summarise(def = round(weighted.mean(grades_defense, snap_counts_defense), digits = 1),
              rdef = round(weighted.mean(grades_run_defense, snap_counts_run_defense), digits = 1),
              tack = round(weighted.mean(grades_tackle, snap_counts_defense, na.rm = TRUE), digits = 1),
              prsh = round(weighted.mean(grades_pass_rush_defense, snap_counts_pass_rush), digits = 1),
              cov = round(weighted.mean(grades_coverage_defense, snap_counts_coverage), digits =1))
  
  def_table <<- def_table %>% 
    mutate(def_rank = round(rank(-def), digits = 0), 
           def_sd = round((def - mean(def)) / sd(def, na.rm = T), digits = 2),
           
           rdef_rank = round(rank(-def_table$rdef), digits = 0), 
           rdef_sd = round((rdef - mean(rdef)) / sd(rdef, na.rm = T), digits = 2),
           
           tack_rank = round(rank(-def_table$tack), digits = 0), 
           tack_sd = round((tack - mean(tack)) / sd(tack, na.rm = T), digits = 2),
           
           prsh_rank = round(rank(-def_table$prsh), digits = 0), 
           prsh_sd = round((prsh - mean(prsh)) / sd(prsh, na.rm = T), digits = 2),
           
           cov_rank = round(rank(-def_table$cov), digits = 0),
           cov_sd = round((cov - mean(cov)) / sd(cov, na.rm = T), digits = 2),
           
           team_name = gsub('ARZ','ARI', team_name), 
           team_name = gsub('BLT','BAL', team_name), 
           team_name = gsub('CLV','CLE', team_name), 
           team_name = gsub('HST','HOU', team_name), 
           #team_name = gsub('JAX','JAC', team_name), 
           team_name = gsub('LA','LAR', team_name), 
           team_name = gsub('LARC','LAC', team_name))
}
def_table()

# load and processdepth charts
nfl_depth <- function() {
  # load depth chart
  depth_charts <<- load_depth_charts(seasons = nfl_year) %>% 
    # current week depth charts not always available
    #filter(week == contest_week-1) %>% 
    #mutate(week = week +1) %>% 
    
    # current week depth charts n
    filter(week == contest_week) %>% 
    distinct(full_name, .keep_all = T) %>% 
    
    filter(position == "WR" | position == "TE") %>% 
    select(1:5, 10, 12, 15) %>% 
    mutate(full_name = clean_player_names(full_name, lowercase = T), 
           game_id = paste(season, week, club_code, sep = "_")) %>% 
    
    left_join(odds %>% select(away_team, home_team, home_spread, away_spread, home_join, total_line), by = c("game_id" = "home_join")) %>% 
    left_join(odds %>% select(away_team, home_team, home_spread, away_spread, away_join, total_line), by = c("game_id" = "away_join")) %>% 
    
    mutate(away_team = coalesce(away_team.x, away_team.y),
           home_team = coalesce(home_team.x, home_team.y),
           
           home_spread = coalesce(home_spread.x, home_spread.y),
           away_spread = coalesce(away_spread.x, away_spread.y), 
           
           total_line = coalesce(total_line.x, total_line.y)) %>% 
    
    select(-c(away_team.x, away_team.y, home_team.x, home_team.y, home_spread.x, home_spread.y, away_spread.x, away_spread.y)) %>% 
    
    mutate(opp = if_else(club_code == home_team, away_team, home_team), 
           spread = if_else(club_code == home_team, home_spread, away_spread),
           home = if_else(club_code == home_team, 1,0), 
           game_id = paste(season, week, away_team, home_team, sep = "_")) %>% 
    
    select(-c(away_team, home_team, away_spread, home_spread, total_line.x, total_line.y)) %>% 
    #drop_na() %>% # to remove bye week teams
    
    # add schedule
    left_join(schedule %>% select(game_id, gameday, weekday, gametime), by=c("game_id"))
  
  unique(depth_charts$club_code)
  
  teams_colors_logos$team_abbr <- teams_colors_logos[!teams_colors_logos %in% c("OAK", "SD", "STL")]
  teams_colors_logos[!teams_colors_logos %in% unique(depth_charts$club_code)]
}
nfl_depth()

# load and process pff data
pff_rec <- function() {
  
  # load recieving summary
  receiving_summary <<- read.csv(glue("./01_data/contests/{folder}/pff/receiving_summary.csv")) %>% 
    mutate(player = clean_player_names(player, lowercase = T), 
           targets_game = round(targets / player_game_count, digits = 1),
           rec_game = round(receptions / player_game_count, digits = 2),
           rec_yards_game = round(yards / player_game_count, digits = 2),
           rec_td_game = round(touchdowns / player_game_count,digits = 2)) %>% 
    select(-c("receptions")) # must remove for the join

  # load scheme
  receiving_scheme <<- read.csv(glue("./01_data/contests/{folder}/pff/receiving_scheme.csv")) %>% 
    mutate(player = clean_player_names(player, lowercase = T)) %>% 
    select(player, man_yprr, zone_yprr) %>% 
    mutate(man_adv = man_yprr - zone_yprr)
  
  defense_coverage_scheme <<- read.csv(glue("./01_data/contests/{folder}/pff/defense_coverage_scheme.csv")) %>% 
    mutate(player = clean_player_names(player, lowercase = T), 
           team_name = clean_team_abbrs(team_name)) %>% 
    select(player,
           team_name,
           man_snap_counts_coverage, 
           man_snap_counts_coverage_percent,
           man_grades_coverage_defense,
           zone_snap_counts_coverage,
           zone_snap_counts_coverage_percent,
           zone_grades_coverage_defense) %>% 
    group_by(team_name) %>% 
    
    summarize(man_snaps = sum(man_snap_counts_coverage), 
              zone_snaps = sum(zone_snap_counts_coverage),
              def_man_grade = weighted.mean(man_grades_coverage_defense, man_snap_counts_coverage), 
              def_zone_grade = weighted.mean(zone_grades_coverage_defense, zone_snap_counts_coverage)) %>% 
    
    mutate(man_percentage = round(man_snaps / (man_snaps + zone_snaps), digits = 3), 
           man_rank =  round(rank(-man_percentage), digits = 0), 
           def_man_grade = round(def_man_grade, digits = 1), 
           def_man_grade_rank = round(rank(-def_man_grade), digits = 0), 
           zone_percentage = 1 - man_percentage, 
           zone_rank = round(rank(-zone_percentage), digits = 0), 
           def_zone_grade = round(def_zone_grade, digits = 1), 
           def_zone_grade_rank = round(rank(-def_zone_grade), digits = 0))
  
}
pff_rec()

# combine all wr data
combine_wr <- function(){
  
  wr <<- depth_charts %>%
    
    left_join(receiving_summary, by = c('full_name' = 'player')) %>% 
    left_join(receiving_scheme,by = c('full_name' = 'player')) %>% 
    
    left_join(pbp_def, by = c('opp' = 'defteam')) %>% 
    left_join(pbp_off, by = c('club_code' = 'posteam')) %>%
    
    left_join(defense_coverage_scheme, by = c('opp' = 'team_name')) %>% 
    left_join(def_table, by = c('opp' = 'team_name')) %>% 
    
    mutate(full_name = str_to_title(full_name)) %>% 
    mutate(z_score = off_pass_epa_sd - def_pass_epa_sd) %>% 
    
    left_join(schedule %>% select(game_id, roof), by=c("game_id")) %>% # add field data from schedule
    mutate(#temp = if_else(roof == "closed" | roof == "dome", 70, temp), 
           #wind = if_else(is.na(wind), 0, wind), 
           #weather_check = if_else(temp == 0 & wind == 0, 0, 1), 
           dome = if_else(roof == "dome" | roof == "closed",1,0))
  
}
combine_wr()

# 2.1.0 load and deploy rf fpts model -----------------------------------------


# load rf models
load("./04_models/wr/wr_fpts_rf.Rdata")
load("./04_models/wr/wr_receptions_rf.Rdata")
load("./04_models/wr/wr_receiving_yards_rf.Rdata")
load("./04_models/wr/wr_pass_touchdown_rf.Rdata")

# make projections with rf models
model_projections_fpts <- predict(wr_fpts_rf, newdata = wr)
model_projections_receptions <- predict(wr_receptions_rf, newdata = wr)
model_projections_receiving_yards <- predict(wr_receiving_yards_rf, newdata = wr)
model_projections_pass_td <- predict(wr_pass_touchdown_rf, newdata = wr)

# join projections to data
wr <- wr %>% 
  
  mutate(fpt_proj = round(model_projections_fpts, digits = 1), 
         receptions_proj = round(model_projections_receptions, digits = 1), 
         receiving_yards_proj = round(model_projections_receiving_yards, digits = 1), 
         pass_td_proj = round(model_projections_pass_td, digits = 1)) %>% 
  
  relocate(c("fpt_proj", "receptions_proj", "receiving_yards_proj", "pass_td_proj"),
           .after = full_name) %>% 
  arrange(-fpt_proj)

# join projections to data and view
wr_slate <- wr %>% 
  #filter(weekday == "Monday") %>% # toggle as needed for slates
  mutate(position = paste0("wr",row_number())) %>% 
  view(title = glue("{folder}_wr"))



# 2.1 revised -------------------------------------------------------------

# Helper function to make predictions and round them
predict_and_round <- function(model, data, digits = 2) {
  round(predict(model, newdata = data), digits)
}

# Load models
models <- list(
  fpts = "./04_models/wr/wr_fpts_rf.Rdata",
  receptions = "./04_models/wr/wr_receptions_rf.Rdata",
  receiving_yards = "./04_models/wr/wr_receiving_yards_rf.Rdata",
  pass_td = "./04_models/wr/wr_pass_touchdown_rf.Rdata"
)

lapply(models, load)

# Add projections to te data
te <- wr %>%
  filter(position.x == "TE") %>% 
  mutate(
    fpt_proj = predict_and_round(wr_fpts_rf, .),
    receptions_proj = predict_and_round(wr_receptions_rf, .),
    receiving_yards_proj = predict_and_round(wr_receiving_yards_rf, .),
    pass_td_proj = predict_and_round(wr_pass_touchdown_rf, .)
  ) %>%
  relocate(c(fpt_proj, receptions_proj, receiving_yards_proj, pass_td_proj), .after = full_name) %>%
  arrange(desc(fpt_proj))

# Prepare slate and view
te_slate <- te %>%
  # Uncomment the next line if filtering by weekday
  # filter(weekday == "Monday") %>%
  select(7:50) %>% 
  mutate(position.x = paste0("TE", row_number())) %>%
  view(title = glue("{folder}_te"))

# Add projections to WR data
wr <- wr %>%
  filter(position.x == "WR") %>% 
  mutate(
    fpt_proj = predict_and_round(wr_fpts_rf, .),
    receptions_proj = predict_and_round(wr_receptions_rf, .),
    receiving_yards_proj = predict_and_round(wr_receiving_yards_rf, .),
    pass_td_proj = predict_and_round(wr_pass_touchdown_rf, .)
  ) %>%
  relocate(c(fpt_proj, receptions_proj, receiving_yards_proj, pass_td_proj), .after = full_name) %>%
  arrange(desc(fpt_proj))

# Prepare slate and view
wr_slate <- wr %>%
  # Uncomment the next line if filtering by weekday
  # filter(weekday == "Monday") %>%
  select(7:50) %>% 
  mutate(position.x = paste0("WR", row_number())) %>%
  view(title = glue("{folder}_wr"))


# 3.0 save and upload projections -----------------------------------------

# save to local csv
write.csv(wr_slate, file = glue("./05_outputs/proj/{folder}_wr.csv"))

# authorize google sheets
gs4_auth(email = "michael.john.francis2015@gmail.com")

# sheet id
sheet_id <- "https://docs.google.com/spreadsheets/d/1wo7QLvS5nbj6v3GVOlNs3Htw1VXhW9TEHCWkD-LHPSQ/edit?gid=0#gid=0"

# overwrite an entire sheet
sheet_write(wr_slate, ss = sheet_id, sheet = "wr")

# overwrite an entire sheet
sheet_write(te_slate, ss = sheet_id, sheet = "te")
