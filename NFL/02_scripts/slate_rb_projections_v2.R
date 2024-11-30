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
    library(xgboost) # xgb model
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
  target_row <- which.min(abs((as.Date(schedule$gameday)-target_date)))
  
  contest_week <<- as.numeric(schedule$week[target_row])
  
  # load spreads and totals
  odds <<- function(year){
    odds <<- load_schedules(year) %>% 
      select(season, week, away_team, home_team, spread_line, total_line) %>% 
      mutate(away_spread = spread_line, 
             home_spread = spread_line * -1, 
             away_team = str_replace_all(away_team, "LA", "LAR"), 
             away_team = str_replace_all(away_team, "LARC", "LAC"),
             home_team = str_replace_all(home_team, "LA", "LAR"),
             home_team = str_replace_all(home_team, "LARC", "LAC"),
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
  pbp <<- load_pbp(nfl_year)
  
}
load_all()

# calc def epa
epa_def <- function(game_week){
  
  # def pass epa
  pbp_def_pass <- pbp %>% 
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
           defteam = gsub('LA','LAR', defteam), 
           defteam = gsub('LARC','LAC', defteam), 
           avg_rank = (def_rush_epa_rank + def_pass_epa_rank) /2) %>% 
    select(defteam, 
           def_rush_epa, def_rush_epa_rank, def_rush_epa_sd, 
           def_pass_epa, def_pass_epa_rank, def_pass_epa_sd)
}
epa_def(contest_week)

# calc off epa
epa_off <- function(game_week){
  
  # off pass epa
  pbp_off_pass <- pbp %>% 
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
           posteam = gsub('LA','LAR', posteam), 
           posteam = gsub('LARC','LAC', posteam),
           avg_rank = (off_rush_epa_rank + off_pass_epa_rank) /2) %>% 
    select(posteam, 
           off_rush_epa, off_rush_epa_rank, off_rush_epa_sd, 
           off_pass_epa, off_pass_epa_rank, off_pass_epa_sd)
}
epa_off(contest_week)

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
    filter(week == contest_week) %>%
    filter(position == "RB") %>% 
    filter(depth_position == "RB") %>% 
    select(1:5, 10, 12, 15) %>% 
    mutate(full_name = clean_player_names(full_name, lowercase = T),
           game_id = paste(season, week, club_code, sep = "_")) %>% 
    
    left_join(odds %>% select(away_team, home_team, home_spread, away_spread, home_join, total_line), by = c("game_id" = "home_join")) %>% 
    left_join(odds %>% select(away_team, home_team, home_spread, away_spread, away_join, total_line), by = c("game_id" = "away_join")) %>% 
    
    mutate(away_team = coalesce(away_team.x, away_team.y),
           home_team = coalesce(home_team.x, home_team.y),
           
           home_spread = coalesce(home_spread.x, home_spread.y),
           away_spread = coalesce(away_spread.x, away_spread.y), 
           
           total_line =coalesce(total_line.x, total_line.y)) %>% 
    
    select(-c(away_team.x, away_team.y, home_team.x, home_team.y, home_spread.x, home_spread.y, away_spread.x, away_spread.y)) %>% 
    mutate(opp = if_else(club_code == home_team, away_team, home_team), 
           spread = if_else(club_code == home_team, home_spread, away_spread),
           home = if_else(club_code == home_team, 1,0)) %>% 
    select(-c(away_team, home_team, away_spread, home_spread, total_line.x, total_line.y)) %>% 
    drop_na() # to remove bye week teams
}
nfl_depth()

# load and process pff rushing summary
pff_rush <- function() {
  # load pff rushing data
  rushing_summary <- read.csv(glue("./01_data/contests/{folder}/pff/rushing_summary.csv")) %>% 
    select(player, player_id, player_game_count, team_name, 
           total_touches, elu_rush_mtf, 
           attempts, yco_attempt, ypa, first_downs, grades_run,
           targets, yprr) %>% 
    mutate(player = clean_player_names(player, lowercase = T))
  
  rushing_summary_share <- rushing_summary %>% 
    group_by(team_name) %>% 
    summarize(team_attempts = sum(attempts))
  
  rushing_summary <<- rushing_summary %>% 
    left_join(rushing_summary_share, by=c("team_name")) %>% 
    mutate(rush_share = round(attempts / team_attempts, digits = 2)) %>% 
    relocate(c("rush_share","team_attempts"), .after = attempts)
}
pff_rush()
  
# combine all rb data
combine_rb <- function() {
  rb <<- depth_charts %>%
    left_join(rushing_summary, by = c('full_name' = 'player')) %>% 
    mutate(full_name = str_to_title(full_name)) %>% 
    left_join(pbp_def, by = c('opp' = 'defteam')) %>% 
    left_join(pbp_off, by = c('club_code' = 'posteam')) %>%
    left_join(def_table, by = c('opp' = 'team_name')) %>% 
    mutate(touches_game = round(total_touches / player_game_count, digits = 1), 
           mtf_per_attempt = round(elu_rush_mtf / attempts, digits = 1), 
           yco_attempt_sd = round((yco_attempt - mean(yco_attempt, na.rm=T)) / sd(yco_attempt, na.rm = T), digits = 2), 
           touches_game_sd = round((touches_game - mean(touches_game, na.rm=T)) / sd(touches_game, na.rm = T), digits = 2), 
           off_def = (def_rush_epa_rank+rdef_rank)/2 - off_rush_epa_rank, 
           attempts_game = round(attempts / player_game_count, digits = 1),
           #gap_attempts_game = round(gap_attempts / player_game_count, digits = 1), 
           #zone_attempts_game = round(zone_attempts / player_game_count, digits = 1), 
           yards_per_game = round(attempts_game * ypa, digits = 1), 
           #first_downs_att = round(first_downs / attempts, digits = 1), 
           targets_game = round(targets / player_game_count, digits = 1), 
           contest_year = nfl_year, 
           contest_week = contest_week) %>% 
    #separate(contest, into = c("folder", "contest"), sep = "./01_data/contests/") %>% 
    mutate(z_score = round(
      (0.20 * off_rush_epa_sd) - 
        (0.20 * def_rush_epa_sd) - 
        (0.20 * rdef_sd) + 
        (0.10 * (yco_attempt_sd - tack_sd)) +
        (0.30 * touches_game_sd), 
      digits = 3)) %>% 
    
    # add sd columns
    mutate(#def_rush_epa_sd = round((def_rush_epa - weighted.mean(def_rush_epa, n_plays.y.x, na.rm=T)) / sd(def_rush_epa, na.rm = T), digits = 2),
      #off_rush_epa_sd = round((off_rush_epa - weighted.mean(off_rush_epa, n_plays.y.y, na.rm=T)) / sd(off_rush_epa, na.rm = T), digits = 2),
      def_sd = round((def - mean(def)) / sd(def, na.rm = T), digits = 2), 
      rdef_sd = round((rdef - mean(rdef)) / sd(rdef, na.rm = T), digits = 2),
      tack_sd = round((tack - mean(tack)) / sd(tack, na.rm = T), digits = 2),
      prsh_sd = round((prsh - mean(prsh)) / sd(prsh, na.rm = T), digits = 2),
      cov_sd = round((cov - mean(cov)) / sd(cov, na.rm = T), digits = 2),
      yco_attempt_sd = round((yco_attempt - mean(yco_attempt, na.rm=T)) / sd(yco_attempt, na.rm = T), digits = 2), 
      touches_game_sd = round((touches_game - mean(touches_game, na.rm=T)) / sd(touches_game, na.rm = T), digits = 2), 
      
      inj_join = paste(contest_year, contest_week, club_code, full_name, sep = "_")) %>% 
    
    left_join(inj %>% select(inj_join, report_status, practice_status), by=c("inj_join")) %>% 
    
    relocate(c("z_score"), .after = game_id) %>% 
    relocate(c("off_rush_epa_sd", "def_rush_epa_sd", "rdef_sd", "yco_attempt_sd", "tack_sd", "touches_game_sd"), 
             .after = home) %>% 
    drop_na(z_score) %>% # removes players with 0 snaps
    arrange(-z_score)
}
combine_rb()

# load and deploy model --------------------------------------------------------------

# load random forest model
load("./04_models/rb_fpts_rf.Rdata")

# projections with rf model
model_projections <- predict(rb_fpts_rf, newdata = rb)

# join projections to data
rb <- rb %>% 
  mutate(fpt_proj = round(model_projections, digits = 2)) %>% 
  relocate(fpt_proj, .after = z_score) %>% 
  arrange(-fpt_proj) %>% 
  mutate(position = paste0("RB",row_number()))

view(rb, title = glue("{folder}_rb"))
