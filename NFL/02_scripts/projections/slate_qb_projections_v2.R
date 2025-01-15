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
  nfl_year <<- year(Sys.Date())-1
  
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

# calc qb fpts by game week
qb_fpts_pbp <- function(){
  
  # Load regular season data
  qb_pbp <- pbp %>% 
    group_by(game_id, passer, passer_id, posteam) %>% 
    summarize(
      
      epa = round(sum(qb_epa, na.rm = T), digits = 2),
      snaps = n(),
      epa_per_play = round(epa/snaps, digits = 2),
      
      pass_attempt = sum(pass_attempt, na.rm = T), 
      passing_yards = sum(passing_yards, na.rm = T), 
      pass_touchdown = sum(pass_touchdown, na.rm = T), 
      interception = sum(interception, na.rm = T), 
      
      qb_scramble = sum(qb_scramble, na.rm = T),
      rush_attempt = sum(rush_attempt, na.rm = T),
      rushing_yards = sum(rushing_yards, na.rm = T),
      rush_touchdown = sum(rush_touchdown, na.rm = T),
      
      fumble = sum(fumble, na.rm = T),
      
      # use to get last non-missing values
      season_type = last(season_type), 
      temp = last(temp), 
      wind = last(wind), 
      spread_line = last(spread_line), 
      total_line = last(total_line), 
      posteam = last(posteam), 
      week = last(week), 
      season = last(season)
      
    ) %>% 
    ungroup() %>% 
    mutate(join = paste(game_id, passer_id, sep = "_"))
  
  # Get rusher rushing stats
  rusher_pbp <- pbp %>% 
    group_by(game_id, rusher, rusher_id) %>% 
    summarise(
      
      rush_attempt = sum(rush_attempt, na.rm = T),
      rushing_yards = sum(rushing_yards, na.rm = T),
      rush_touchdown = sum(rush_touchdown, na.rm = T),
      
      fumble = sum(fumble, na.rm = T)) %>% 
    
    drop_na() %>% 
    ungroup() %>% 
    mutate(join = paste(game_id, rusher_id, sep = "_")) %>% 
    select(-c("game_id"))
  
  # join stats and calc fpts, dk scoring
  qb_fpts <<- qb_pbp %>% 
    left_join(rusher_pbp, by=c("join")) %>% # add passer + rusher stat 
    
    # add zeros so that the columns can be added
    mutate(across(c(rush_attempt.x, rush_attempt.y, 
                    rushing_yards.x, rushing_yards.y, 
                    rush_touchdown.x, rush_touchdown.y, 
                    fumble.x, fumble.y), 
                  ~ replace_na(., 0))) %>% 
    
    # add the columns from scrambles + designed rushes
    mutate(rush_attempt = rush_attempt.x + rush_attempt.y, 
           rushing_yards = rushing_yards.x + rushing_yards.y,
           rush_touchdown = rush_touchdown.x + rush_touchdown.y,
           fumble = fumble.x + fumble.y) %>% 
    select(-c("rush_attempt.x", "rush_attempt.y", "rushing_yards.x", "rushing_yards.y", 
             "rush_touchdown.x", "rush_touchdown.y", "fumble.x", "fumble.y", "join")) %>% 
    
    relocate(c("rush_attempt", "rushing_yards", "rush_touchdown", "fumble"), .after = qb_scramble) %>% 
    
    drop_na(passer) %>% 
    mutate(
      big_rush = ifelse(rushing_yards > 100, 1,0), 
      big_pass = ifelse(passing_yards > 300, 1,0), 
      fpts = 
        
        big_pass * 3 +
        big_rush * 3 +
        
        pass_touchdown * 4 +
        passing_yards * .04 +
        interception * -1 +
        
        rushing_yards * .1 +
        rush_touchdown * 6 +
        fumble * -1, 
      
      fpts_ntile = ntile(fpts, 100)
    ) %>% 
    arrange(-fpts) %>% 
    
    mutate(join = tolower(paste(season, week, posteam, passer, sep = "_"))) %>% 
    
    # add field data from schedule
    left_join(schedule %>% select(game_id, roof), by=c("game_id")) %>% 
    mutate(temp = if_else(roof == "closed" | roof == "dome", 70, temp), 
           wind = if_else(is.na(wind), 0, wind), 
           weather_check = if_else(temp == 0 & wind == 0, 0, 1), 
           dome_games = if_else(roof == "dome" | roof == "closed",1,0)) %>% 
    relocate(weather_check, .after = wind) %>% 
    
    relocate(c("fpts", "fpts_ntile", "spread_line", "total_line"), .after = posteam) %>% 
    relocate(c("snaps", "epa", "epa_per_play"), .after = big_pass)
  
}
qb_fpts_pbp()

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
    filter(week == contest_week-1) %>% 
    mutate(week = week +1) %>% 
    
    # current week depth charts n
    #filter(week == 20) %>% # hard code for playoffs
    
    filter(position == "QB") %>% 
    filter(depth_team == 1) %>% 
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
    drop_na() %>% # to remove bye week teams
    
    # add schedule
    left_join(schedule %>% select(game_id, gameday, weekday, gametime), by=c("game_id")) 

}
nfl_depth()

# load and process pff qb data
pff_pass <- function(){
  
  passing_summary <<- read.csv(glue("./01_data/contests/{folder}/pff/passing_summary.csv")) %>% 
    mutate(player = clean_player_names(player, lowercase = T), 
           team_name = clean_team_abbrs(team_name), 
           player = clean_player_names(player), 
           
           td_game = round(touchdowns / player_game_count, digits = 1),
           pyards_game = round(yards / player_game_count), digits = 1) %>% 
    select(-c("position"))
  
  pblk <<- read.csv(glue("./01_data/contests/{folder}/pff/line_pass_blocking_efficiency.csv")) %>% 
    mutate(team_name = clean_team_abbrs(team_name),
           
           pressures_game = round(pressures_allowed / player_game_count, digits = 1),
           pbe_rank = round(rank(-pbe), digits = 0), 
           pbe_sd = round((pbe - mean(pbe, na.rm=T)) / sd(pbe, na.rm = T), digits = 2)) %>% 
    select(team_name, pbe, pressures_game)
  
  passing_pressure <<- read.csv(glue("./01_data/contests/{folder}/pff/passing_pressure.csv")) %>% 
    mutate(player = clean_player_names(player, lowercase = T),
           player = clean_player_names(player)) %>% 
    select(c("player","no_blitz_grades_pass", "blitz_grades_pass"))
  
  #passing_concept <<- read.csv(glue("./01_data/contests/{folder}/pff/passing_concept.csv")) %>% 
    #mutate(team_name = clean_team_abbrs(team_name), 
           #player = clean_player_names(player))
  
  receiving_summary <<- read.csv(glue("./01_data/contests/{folder}/pff/receiving_summary.csv")) %>%
    mutate(player = clean_player_names(player, lowercase = T), 
           team_name = clean_team_abbrs(team_name), 
           player = clean_player_names(player), 
           
           td_game = round(touchdowns / player_game_count, digits = 1),
           ryards_game = round(yards / player_game_count, digits = 1))
  
  receiving_summary_group <<- receiving_summary %>% 
    group_by(team_name) %>% 
    summarize(team_rec = round(weighted.mean(x = grades_pass_route, w = routes), digits = 1),
              team_yprr = round(weighted.mean(x = yprr, w = routes), digits = 2))
  
  qb_ids <- pbp %>% select(passer_id, passer) %>% drop_na() %>% unique()
}
pff_pass()

# load and process pff rushing summary
pff_rush <- function() {
  # load pff rushing data
  rushing_summary <- read.csv(glue("./01_data/contests/{folder}/pff/rushing_summary.csv")) %>% 
    select(player, player_id, player_game_count, team_name, 
           total_touches, elu_rush_mtf, elu_recv_mtf,
           touchdowns, 
           attempts, yco_attempt, ypa, grades_run, explosive,
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

blitz <- function(week) {
  
  # load fukes
  defense_summary <- read.csv(glue("{folder}/pff/defense_summary.csv"))
  pass_rush_summary <- read.csv(glue("{folder}/pff/pass_rush_summary.csv")) 
  run_defense_summary <- read.csv(glue("{folder}/pff/run_defense_summary.csv")) 
  defense_coverage_summary <- read.csv(glue("{folder}/pff/defense_coverage_summary.csv"))
  defense_coverage_scheme <- read.csv(glue("{folder}/pff/defense_coverage_scheme.csv"))
  slot_coverage <- read.csv(glue("{folder}/pff/slot_coverage.csv"))
  pass_rush_productivity <- read.csv(glue("{folder}/pff/pass_rush_productivity.csv"))
  
  def_list <- list()
  
  def_list[[1]] <- list(defense_summary, 
                        pass_rush_summary %>% select(2,6:dim(pass_rush_summary)[2]), 
                        run_defense_summary %>% select(2,6:dim(run_defense_summary)[2]), 
                        defense_coverage_summary %>% select(2,6:dim(defense_coverage_summary)[2]), 
                        defense_coverage_scheme %>% select(2,6:dim(defense_coverage_scheme)[2]), 
                        slot_coverage %>% select(2,6:dim(slot_coverage)[2]), 
                        pass_rush_productivity %>% select(2,6:dim(pass_rush_productivity)[2])) %>% 
    reduce(left_join, by = "player_id")
  
  def <- def_list[[1]] %>% filter(!position %in% c('G','WR','HB','FB','TE'))
  
  #blitz
  def_blitz_pos <- def %>% 
    group_by(team_name, position) %>% 
    summarise(snaps = sum(snap_counts_pass_rush.x), 
              .groups = "drop") %>% 
    ungroup()
  
  def_blitz <- def %>% 
    group_by(team_name) %>% 
    summarise(prsh_snaps = sum(snap_counts_pass_rush.x), 
              .groups = "drop")
  
  def_blitz_pos <- def_blitz_pos %>% 
    left_join(def_blitz, by=c('team_name')) 
  
  def_blitz_pos <- def_blitz_pos %>% 
    mutate(blitz_pos = round(snaps/prsh_snaps, digits = 3)) %>% 
    filter(position == "LB" | position == "CB" | position == "S") 
  
  team_blitz <<- def_blitz_pos %>% 
    group_by(team_name) %>% 
    summarise(blitz_team = sum(blitz_pos), .groups = "drop") %>% 
    mutate(blitz_rank = dense_rank(desc(blitz_team)), 
           team_name = gsub('ARZ','ARI', team_name), 
           team_name = gsub('BLT','BAL', team_name), 
           team_name = gsub('CLV','CLE', team_name), 
           team_name = gsub('HST','HOU', team_name), 
           #team_name = gsub('JAX','JAC', team_name), 
           team_name = gsub('LA','LAR', team_name), 
           team_name = gsub('LARC','LAC', team_name)) 
  
  def_blitz_pos <- def_blitz_pos %>% 
    left_join(team_blitz, by=c('team_name')) 
  
}

# combine all qb stats
combine_qb <- function(){
  qb <<- depth_charts %>%
    
    left_join(passing_summary, by = c('full_name' = 'player')) %>% 
    
    left_join(pblk, by = c('club_code' = 'team_name')) %>% 
    left_join(passing_pressure, by = c('full_name' = 'player')) %>% 
    left_join(receiving_summary_group, by = c("team_name")) %>% 
    
    mutate(full_name = str_to_title(full_name)) %>% 
    
    left_join(pbp_def, by = c('opp' = 'defteam')) %>% 
    left_join(pbp_off, by = c('club_code' = 'posteam')) %>%
    left_join(def_table, by = c('opp' = 'team_name')) %>% 
    left_join(schedule %>% select(game_id, roof), by =c("game_id")) %>% 
    mutate(dome = if_else(roof == "dome",1,0), 
           dome = if_else(is.na(dome), 0, dome),
           z_score = round(
             (0.20 * off_pass_epa_sd) - (0.20 * def_pass_epa_sd) - (0.20 * cov_sd), digits = 3))
}
combine_qb()


# 2.1.0 load and deploy rf fpts model -----------------------------------------


# load rf models
load("./04_models/qb/qb_fpts_rf.Rdata")
load("./04_models/qb/qb_pass_attempt_rf.Rdata")
load("./04_models/qb/qb_passing_yards_rf.Rdata")
load("./04_models/qb/qb_pass_touchdown_rf.Rdata")
load("./04_models/qb/qb_rush_attempt_rf.Rdata")
load("./04_models/qb/qb_rushing_yards_rf.Rdata")

# make projections with rf models
model_projections_qb_fpts <- predict(qb_fpts_rf, newdata = qb)
model_projections_qb_pass_attempt <- predict(qb_pass_attempt_rf, newdata = qb)
model_projections_qb_passing_yards <- predict(qb_passing_yards_rf, newdata = qb)
model_projections_qb_pass_touchdown <- predict(qb_pass_touchdown_rf, newdata = qb)
model_projections_qb_rush_attempt <- predict(qb_rush_attempt_rf, newdata = qb)
model_projections_qb_rushing_yards <- predict(qb_rushing_yards_rf, newdata = qb)

# join projections to data
qb <- qb %>% 
  mutate(fpt_proj = round(model_projections_qb_fpts, digits = 2), 
         pass_attempt = round(model_projections_qb_pass_attempt, digits = 2), 
         passing_yards_proj = round(model_projections_qb_passing_yards, digits = 2), 
         pass_touchdown_proj = round(model_projections_qb_pass_touchdown, digits = 2), 
         rush_attempt_proj = round(model_projections_qb_rush_attempt, digits = 2), 
         rushing_yards_proj = round(model_projections_qb_rushing_yards, digits = 2)) %>% 
  relocate(c("fpt_proj", 
             "pass_attempt", "passing_yards_proj", "pass_touchdown_proj", 
             "rush_attempt_proj", "rushing_yards_proj"),
           .after = full_name) %>% 
  relocate(c("def_pass_epa_rank", "def_rush_epa_rank"), .after = opp) %>% 
  relocate(pyards_game, .after = team_name) %>% 
  arrange(-fpt_proj)

# join projections to data and view
qb_slate <- qb %>% 
  #filter(weekday == "Monday") %>% # toggle as needed for slates
  select(7:35) %>% 
  distinct(full_name, .keep_all = T) %>% 
  mutate(position = paste0("qb",row_number())) %>% 
  view(title = glue("{folder}_qb"))


# 3.0 write and upload projections ----------------------------------------

# write a local csv
write.csv(qb_slate, file = glue("./05_outputs/proj/{folder}_qb.csv"))

# authorize google sheets
gs4_auth(email = "michael.john.francis2015@gmail.com")

# sheet id
sheet_id <- "https://docs.google.com/spreadsheets/d/1wo7QLvS5nbj6v3GVOlNs3Htw1VXhW9TEHCWkD-LHPSQ/edit?gid=0#gid=0"

# overwrite an entire sheet
sheet_write(qb_slate, ss = sheet_id, sheet = "qb")
