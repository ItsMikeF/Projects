# create a list of all the rb slates

# 0.0 load packages -------------------------------------------------------


# load packages
suppressMessages({
  
  #nflverse packages
  options(nflreadr.verbose = FALSE)
  library(nflfastR) # pbp data
  library(nflreadr) # nfl schedule
  library(tidyverse) # ggplot2 dplyr tibble tidyr purrr forecats 
  
  # data pacakges
  library(glue) # interpreted literal strings
  
  # modeling packages
  library(caret) # data partition
  library(randomForest) # rf model
  library(ranger) # fast implementation of random forest
  #library(MultivariateRandomForest) # models multivariate cases using random forests
  
  
})


# 1.0 ---------------------------------------------------------------------


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


# 2.0 nflverse data ---------------------------------------------


# load spreads and totals
odds <- function(year){
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
odds(2022:nfl_year)

#load injuries
inj <- load_injuries(2022:nfl_year) %>% 
  mutate(inj_join = paste(season, week, team, full_name, sep = "_"))

# load schedule
schedule <- load_schedules(2022:nfl_year)


# 2.1 load pbp and calc fpts ----------------------------------------------


# load pbp
pbp <- load_pbp(data_start:nfl_year) %>% 
  mutate(weather = as.character(weather))

# calc rb fpts by game week
qb_fpts_pbp <- function(){
  
  # Load regular season data
  qb_pbp <- pbp %>% 
    group_by(game_id, passer, passer_id, posteam) %>% 
    summarize(
      
      epa = round(sum(qb_epa, na.rm = T), digits = 2),
      snaps = n(),
      epa_per_play = round(epa/snaps, digits = 2),
      
      passing_yards = sum(passing_yards, na.rm = T), 
      pass_attempt = sum(pass_attempt, na.rm = T), 
      pass_touchdown = sum(pass_touchdown, na.rm = T), 
      interception = sum(interception, na.rm = T), 
      
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
  
  # Get passer rushing stats
  passer_pbp <- pbp %>% 
    group_by(game_id, passer, passer_id, posteam) %>% 
    summarise(
      
      qb_scramble = sum(qb_scramble, na.rm = T),
      rush_attempt = sum(rush_attempt, na.rm = T),
      rushing_yards = sum(rushing_yards, na.rm = T),
      rush_touchdown = sum(rush_touchdown, na.rm = T),
      
      fumble = sum(fumble, na.rm = T), 
      
    ) %>% 
    drop_na() %>% 
    ungroup() %>% 
    mutate(join = paste(game_id, passer_id, sep = "_"))
  
  # Get rusher rushing stats
  rusher_pbp <- pbp %>% 
    group_by(game_id, season_type, temp, spread_line, total_line, 
             rusher, rusher_id, posteam, week, season) %>% 
    summarise(
      
      rush_attempt = sum(rush_attempt, na.rm = T),
      rushing_yards = sum(rushing_yards, na.rm = T),
      rush_touchdown = sum(rush_touchdown, na.rm = T),
      
      fumble = sum(fumble, na.rm = T)) %>% 
    drop_na() %>% 
    ungroup() %>% 
    mutate(join = paste(game_id, rusher_id, sep = "_"))
  
  qb_rush <- passer_pbp %>%
    left_join(rusher_pbp, by = "join") %>%
    
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
    select(rush_attempt, rushing_yards, rush_touchdown, fumble, join)
  
  # join stats and calc fpts, dk scoring
  qb_fpts <<- qb_pbp %>% 
    left_join(qb_rush, by=c("join")) %>% # add passer + rusher stat 
    select(-c("join")) %>% # drop the join column
    drop_na(passer) %>% 
    replace(is.na(.),0) %>% 
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
    mutate(join = paste(season, week, posteam, passer, sep = "_")) %>% 
    left_join(schedule %>% select(game_id, roof), by=c("game_id")) %>% 
    mutate(temp = if_else(roof == "closed" | roof == "dome", 70, temp), 
           wind = if_else(is.na(wind), 0, wind), 
           weather_check = if_else(temp == 0 & wind == 0, 0, 1)) %>% 
    relocate(weather_check, .after = wind) %>% 
    relocate(c("fpts", "fpts_ntile", "spread_line", "total_line"), .after = posteam)
  
}
qb_fpts_pbp()

contests_rb <- lapply(contest_files, function(x){
  
  print(paste(x, ": Begin"))
  
  # use contest file to define year and game week
  game_year = as.numeric(str_sub(x, start = 1, end = 4)) # first 4 digits
  game_week = as.numeric(str_sub(x, start = 7, end = 8)) # last 2 digits
  
  # define folder
  folder = glue("./01_data/contests/{x}")
  
  # load pbp for game year
  pbp <- load_pbp(game_year)
  
  # def epa
  print(paste(x, ": Run epa_def"))
  epa_def <- function(){
    
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
             #defteam = gsub('LA','LAR', defteam), 
             #defteam = gsub('LARC','LAC', defteam), 
             avg_rank = (def_rush_epa_rank + def_pass_epa_rank) /2) %>% 
      select(defteam, 
             def_rush_epa, def_rush_epa_rank, def_rush_epa_sd, 
             def_pass_epa, def_pass_epa_rank, def_pass_epa_sd)
  }
  epa_def()
  
  # define off epa
  print(paste(x, ": Run epa_off"))
  epa_off <- function(){
    
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
             #posteam = gsub('LA','LAR', posteam), 
             #posteam = gsub('LARC','LAC', posteam),
             avg_rank = (off_rush_epa_rank + off_pass_epa_rank) /2) %>% 
      select(posteam, 
             off_rush_epa, off_rush_epa_rank, off_rush_epa_sd, 
             off_pass_epa, off_pass_epa_rank, off_pass_epa_sd)
  }
  epa_off()
  
  # define pff def table
  print(paste(x, ": Run def table"))
  def_table <- function() {
    def <- read.csv(glue("{folder}/pff/defense_summary.csv"))
    
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
  
  # process and join rb data
  print(paste(x, ": Run running back"))
  quarterback <- function(){
    
    # load depth chart
    depth_charts <- load_depth_charts(seasons = game_year) %>% 
      filter(week == game_week) %>%
      
      filter(position == "QB") %>% 
      filter(depth_position == "QB") %>% 
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
      
      select(-c(away_team, home_team, away_spread, home_spread, total_line.x, total_line.y))
    
    print(paste(x, ": Depth Charts"))
    print(depth_charts)
    
    # load and process pff qb data
    pff_pass <- function(){
      pblk <<- read.csv(glue("{folder}/pff/line_pass_blocking_efficiency.csv")) %>% 
        mutate(team_name = clean_team_abbrs(team_name)) %>% 
        mutate(pbe_rank = round(rank(-pbe), digits = 0), 
               pbe_sd = round((pbe - mean(pbe, na.rm=T)) / sd(pbe, na.rm = T), digits = 2))
      
      passing_summary <<- read.csv(glue("{folder}/pff/passing_summary.csv")) %>% 
        mutate(team_name = clean_team_abbrs(team_name), 
               player = clean_player_names(player))
      
      passing_pressure <<- read.csv(glue("{folder}/pff/passing_pressure.csv")) %>% 
        mutate(team_name = clean_team_abbrs(team_name), 
               player = clean_player_names(player))
      
      qb_ids <- pbp %>% select(passer_id, passer) %>% drop_na() %>% unique()
    }
    pff_pass()
    
    print(paste(x, ": Rushing Summary"))
    
    # join data
    Qb <<- depth_charts %>%
      left_join(rushing_summary, by = c('full_name' = 'player')) %>% 
      mutate(full_name = str_to_title(full_name)) %>% 
      left_join(pbp_def, by = c('opp' = 'defteam')) %>% 
      left_join(pbp_off, by = c('club_code' = 'posteam')) %>%
      left_join(def_table, by = c('opp' = 'team_name')) %>% 
      mutate(
             
             contest_year = nfl_year, 
             contest_week = game_week,
             contest = x) %>% 
      
      separate(contest, into = c("folder", "contest"), sep = "./01_data/contests/") %>% 
      mutate(z_score = round(
        (0.20 * off_rush_epa_sd) - 
          (0.20 * def_rush_epa_sd) - 
          (0.20 * rdef_sd) + 
          (0.10 * (yco_attempt_sd - tack_sd)) +
          (0.30 * touches_game_sd), 
        digits = 3))
    
  }
  running_back()
  
})