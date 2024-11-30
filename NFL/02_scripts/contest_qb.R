# create a list of all the qb slates
# update qb fpts model

# 0.0 load packages -------------------------------------------------------

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

# 1.0 ---------------------------------------------------------------------

files <- function(){
  
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
files()

# 2.0 load pbp and calc fpts ----------------------------------------------

# define year
nfl_year <- year(Sys.Date())

# load pbp
pbp <- load_pbp(2022:nfl_year)

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

# 2.1 load spreads and totals ---------------------------------------------

odds <- function(year){
  odds <<- load_schedules(year) %>% 
    select(season, week, away_team, home_team, spread_line, total_line) %>% 
    mutate(away_spread = spread_line, 
           home_spread = spread_line * -1, 
           away_team = str_replace_all(away_team, "LA", "LAR"), 
           away_team = str_replace_all(away_team, "LARC", "LAC"),
           home_team = str_replace_all(home_team, "LA", "LAR"),
           home_team = str_replace_all(home_team, "LARC", "LAC"),
           week = sprintf("%02d", week), 
           game_id = paste(season, week, away_team, home_team, sep = "_")
    ) %>% 
    select(-spread_line)
}
odds(2022:nfl_year)

# 2.2 load injuries -------------------------------------------------------

inj <- load_injuries(2022:nfl_year) %>% 
  mutate(inj_join = paste(season, week, team, full_name, sep = "_"))

# 3.0 load qb contest -----------------------------------------------------

contests_qb <- lapply(contest_files, function(x){
  
  print(paste(x, ": Begin"))
  
  # use contest file to define year and game week
  year = as.numeric(str_sub(x, start = 1, end = 4))
  game_week = as.numeric(str_sub(x, start = 7, end = 8))
  
  # define folder
  folder = glue("./01_data/contests/{x}")
  
  # run dfs nfl defense file
  pbp <- load_pbp(year)
  
  # def epa
  print(paste(x, ": Run epa_def"))
  epa_def <- function(){
    
    # def pass epa
    pbp_def_pass <- pbp %>% 
      filter(pass == 1 & 
               year == year &
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
               year == year &
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
             avg_rank = (def_rush_epa_rank + def_pass_epa_rank) /2)
  }
  epa_def()
  
  # define off epa
  print(paste(x, ": Run epa_off"))
  epa_off <- function(){
    
    # off pass epa
    pbp_off_pass <- pbp %>% 
      filter(pass == 1 &
               year == year &
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
               year == year &
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
             avg_rank = (off_rush_epa_rank + off_pass_epa_rank) /2)
  }
  epa_off()
  
  # define pff def table
  print(paste(x, ": Run def table"))
  def_table <- function(game_week) {
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
  def_table(game_week)
  
  # define def coverage scheme
  print(paste(x, ": Run def cov scheme"))
  defense_coverage_scheme <- function(week) {
    defense_coverage_scheme <- read.csv(glue("{folder}/pff/defense_coverage_scheme.csv")) %>% 
      mutate(team_name = gsub('ARZ','ARI', team_name), 
             team_name = gsub('BLT','BAL', team_name), 
             team_name = gsub('CLV','CLE', team_name), 
             team_name = gsub('HST','HOU', team_name), 
             team_name = gsub('LA','LAR', team_name), 
             team_name = gsub('LARC','LAC', team_name), ) 
    
    defense_coverage_scheme <<- defense_coverage_scheme %>% 
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
  defense_coverage_scheme(game_week)
  
  # define def slot coverage
  print(paste(x, ": Run def slot cov"))
  slot <- function(week){
    slot <-  read.csv(glue("{folder}/pff/defense_summary.csv")) %>% 
      group_by(team_name) %>% 
      summarise(slot = round(weighted.mean(grades_coverage_defense, snap_counts_slot, na.rm = T), digits = 2)) %>% 
      mutate(slot_rank = dense_rank(desc(slot)))
    
    wide <- read.csv(glue("{folder}/pff/defense_summary.csv")) %>% 
      group_by(team_name) %>% 
      summarise(wide = round(weighted.mean(grades_coverage_defense, snap_counts_corner, na.rm = T), digits = 2)) %>% 
      mutate(wide_rank = dense_rank(desc(wide)))
    
    slot <<- slot %>% left_join(wide, by=c('team_name'))
  }
  slot(game_week)
  
  # define blitz table
  print(paste(x, ": Run team blitz"))
  blitz <- function(week) {
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
      summarise(snaps = sum(snap_counts_pass_rush.x)) %>% 
      ungroup()
    
    def_blitz <- def %>% 
      group_by(team_name) %>% 
      summarise(prsh_snaps = sum(snap_counts_pass_rush.x))
    
    def_blitz_pos <- def_blitz_pos %>% 
      left_join(def_blitz, by=c('team_name')) 
    
    def_blitz_pos <- def_blitz_pos %>% 
      mutate(blitz_pos = round(snaps/prsh_snaps, digits = 3)) %>% 
      filter(position == "LB" | position == "CB" | position == "S") 
    
    team_blitz <<- def_blitz_pos %>% 
      group_by(team_name) %>% 
      summarise(blitz_team = sum(blitz_pos)) %>% 
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
  blitz(game_week)
  
  # run te
  print(paste(x, ": Run te"))
  tight_end <- function(){
    
    salaries <- read.csv(glue("{folder}/DKSalaries.csv")) %>% 
      select(1,3,6:8) %>% 
      rename_with(~c("pos", "name", "salary", "game_info", "team")) %>% 
      separate(game_info, sep = "@", into = c("alpha", "bravo")) %>% 
      mutate(away_team = alpha) %>% 
      separate(bravo, sep = " ", into = c("charlie", "delta"), extra = "drop") %>% 
      mutate(home_team = charlie, 
             opp = if_else(team == alpha, charlie, alpha), 
             home = if_else(team == home_team, 1, 0)) %>% 
      select(pos, name, salary, team, home, opp, home_team, away_team)
    
    # load pff data
    chart_te_matchup <- read.csv(glue("{folder}/pff/te_matchup_chart.csv"))
    receiving_summary <- read.csv(glue("{folder}/pff/receiving_summary.csv"))
    receiving_summary <- replace(receiving_summary, receiving_summary =='D.K. Metcalf','DK Metcalf') 
    receiving_scheme <- read.csv(glue("{folder}/pff/receiving_scheme.csv"))
    
    # create te dataframe
    te <<- salaries %>% filter(pos=="TE") %>% 
      left_join(chart_te_matchup, by = c('name'='offPlayer')) %>% 
      left_join(receiving_summary %>% filter(position=="TE"), by = c('name'='player')) %>%
      left_join(receiving_scheme %>% select(player, man_yprr, man_routes, zone_yprr, zone_routes), by = c('name' = 'player')) %>% 
      
      left_join(pbp_def, by = c('opp' = 'defteam')) %>% 
      left_join(def_table, by = c('opp' = 'team_name')) %>% 
      left_join(defense_coverage_scheme, by = c('opp' = 'team_name')) %>% 
      
      mutate(man_zone_yprr_split = man_yprr - zone_yprr, 
             test = man_zone_yprr_split * man_percentage) %>% 
      select(team,
             opp,
             name,
             salary, 
             offYprr,
             grades_offense,
             adv, 
             def_pass_epa_rank, 
             cov_rank,
             man_zone_yprr_split,
             man_rank, 
             man_percentage, 
             test, 
             def_man_grade_rank,
             man_yprr,
             man_routes, 
             zone_rank, 
             def_zone_grade_rank, 
             zone_yprr, 
             zone_routes)
    
  }
  tight_end()
  
  # run wr
  print(paste(x, ": Run wr"))
  wide_receiver <- function(){
    
    salaries <- read.csv(glue("{folder}/DKSalaries.csv")) %>% 
      select(1,3,6:8) %>% 
      rename_with(~c("pos", "name", "salary", "game_info", "team")) %>% 
      separate(game_info, sep = "@", into = c("alpha", "bravo")) %>% 
      mutate(away_team = alpha) %>% 
      separate(bravo, sep = " ", into = c("charlie", "delta"), extra = "drop") %>% 
      mutate(home_team = charlie, 
             opp = if_else(team == alpha, charlie, alpha), 
             home = if_else(team == home_team, 1, 0)) %>% 
      select(pos, name, salary, team, home, opp, home_team, away_team)
    
    #load the wr matchup table
    chart_wr_cb_matchup <- read.csv(glue("{folder}/pff/wr_cb_matchup_chart.csv")) %>% 
      filter(defPlayer == 'All Defenders')
    
    chart_wr_cb_matchup <- chart_wr_cb_matchup %>% 
      replace(., chart_wr_cb_matchup =='ARZ','ARI') %>% 
      replace(., chart_wr_cb_matchup =='BLT','BAL') %>% 
      replace(., chart_wr_cb_matchup =='CLV','CLE') %>% 
      replace(., chart_wr_cb_matchup =='HST','HOU') %>% 
      replace(., chart_wr_cb_matchup =='JAX','JAC') %>% 
      replace(., chart_wr_cb_matchup =='LA','LAR') %>% 
      mutate(advantage = round(chart_wr_cb_matchup$advantage, digits = 1), 
             expectedSnaps = round(chart_wr_cb_matchup$expectedSnaps, digits = 1))
    
    chart_wr_cb_matchup <- replace(chart_wr_cb_matchup, chart_wr_cb_matchup =='D.K. Metcalf','DK Metcalf')
    
    # load recieving summary
    receiving_summary <- read.csv(glue("{folder}/pff/receiving_summary.csv"))
    receiving_summary <- replace(receiving_summary, receiving_summary =='D.K. Metcalf','DK Metcalf')
    
    # load scheme
    receiving_scheme <- read.csv(glue("{folder}/pff/receiving_scheme.csv"))
    receiving_scheme <- replace(receiving_scheme, receiving_scheme =='D.K. Metcalf','DK Metcalf')
    
    
    wr <- salaries %>% 
      filter(pos=="WR") %>%
      left_join(receiving_summary, by = c('name' = 'player')) %>% 
      left_join(chart_wr_cb_matchup, by = c('name' = 'offPlayer')) %>% 
      left_join(pbp_def, by = c('opp' = 'defteam')) %>% 
      left_join(defense_coverage_scheme, by = c('opp' = 'team_name')) %>% 
      left_join(receiving_scheme,
                by = c('name' = 'player')) %>% 
      left_join(def_table, by = c('opp' = 'team_name')) %>% 
      left_join(slot, by=c('opp'='team_name'))
    
    wr <- wr %>% 
      mutate(name_salary = paste(name, salary), 
             #name_salary_own = paste(name, salary, proj_own), 
             yprr_sd = round((yprr - mean(yprr, na.rm=T)) / sd(yprr, na.rm = T), digits = 2), 
             advantage_sd = round((advantage - mean(advantage, na.rm=T)) / sd(advantage, na.rm = T), digits = 2), 
             man_grade_yprr_man_cov = round((man_grades_pass_route * man_yprr * man_percentage), digits = 1), 
             man_grade_yprr_man_cov_sd = round((man_grade_yprr_man_cov - mean(man_grade_yprr_man_cov, na.rm=T)) / sd(man_grade_yprr_man_cov, na.rm = T), digits = 2), 
             targets_per_game = round(targets / player_game_count.x, digits = 1),
             targets_per_game_sd = round((targets_per_game - mean(targets_per_game, na.rm=T)) / sd(targets_per_game, na.rm = T), digits = 2), 
             man_zone_yprr_split = man_yprr - zone_yprr)
    #slot_rate_cov = round((slot_rate/100)*slot +(1-slot_rate/100)*wide,digits=1), 
    #adv = grades_pass_route-slot_rate_cov)
    
    
    wr$sum_sd <- 
      (0.20 * wr$yprr_sd) + 
      (0.20 * wr$targets_per_game_sd) +
      (0.20 * wr$advantage_sd) - 
      (0.10 * wr$def_pass_epa_sd) -
      (0.10 * wr$cov_sd) + 
      (0.20 * wr$man_grade_yprr_man_cov_sd)
    
    #Salary Table
    wr_salary_table <- wr %>%
      group_by(team) %>%
      summarise(wr_sum_salary = round(mean(salary), digits = 0))
    
    wr_count <- table(wr$team)
    
    te_salary_table <- te %>% 
      group_by(team) %>% 
      summarise(te_sum_salary = round(mean(salary), digits = 0))
    
    names(te_salary_table)[2] <- 'te_sum_salary'
    
    reciever_salary <- wr_salary_table %>% 
      left_join(te_salary_table, by = c('team' = 'team'))
    
    reciever_salary$total_rec_salary <- reciever_salary$wr_sum_salary + reciever_salary$te_sum_salary
    
    reciever_salary$total_rec_salary <- rowSums(reciever_salary[,c("wr_sum_salary", "te_sum_salary")], na.rm=TRUE)
    
    reciever_salary$total_rec_salary_sd <- round((reciever_salary$total_rec_salary - mean(reciever_salary$total_rec_salary, na.rm=T)) / sd(reciever_salary$total_rec_salary, na.rm = T), digits = 2)
    
    reciever_salary <<- reciever_salary
    
    wr <<- wr %>%
      select(name,
             team,
             salary,
             grades_pass_route, 
             advantage,
             targets_per_game,
             yprr,
             sum_sd,
             opp,
             def_pass_epa_rank,
             cov_rank,
             man_zone_yprr_split,
             man_yprr,
             zone_yprr,
             man_rank,
             zone_rank,
             man_pass_plays, 
             man_percentage,
             def_man_grade_rank,
             zone_pass_plays,
             zone_percentage,
             route_rate, 
             slot_rate, 
             slot,
             wide, 
             def_zone_grade_rank,
             def_pass_epa,
             touchdowns,
             yards_after_catch_per_reception) %>%
      arrange(-sum_sd)
  }
  wide_receiver()
  
  # run qb
  print(paste(x, ": Run quarterback"))
  quarterback <- function(){
    
    salaries <- read.csv(glue("{folder}/DKSalaries.csv")) %>% 
      select(1,3,6:8) %>% 
      rename_with(~c("pos", "name", "salary", "game_info", "team")) %>% 
      separate(game_info, sep = "@", into = c("alpha", "bravo")) %>% 
      mutate(away_team = alpha) %>% 
      separate(bravo, sep = " ", into = c("charlie", "delta"), extra = "drop") %>% 
      mutate(home_team = charlie, 
             opp = if_else(team == alpha, charlie, alpha), 
             home = if_else(team == home_team, 1, 0)) %>% 
      select(pos, name, salary, team, home, opp, home_team, away_team)
    
    pblk <- read.csv(glue("{folder}/pff/line_pass_blocking_efficiency.csv"))
    pblk <- pblk %>% 
      mutate(team_name = clean_team_abbrs(team_name)) %>% 
      mutate(pbe_rank = round(rank(-pblk$pbe), digits = 0), 
             pbe_sd = round((pblk$pbe - mean(pblk$pbe, na.rm=T)) / sd(pblk$pbe, na.rm = T), digits = 2))
    
    passing_summary <- read.csv(glue("{folder}/pff/passing_summary.csv")) %>% 
      mutate(team_name = clean_team_abbrs(team_name))
    
    passing_concept <- read.csv(glue("{folder}/pff/passing_concept.csv")) %>% 
      mutate(team_name = clean_team_abbrs(team_name))
    
    passing_pressure_blitz <- read.csv(glue("{folder}/pff/passing_pressure.csv")) %>% 
      mutate(team_name = clean_team_abbrs(team_name))
    
    qb_ids <- pbp %>% select(passer_id, passer) %>% drop_na() %>% unique()
    
    
    #QB
    qb <- salaries %>% 
      filter(pos=="QB") %>%
      mutate(name = str_replace(name, "Gardner Minshew II","Gardner Minshew")) %>% 
      filter(salary > 5000) %>% 
      left_join(passing_summary, by = c('name' = 'player')) %>% 
      left_join(pblk, by = c('team' = 'team_name')) %>% 
      left_join(passing_concept, by = c('name' = 'player')) %>% 
      left_join(reciever_salary, by = c('team' = 'team'))
    
    player <- passing_pressure_blitz$player
    blitz_dropbacks_percent <- passing_pressure_blitz$blitz_dropbacks_percent / 100
    blitz_grades_pass <- passing_pressure_blitz$blitz_grades_pass
    pressure_grades_pass <- passing_pressure_blitz$pressure_grades_pass
    
    qb_blitz <- tibble(player, blitz_dropbacks_percent, blitz_grades_pass, pressure_grades_pass)
    
    qb <- qb %>% 
      left_join(qb_blitz, by = c('name' = 'player')) %>% 
      left_join(pbp_def, by = c('opp' = 'defteam')) %>% 
      left_join(def_table, by = c('opp' = 'team_name')) %>% 
      left_join(team_blitz, by = c('opp' = 'team_name')) 
    
    qb <- qb %>% 
      mutate(#points_per_dollar = round(fantasyPoints / (salary/100), digits = 3), 
        name_salary = paste(name, salary), 
        #name_salary_own = paste(name, salary, proj_own), 
        btt_twp_ratio = round(btt_rate / twp_rate, digits = 1), 
        btt_twp_ratio_sd = round((btt_twp_ratio - mean(btt_twp_ratio, na.rm=T)) / sd(btt_twp_ratio, na.rm = T), digits = 2), 
        dropbacks_game = round(dropbacks.x / player_game_count, digits = 1), 
        pressure_vs_prsh = round(pressure_grades_pass/ prsh, digits =1), 
        pressure_vs_prsh_sd = round((pressure_vs_prsh - mean(pressure_vs_prsh, na.rm=T)) / sd(pressure_vs_prsh, na.rm = T), digits = 2), 
        ttt_run_p2s = round(avg_time_to_throw*grades_run/pressure_to_sack_rate, digits = 1), 
        contest_year = year, 
        contest_week = game_week,
        contest = x, 
        join = paste(name, contest, sep = "_"))
    
    qb$blitz_grades_pass_sq_blitz_rate <- round(qb$blitz_grades_pass^2 * qb$blitz_team, digits = -1)
    qb$blitz_grades_pass_sq_blitz_rate_sd <- round((qb$blitz_grades_pass_sq_blitz_rate - mean(qb$blitz_grades_pass_sq_blitz_rate, na.rm=T)) / sd(qb$blitz_grades_pass_sq_blitz_rate, na.rm = T), digits = 2)
    
    qb$grades_pass_sd <- round((qb$grades_pass - weighted.mean(qb$grades_pass, qb$dropbacks.x, na.rm=T)) / sd(qb$grades_pass, na.rm = T), digits = 2)
    
    qb$sum_sd <- round(
      (0.30 * qb$grades_pass_sd) +
        (0.30 * qb$def_pass_epa_sd) +
        (0.10 * (qb$total_rec_salary_sd - qb$cov_sd)) +
        (0.10 * qb$blitz_grades_pass_sq_blitz_rate_sd)+
        (0.00 * qb$pressure_vs_prsh_sd) +
        (0.00 * (qb$pbe_sd - qb$prsh_sd)), 
      digits = 3)
    
    qb <<- qb %>%
      select(name,
             team,
             home,
             salary,
             grades_pass,
             dropbacks_game,
             sum_sd,
             opp, 
             def_pass_epa, 
             def_pass_epa_rank,
             def_rank, 
             cov_rank,
             ttt_run_p2s,
             grades_run, 
             def_rush_epa_rank,
             pbe_rank,
             prsh_rank,
             prsh, 
             pressure_grades_pass, 
             avg_time_to_throw,
             avg_depth_of_target, 
             pressure_vs_prsh, 
             blitz_dropbacks_percent,
             blitz_grades_pass,
             blitz_rank,
             blitz_grades_pass_sq_blitz_rate, 
             contest_year, 
             contest_week,
             contest, 
             join, 
             away_team, 
             home_team
             ) %>%
      arrange(-sum_sd)
  }
  quarterback()
  
})

# remove objects
rm(pbp_def, pbp_off)

# 4.0 create dataframe ----------------------------------------------------

# bind to single dataframe and process data
contests_qb <- {bind_rows(contests_qb) %>% 
  drop_na() %>% 
  
  # changing name to pbp format
  separate(name, into = c("first_name", "last_name"), sep = " ", extra = "drop") %>% 
  mutate(player = paste0(substr(first_name, 1, 1), ".", last_name), 
         join = paste(contest_year, contest_week, team, player, sep = "_"), 
         name = paste(first_name, last_name)) %>%
  relocate(name, .before = "first_name") %>% 
  select(-c("first_name", "last_name")) %>%
  
  # joining fpts
  left_join(qb_fpts %>% select(join, fpts, fpts_ntile), by=c("join")) %>%
  replace_na(list(fpts = 0, fpts_ntile = 0)) %>%
  
  # joining odds
  mutate(schedule_join = sprintf("%02d", contest_week), 
         schedule_join = paste(contest_year, schedule_join, away_team, home_team, sep = "_")) %>% 
  left_join(odds %>% select(game_id, total_line, away_spread, home_spread), 
            by = c("schedule_join" = "game_id")) %>% 
  mutate(spread = if_else(home == 1, home_spread, away_spread)) %>% 
  relocate(c(spread, total_line), .after = "team") %>% 
  
  # add sd columns
  mutate(inj_join = paste(contest_year, contest_week, team, name, sep = "_")) %>% 
  
  left_join(inj %>% select(inj_join, report_status, practice_status), by=c("inj_join")) %>% 
  
  # add inj status as factors
  mutate(column = as.factor(report_status)) %>%
  mutate(id = row_number()) %>%  # create a temporary id column for reshaping
  pivot_wider(names_from = report_status, 
              values_from = report_status,
              names_prefix = "status_", 
              values_fill = 0,
              values_fn = function(x) 1) %>%
  select(-id) 
}
  
# 5.0 eda -----------------------------------------------------------------

# find individual correlations
cor(contests_qb$dropbacks_game, 
    contests_qb$fpts)

# select only numeric columns
numeric_contests_qb <- contests_qb[, sapply(contests_qb, is.numeric)]

# find cor of all variables
cor(numeric_contests_qb)[,"fpts"]

# 6.0 split train test ----------------------------------------------------

model_data <- numeric_contests_qb

set.seed(123)

# split data
split_index <- createDataPartition(model_data$fpts, 
                                   p = 0.75, 
                                   list = F, 
                                   times = 1)

train_data <- model_data[split_index, ]
test_data <- model_data[-split_index, ]

# list of models to train
models <- c("lm", "glm", # linear models 
            "gbm", # gradient boosting
            "svmRadial", # kernel trick
            "knn") # k nearest neighbors

models_not_ready <- c("rf", "nnet", "rpart")

# set up control parameters
ctrl <- trainControl(method = "cv", 
                     number = 10, 
                     savePredictions = "all")

# Train each model and store the results.
qb_pts_models <- lapply(models, function(model){
  
  print(paste(model, "start"))
  
  set.seed(1)
  
  fit <- train(fpts ~ salary + spread + total_line + home +
                 grades_pass + blitz_grades_pass_sq_blitz_rate + dropbacks_game + 
                 def_pass_epa +
                 status_NA + status_Questionable + status_Out, 
               data = train_data, 
               method = model, 
               trControl = ctrl)
  
})
# removed temporarily
#status_Doubtful

names(qb_pts_models) <- models

#Evaluate Models on Test Data:
predictions <- lapply(qb_pts_models, function(fit) predict(fit, test_data))

# get performance metrics
performance <- lapply(predictions, function(pred) postResample(pred, test_data$fpts))

# visualize and compare
performance_df <- as.data.frame(do.call(rbind, performance))
performance_df

# 6.3 model selection -----------------------------------------------------

save(qb_pts_models, file = "./04_models/qb_pts_models.RData")
