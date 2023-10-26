# create a list of all the rb slates


# 0.0 load packages -------------------------------------------------------


#load packages
suppressMessages({
  library(nflfastR) #nflfastr nflseedr nflplotr
  library(tidyverse) #ggplot2 dplyr tibble tidyr purrr forecats 
  library(ggrepel) #automatically position non-overlapping text labels
  library(glue) #interpreted literal strings
  library(gt)
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


# 2.0 load pbp and week data ----------------------------------------------


#pbp <- load_pbp(2022:2023)


# 3.0 load and join all contests ------------------------------------------


contests_rb <- lapply(contest_files, function(x){
  
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
    pbp_def_pass <- pbp %>% 
      filter(pass == 1 &
               wp > 0.1 &
               wp < 0.9 &
               half_seconds_remaining > 120 & 
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
               wp > 0.1 &
               wp < 0.9 &
               half_seconds_remaining > 120 & 
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
               wp > .10 &
               wp < .90 &
               half_seconds_remaining > 120 & 
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
               wp > .10 &
               wp < .90 &
               half_seconds_remaining > 120 & 
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
  
  # run normal rb file
  print(paste(x, ": Run running back"))
  running_back <- function(){
   
    
    salaries <- read.csv(glue("{folder}/DKSalaries.csv")) %>% 
      select(1,3,6:8) %>% 
      rename_with(~c("pos", "name", "salary", "game_info", "team")) %>% 
      separate(game_info, sep = "@", into = c("alpha", "bravo")) %>% 
      separate(bravo, sep = " ", into = c("charlie", "delta"), extra = "drop") %>% 
      mutate(opp = if_else(team == alpha, charlie, alpha)) %>% 
      select(pos, name, salary, team, opp)
    
    rushing_summary <- read.csv(glue("{folder}/pff/rushing_summary.csv"))
    
    chart_oline_dline_matchup <- read.csv(glue("{folder}/pff/oline_dline_matchup_chart.csv"))
    chart_oline_dline_matchup <- chart_oline_dline_matchup %>% 
      replace(., chart_oline_dline_matchup == 'ARZ', 'ARI') %>% 
      replace(., chart_oline_dline_matchup == 'BLT', 'BAL') %>% 
      replace(., chart_oline_dline_matchup == 'CLV', 'CLE') %>% 
      replace(., chart_oline_dline_matchup == 'HST', 'HOU') %>% 
      replace(., chart_oline_dline_matchup == 'LA', 'LAR')
    
    rb <- salaries %>%
      filter(pos == "RB") %>%
      mutate(name = str_replace(name, "Brian Robinson Jr\\.", "Brian Robinson")) %>% 
      left_join(rushing_summary, by = c('name' = 'player')) %>% 
      #left_join(rg, by=c("name" = "name")) %>% 
      #filter(pos == "RB" & proj_own >= 0) %>% 
      left_join(pbp_def, by = c('opp' = 'defteam')) %>% 
      left_join(pbp_off, by = c('team' = 'posteam')) %>%
      left_join(chart_oline_dline_matchup, by = c('team' = 'offTeam')) %>% 
      left_join(def_table, by = c('opp' = 'team_name')) %>% 
      mutate(name_salary = paste(name, salary), 
             #name_salary_own = paste(name, salary, proj_own), 
             touches_game = round(total_touches / player_game_count, digits = 1), 
             #mtf_per_attempt = round(elu_rush_mtf / rushAtt, digits = 1), 
             runBlockAdv_sd = round((runBlockAdv - mean(runBlockAdv, na.rm=T)) / sd(runBlockAdv, na.rm = T), digits = 2), 
             yco_attempt_sd = round((yco_attempt - mean(yco_attempt, na.rm=T)) / sd(yco_attempt, na.rm = T), digits = 2), 
             touches_game_sd = round((touches_game - mean(touches_game, na.rm=T)) / sd(touches_game, na.rm = T), digits = 2), 
             off_def = (def_rush_epa_rank+rdef_rank)/2 - off_rush_epa_rank, 
             bco_delta = offYardsBco - defYardsBco, 
             attempts_game = round(attempts / player_game_count, digits = 1),
             gap_attempts_game = round(gap_attempts / player_game_count, digits = 1), 
             zone_attempts_game = round(zone_attempts / player_game_count, digits = 1), 
             yards_per_game = round(attempts_game * ypa, digits = 1), 
             first_downs_att = round(first_downs / attempts, digits = 1), 
             targets_game = round(targets / player_game_count, digits = 1), 
             contest = x, 
             join = paste(name, contest, sep = "_")) %>% 
      separate(contest, into = c("folder", "contest"), sep = "./01_data/contests/")
    
    rb$sum_sd <- round(
      (0.05 * rb$runBlockAdv_sd) +
        (0.20 * rb$off_rush_epa_sd) -
        (0.20 * rb$def_rush_epa_sd) - 
        (0.20 * rb$rdef_sd) + 
        (0.05 * (rb$yco_attempt_sd - rb$tack_sd)) +
        (0.40 * rb$touches_game_sd), 
      digits = 3)
    
    rb <- rb %>% 
      #filter(proj_own != 0) %>% 
      select(name,
             team,
             salary,
             sum_sd,
             touches_game,
             targets_game,
             attempts_game,
             gap_attempts_game,
             zone_attempts_game,
             first_downs, 
             first_downs_att,
             ypa, 
             yards_per_game,
             offYardsBco,
             defYardsBco,
             bco_delta,
             runBlockAdv,
             opp,
             off_def,
             off_rush_epa_rank,
             def_rush_epa_rank,
             rdef_rank,
             breakaway_percent,
             elusive_rating,
             tack_rank,
             grades_offense,
             yco_attempt,
             yprr, 
             join) %>%
      arrange(-sum_sd)
  }
  running_back()

})

# remove objects
rm(pbp_def, pbp_off)

# bind to single dataframe
contests_rb <- bind_rows(contests_rb)
