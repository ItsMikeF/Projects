# create a list of all the wr slates

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

  library(future.apply)
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
      
      # use to get last non-missing values
      season_type = last(season_type), 
      temp = last(temp), 
      wind = last(wind), 
      spread_line = last(spread_line), 
      total_line = last(total_line), 
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
    relocate(c("season","week","join"), .after = big_rec) %>% 
    
    mutate(join = tolower(paste(season, week, posteam, receiver, sep = "_")))
  
}
wr_fpts_pbp()


# create list of all wr slates
contests_wr <- lapply(contest_files, function(x){
  
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
  
  # load and process pff data
  pff_rec <- function() {
    
    # load recieving summary
    receiving_summary <<- read.csv(glue("{folder}/pff/receiving_summary.csv")) %>% 
      mutate(player = clean_player_names(player, lowercase = T), # must be lowercase for join
             
             rec_game = round(receptions / player_game_count, digits = 2),
             rec_yards_game = round(yards / player_game_count, digits = 2),
             rec_td_game = round(touchdowns / player_game_count,digits = 2)) %>% 
      select(-c("receptions")) # must remove for the join
    
    # load scheme
    receiving_scheme <<- read.csv(glue("{folder}/pff/receiving_scheme.csv")) %>% 
      mutate(player = clean_player_names(player, lowercase = T)) %>% 
      select(player, man_yprr, zone_yprr)
    
    defense_coverage_scheme <<- read.csv(glue("{folder}/pff/defense_coverage_scheme.csv")) %>% 
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
  
  # load and processdepth charts
  nfl_depth <- function() {
    # load depth chart
    depth_charts <<- load_depth_charts(seasons = nfl_year) %>% 
      # current week depth charts not always available
      #filter(week == game_week-1) %>% 
      #mutate(week = week +1) %>% 
      
      # current week depth charts n
      filter(week == game_week) %>% 
      
      filter(position == "WR") %>% 
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
  
  # combine all wr data
  combine_wr <- function(){
    
    wr <<- depth_charts %>%
      
      left_join(receiving_summary, by = c('full_name' = 'player')) %>% 
      left_join(receiving_scheme,by = c('full_name' = 'player')) %>% 
      
      left_join(pbp_def, by = c('opp' = 'defteam')) %>% 
      left_join(pbp_off, by = c('club_code' = 'posteam')) %>%
      
      left_join(defense_coverage_scheme, by = c('opp' = 'team_name')) %>% 
      left_join(def_table, by = c('opp' = 'team_name')) %>% 
      
      mutate(z_score = off_pass_epa_sd - def_pass_epa_sd)
    
  }
  combine_wr()
  
  return(wr)
  
}) # detect number of cores

# 4.0 create dataframe ----------------------------------------------------


process_wr_df <- function(){
  # bind to single dataframe and process data
  contests_wr_df <<- bind_rows(contests_wr) %>% 
    
    filter(week != 2) %>% # remove week 2s, bad data
    
    # changing name to pbp format
    separate(full_name, into = c("first_name", "last_name"), sep = " ", extra = "drop") %>% 
    mutate(player = paste0(substr(first_name, 1, 1), ".", last_name), 
           join = tolower(paste(season, week, club_code, player, sep = "_")), 
           name = paste(first_name, last_name)) %>%
    relocate(name, .before = "first_name") %>% 
    select(-c("first_name", "last_name")) %>%
    
    # joining fpts
    left_join(wr_fpts %>% select(join, fpts, fpts_ntile, 
                                 #temp, wind, 
                                 receptions, receiving_yards, pass_touchdown), by=c("join")) %>%
    replace_na(list(fpts = 0, fpts_ntile = 0)) %>%
    
    # add percentile
    mutate(fpts_ntile = ntile(fpts, 100)) %>% 
    
    # add sd columns
    mutate(#def_rush_epa_sd = round((def_rush_epa - weighted.mean(def_rush_epa, n_plays.y.x, na.rm=T)) / sd(def_rush_epa, na.rm = T), digits = 2),
      #off_rush_epa_sd = round((off_rush_epa - weighted.mean(off_rush_epa, n_plays.y.y, na.rm=T)) / sd(off_rush_epa, na.rm = T), digits = 2),
      def_sd = round((def - mean(def)) / sd(def, na.rm = T), digits = 2), 
      rdef_sd = round((rdef - mean(rdef)) / sd(rdef, na.rm = T), digits = 2),
      tack_sd = round((tack - mean(tack)) / sd(tack, na.rm = T), digits = 2),
      prsh_sd = round((prsh - mean(prsh)) / sd(prsh, na.rm = T), digits = 2),
      cov_sd = round((cov - mean(cov)) / sd(cov, na.rm = T), digits = 2)) %>% 
    
    #inj_join = paste(contest_year, contest_week, club_code, name, sep = "_")
    
    #left_join(inj %>% select(inj_join, report_status, practice_status), by=c("inj_join")) %>% 
    
    # add inj status as factors
    #mutate(column = as.factor(report_status)) %>%
    #mutate(id = row_number()) %>%  # create a temporary id column for reshaping
    #pivot_wider(names_from = report_status, 
    #            values_from = report_status,
    #            names_prefix = "status_", 
    #            values_fill = 0,
    #            values_fn = function(x) 1) %>%
    #mutate(depth_team = as.numeric(depth_team)) %>% 
    #select(-id) %>% 
    
    left_join(schedule %>% select(game_id, roof, temp, wind), by =c("game_id")) %>%
    mutate(
      dome = if_else(roof == "dome",1,0),
      dome = if_else(is.na(dome), 0, dome)) %>% 
    
    #filter(attempts > 10) %>% 
    #filter(status_Out == 0) %>% 
    
    mutate(depth_team = as.numeric(depth_team)) %>% 
    
    relocate(c("z_score", "fpts", "fpts_ntile", "receiving_yards", "targets", "pass_touchdown"), .after = game_id) %>% 
    relocate(c("off_pass_epa_sd", "def_pass_epa_sd", "cov_sd"), .after = home) %>% 
    arrange(-fpts) %>% 
    filter(fpts != 0)
}
process_wr_df()

names(contests_wr_df)

# 5.0 find correlated variables-----------------------------------------------

# select only numeric columns
numeric_contest_wr <<- contests_wr_df[, sapply(contests_wr_df, is.numeric)]

# use to look for features of new models
correlation_table <- function() {
  
  # select only numeric columns
  numeric_contest_wr <<- contests_wr_df[, sapply(contests_wr_df, is.numeric)]
  
  # find cor of all variables
  cor_df <- as_tibble(cor(numeric_contest_wr)[,"fpts"])
  
  ## gpt built code
  # Compute the correlation matrix for numeric variables
  cor_matrix <- cor(numeric_contest_wr)
  
  # Convert the correlation matrix to a dataframe
  cor_df <- as.data.frame(cor_matrix)
  
  # Optional: Add a column for row names (variable names) for reference
  cor_df <- tibble::rownames_to_column(cor_df, var = "Variable")
  
  # If you only want correlations with 'fpts' column
  cor_fpts_df <<- cor_df %>% select(Variable, fpts) %>% arrange(-fpts)
}
correlation_table()


# 6.0 split train test ----------------------------------------------------


model_data <- numeric_contest_wr %>% 
  filter(depth_team == 1 | depth_team == 2) %>% 
  drop_na(fpts)

set.seed(10)

# split data
split_index <- createDataPartition(model_data$fpts, 
                                   p = 0.75, 
                                   list = F, 
                                   times = 1)

train_data <- model_data[split_index, ]
test_data <- model_data[-split_index, ]

library(mice)
imputed_data <- mice(train_data, m = 1)
train_data <- complete(imputed_data)


# 6.1 random forest model and tuning-------------------------------------------


# train random forest model
wr_fpts_rf <- randomForest(fpts ~  
                             spread + total_line + dome + #temp + wind + # game data
                             rec_game + rec_yards_game + rec_td_game + # rec stats
                             yprr + # efficiency
                             grades_pass_route + # wr grades
                             def_pass_epa, # pass defense
                           data = train_data, 
                           mtry = 2, 
                           nodesize = 5,
                           ntree = 1000)  

# use wr_fpts_rf to predict on test data
wr_fpts_rf_predictions <- round(predict(wr_fpts_rf, test_data), digits = 2)

# evaulated predictions 
wr_fpts_rf_performance <- round(postResample(wr_fpts_rf_predictions, test_data$fpts), digits = 3)
wr_fpts_rf_performance


# Define the tuning grid
tune_grid <- expand.grid(mtry = c(1, 2, 3, 4, 5), # Experiment with different mtry values
                         splitrule = "variance", 
                         min.node.size = c(5, 10, 15, 20, 25, 50, 100))

# Train the model with caret
wr_fpts_rf_tuned <- train(
  fpts ~  
    spread + total_line + dome + #temp + wind + # game data
    rec_game + rec_yards_game + rec_td_game + # rec stats
    yprr + # efficiency
    grades_pass_route + # wr grades
    def_pass_epa, # pass defense
  data = train_data,
  method = "ranger",
  trControl = trainControl(method = "cv", number = 5), # 5-fold cross-validation
  tuneGrid = tune_grid
)

# View the best model
print(wr_fpts_rf_tuned$bestTune)

# Dotchart of variable importance as measured by a Random Forest
varImpPlot(wr_fpts_rf)

# save model
save(wr_fpts_rf, file = "./04_models/wr/wr_fpts_rf.Rdata")


# 6.2 random forest model and tuning-------------------------------------------


# train random forest model
wr_receptions_rf <- randomForest(receptions ~  
                             spread + total_line + dome + #temp + wind + # game data
                             rec_game + rec_yards_game + rec_td_game + # receptions stats
                             yprr + # efficiency
                             grades_pass_route + # wr grades
                             def_pass_epa, # pass defense
                           data = train_data, 
                           mtry = 2, 
                           nodesize = 5,
                           ntree = 1000)  

# use wr_receptions_rf to predict on test data
wr_receptions_rf_predictions <- round(predict(wr_receptions_rf, test_data), digits = 2)

# evaulated predictions 
wr_receptions_rf_performance <- round(postResample(wr_receptions_rf_predictions, test_data$receptions), digits = 3)
wr_receptions_rf_performance


# Define the tuning grid
tune_grid <- expand.grid(mtry = c(1, 2, 3, 4, 5), # Experiment with different mtry values
                         splitrule = "variance", 
                         min.node.size = c(5, 10, 15, 20, 25, 50, 100))

# Train the model with caret
wr_receptions_rf_tuned <- train(
  receptions ~  
    spread + total_line + dome + #temp + wind + # game data
    rec_game + rec_yards_game + rec_td_game + # receptions stats
    yprr + # efficiency
    grades_pass_route + # wr grades
    def_pass_epa, # pass defense
  data = train_data,
  method = "ranger",
  trControl = trainControl(method = "cv", number = 5), # 5-fold cross-validation
  tuneGrid = tune_grid
)

# View the best model
print(wr_receptions_rf_tuned$bestTune)

# Dotchart of variable importance as measured by a Random Forest
varImpPlot(wr_receptions_rf)

# save model
save(wr_receptions_rf, file = "./04_models/wr/wr_receptions_rf.Rdata")


# 6.2 random forest model and tuning-------------------------------------------


# train random forest model
wr_receiving_yards_rf <- randomForest(receiving_yards ~  
                                   spread + total_line + dome + #temp + wind + # game data
                                   rec_game + rec_yards_game + rec_td_game + # receiving_yards stats
                                   yprr + # efficiency
                                   grades_pass_route + # wr grades
                                   def_pass_epa, # pass defense
                                 data = train_data, 
                                 mtry = 2, 
                                 nodesize = 5,
                                 ntree = 1000)  

# use wr_receiving_yards_rf to predict on test data
wr_receiving_yards_rf_predictions <- round(predict(wr_receiving_yards_rf, test_data), digits = 2)

# evaulated predictions 
wr_receiving_yards_rf_performance <- round(postResample(wr_receiving_yards_rf_predictions, test_data$receiving_yards), digits = 3)
wr_receiving_yards_rf_performance


# Define the tuning grid
tune_grid <- expand.grid(mtry = c(1, 2, 3, 4, 5), # Experiment with different mtry values
                         splitrule = "variance", 
                         min.node.size = c(5, 10, 15, 20, 25, 50, 100))

# Train the model with caret
wr_receiving_yards_rf_tuned <- train(
  receiving_yards ~  
    spread + total_line + dome + #temp + wind + # game data
    rec_game + rec_yards_game + rec_td_game + # receiving_yards stats
    yprr + # efficiency
    grades_pass_route + # wr grades
    def_pass_epa, # pass defense
  data = train_data,
  method = "ranger",
  trControl = trainControl(method = "cv", number = 5), # 5-fold cross-validation
  tuneGrid = tune_grid
)

# View the best model
print(wr_receiving_yards_rf_tuned$bestTune)

# Dotchart of variable importance as measured by a Random Forest
varImpPlot(wr_receiving_yards_rf)

# save model
save(wr_receiving_yards_rf, file = "./04_models/wr/wr_receiving_yards_rf.Rdata")


# 6.3 random forest model and tuning-------------------------------------------


# train random forest model
wr_pass_touchdown_rf <- randomForest(pass_touchdown ~  
                                        spread + total_line + dome + #temp + wind + # game data
                                        rec_game + rec_yards_game + rec_td_game + # pass_touchdown stats
                                        yprr + # efficiency
                                        grades_pass_route + # wr grades
                                        def_pass_epa, # pass defense
                                      data = train_data, 
                                      mtry = 2, 
                                      nodesize = 5,
                                      ntree = 1000)  

# use wr_pass_touchdown_rf to predict on test data
wr_pass_touchdown_rf_predictions <- round(predict(wr_pass_touchdown_rf, test_data), digits = 2)

# evaulated predictions 
wr_pass_touchdown_rf_performance <- round(postResample(wr_pass_touchdown_rf_predictions, test_data$pass_touchdown), digits = 3)
wr_pass_touchdown_rf_performance


# Define the tuning grid
tune_grid <- expand.grid(mtry = c(1, 2, 3, 4, 5), # Experiment with different mtry values
                         splitrule = "variance", 
                         min.node.size = c(5, 10, 15, 20, 25, 50, 100))

# Train the model with caret
wr_pass_touchdown_rf_tuned <- train(
  pass_touchdown ~  
    spread + total_line + dome + #temp + wind + # game data
    rec_game + rec_yards_game + rec_td_game + # pass_touchdown stats
    yprr + # efficiency
    grades_pass_route + # wr grades
    def_pass_epa, # pass defense
  data = train_data,
  method = "ranger",
  trControl = trainControl(method = "cv", number = 5), # 5-fold cross-validation
  tuneGrid = tune_grid
)

# View the best model
print(wr_pass_touchdown_rf_tuned$bestTune)

# Dotchart of variable importance as measured by a Random Forest
varImpPlot(wr_pass_touchdown_rf)

# save model
save(wr_pass_touchdown_rf, file = "./04_models/wr/wr_pass_touchdown_rf.Rdata")

