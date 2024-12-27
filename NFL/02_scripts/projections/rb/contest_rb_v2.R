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


# 1.0 load files -------------------------------------------------------------


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

# calc 0.5ppr rb fpts by game week from since 2022
rb_fpts_pbp <- function(){
  # Get rushing stats
  rb_pbp <- pbp %>% 
    group_by(game_id, rusher, rusher_id) %>% 
    summarise(
      
      rush_attempt = sum(rush_attempt, na.rm = T),
      rushing_yards = sum(rushing_yards, na.rm = T),
      rush_touchdown = sum(rush_touchdown, na.rm = T), 
      fumble = sum(fumble, na.rm = T),
      
      # use to get last non-missing values
      season_type = last(season_type), 
      temp = last(temp, na_rm = F), 
      wind = last(wind, na_rm = F), 
      weather = last(ifelse(is.na(weather), "NA", substr(weather, 1, 4))), # Handle substr and NA
      spread_line = last(spread_line), 
      total_line = last(total_line), 
      posteam = last(posteam), 
      
      week = last(week), 
      season = last(season),
      
      posteam = last(posteam), 
      defteam = last(defteam)
      
      ) %>% 
    drop_na(rusher) %>% 
    ungroup() %>% 
    mutate(join = paste(season, week, rusher_id, sep = "_"))
  
  # Get receiving stats
  wr_pbp <- pbp %>% 
    group_by(game_id, receiver, receiver_id, posteam, defteam) %>% 
    summarize(
      fumble = sum(fumble, na.rm = T), 
      
      receptions = sum(complete_pass, na.rm = T),
      receiving_yards = sum(receiving_yards, na.rm = T), 
      rec_touchdown = sum(pass_touchdown, na.rm = T), 
      
      week = last(week), 
      season = last(season)) %>% 
    drop_na(receiver) %>% 
    ungroup() %>% 
    mutate(join = paste(season, week, receiver_id, sep = "_"))
  
  # join stats and calc fpts
  rb_fpts <<- rb_pbp %>% 
    left_join(wr_pbp %>% select(receptions, receiving_yards, rec_touchdown, join), 
              by=c("join")) %>% 
    replace(is.na(.),0) %>% 
    mutate(
      #big_rush = ifelse(rushing_yards > 100, 1,0), 
      #big_rec = ifelse(receiving_yards > 100, 1,0), 
      fpts = 
        #big_rush * 3 +
        #big_rec * 3
        rushing_yards * .1 +
        rush_touchdown * 6 +
        fumble * -1 +
        
        receptions * 0.5 +
        rec_touchdown * 6 +
        receiving_yards * .1,  
      fpts_ntile = ntile(fpts, 100)) %>% 
    arrange(-fpts) %>% 
    mutate(join = tolower(paste(season, week, posteam, rusher, sep = "_"))) %>% 
    
    left_join(schedule %>% select(game_id, roof), by=c("game_id")) %>% 
    mutate(temp = if_else(roof == "closed" | roof == "dome", 70, temp), 
           wind = if_else(is.na(wind), 0, wind), 
           weather_check = if_else(temp == 0 & wind == 0, 0, 1), 
           dome_games = if_else(roof == "dome" | roof == "closed",1,0)) %>% 
    
    relocate(weather_check, .after = wind) %>% 
    relocate(c("fpts", "fpts_ntile", "spread_line", "total_line"), .after = rusher_id) %>% 
    relocate(c("receptions", "receiving_yards"), .after = fumble)
  
  # remove objects
  rm(rb_pbp, wr_pbp)
}
rb_fpts_pbp()


# 3.0 load and join all contests ------------------------------------------


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
  running_back <- function(){
    
    # load depth chart
    depth_charts <- load_depth_charts(seasons = game_year) %>% 
      filter(week == game_week) %>%
       
      filter(position == "RB") %>% 
      filter(depth_position %in% c("RB","HB")) %>% 
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
    
    # load and process pff rushing summary
    pff_rush <- function() {
      # load pff rushing data
      rushing_summary <- read.csv(glue("{folder}/pff/rushing_summary.csv")) %>% 
        select(player, player_id, player_game_count, team_name, 
               total_touches, elu_rush_mtf, elu_recv_mtf,
               touchdowns, 
               attempts, yco_attempt, ypa, grades_run, explosive,
               receptions, rec_yards, targets, yprr) %>% 
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
    
    print(paste(x, ": Rushing Summary"))
    
    # join data
    rb <<- depth_charts %>%
      
      left_join(rushing_summary, by = c('full_name' = 'player')) %>% 
      mutate(full_name = str_to_title(full_name)) %>% 
      
      left_join(pbp_def, by = c('opp' = 'defteam')) %>% 
      left_join(pbp_off, by = c('club_code' = 'posteam')) %>%
      left_join(def_table, by = c('opp' = 'team_name')) %>% 
      
      mutate(touches_game = round(total_touches / player_game_count, digits = 1), 
             yco_attempt_sd = round((yco_attempt - mean(yco_attempt, na.rm=T)) / sd(yco_attempt, na.rm = T), digits = 2), 
             touches_game_sd = round((touches_game - mean(touches_game, na.rm=T)) / sd(touches_game, na.rm = T), digits = 2),
             
             off_def = (def_rush_epa_rank+rdef_rank)/2 - off_rush_epa_rank, 
             
             attempts_game = round(attempts / player_game_count, digits = 1),
             td_game = round(touchdowns / player_game_count, digits = 1),
             yards_per_game = round(attempts_game * ypa, digits = 1), 
             
             receptions_game = round(receptions / player_game_count, digits = 1),
             receptions_total = receptions, 
             
             targets_game = round(targets / player_game_count, digits = 1), 
             rec_yards_game = round(rec_yards / player_game_count, digits = 1),
             
             mtf_touch = (elu_recv_mtf + elu_rush_mtf) / total_touches,
             explosive_rate = round(explosive/attempts, digits = 2),
             
             contest_year = nfl_year, 
             contest_week = game_week,
             contest = x) %>%
      
      select(-c("receptions")) %>% 
      
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

# remove pbp objects
rm(pbp_def, pbp_off)

# 4.0 create data frame ----------------------------------------------------


process_rb_df <- function(){
  # bind to single dataframe and process data
  contests_rb_df <<- bind_rows(contests_rb) %>% 
    
    filter(week != 2) %>% # remove week 2s, bad data
    
    # changing name to pbp format
    separate(full_name, into = c("first_name", "last_name"), sep = " ", extra = "drop") %>% 
    mutate(player = paste0(substr(first_name, 1, 1), ".", last_name), 
           join = tolower(paste(season, week, club_code, player, sep = "_")), 
           name = paste(first_name, last_name)) %>%
    relocate(name, .before = "first_name") %>% 
    select(-c("first_name", "last_name")) %>%
    
    # joining fpts
    left_join(rb_fpts %>% select(join, fpts, fpts_ntile, 
                                 temp, wind, 
                                 rush_attempt, rushing_yards, rush_touchdown, fumble,
                                 receptions, receiving_yards, rec_touchdown), by=c("join")) %>%
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
      cov_sd = round((cov - mean(cov)) / sd(cov, na.rm = T), digits = 2),
      yco_attempt_sd = round((yco_attempt - mean(yco_attempt, na.rm=T)) / sd(yco_attempt, na.rm = T), digits = 2), 
      touches_game_sd = round((touches_game - mean(touches_game, na.rm=T)) / sd(touches_game, na.rm = T), digits = 2), 
      
      inj_join = paste(contest_year, contest_week, club_code, name, sep = "_")) %>% 
    
    left_join(inj %>% select(inj_join, report_status, practice_status), by=c("inj_join")) %>% 
    
    # add inj status as factors
    mutate(column = as.factor(report_status)) %>%
    mutate(id = row_number()) %>%  # create a temporary id column for reshaping
    pivot_wider(names_from = report_status, 
                values_from = report_status,
                names_prefix = "status_", 
                values_fill = 0,
                values_fn = function(x) 1) %>%
    mutate(depth_team = as.numeric(depth_team)) %>% 
    select(-id) %>% 
    
    filter(attempts > 10) %>% 
    filter(status_Out == 0) %>% 
    
    relocate(c("z_score", "fpts", "fpts_ntile", "rushing_yards", "rush_attempt", "rush_touchdown"), .after = game_id) %>% 
    relocate(c("off_rush_epa_sd", "def_rush_epa_sd", "rdef_sd", "yco_attempt_sd", "tack_sd", "touches_game_sd"), 
             .after = home) %>% 
    
    arrange(-fpts) %>% 
    filter(fpts != 0)
}
process_rb_df()

names(contests_rb_df)


# 5.0 find correlated variables-----------------------------------------------

# select only numeric columns
numeric_contest_rb <<- contests_rb_df[, sapply(contests_rb_df, is.numeric)]

# use to look for features of new models
correlation_table <- function() {

  # select only numeric columns
  numeric_contest_rb <<- contests_rb_df[, sapply(contests_rb_df, is.numeric)]
  
  # find cor of all variables
  cor_df <- as_tibble(cor(numeric_contest_rb)[,"fpts"])

  ## gpt built code
  # Compute the correlation matrix for numeric variables
  cor_matrix <- cor(numeric_contest_rb, use = "complete.obs")
  
  # Convert the correlation matrix to a dataframe
  cor_df <- as.data.frame(cor_matrix)
  
  # Optional: Add a column for row names (variable names) for reference
  cor_df <- tibble::rownames_to_column(cor_df, var = "Variable")
  
  # If you only want correlations with 'fpts' column
  cor_fpts_df <<- cor_df %>% select(Variable, fpts) %>% arrange(-fpts)
}
#correlation_table()


# 6.0 split train test ----------------------------------------------------

model_data <- numeric_contest_rb %>% 
  filter(depth_team == 1 | depth_team == 2)

set.seed(10)

# split data
split_index <- createDataPartition(model_data$fpts, 
                                   p = 0.75, 
                                   list = F, 
                                   times = 1)

train_data <- model_data[split_index, ]
test_data <- model_data[-split_index, ]


# 6.1.0 random forest model and tuning for fpts-------------------------------------------

# variables to add / sub / store
# 

# train random forest model
rb_fpts_rf <- randomForest(fpts ~  
                             spread + total_line + home + #temp + wind + # game data
                             attempts_game + ypa + td_game + rush_share + # rush usage
                             targets_game + yprr + # rec usage
                             grades_run + mtf_touch + # player grade
                             def_rush_epa, # defense
                   data = train_data, 
                   mtry = 1, 
                   nodesize = 15,
                   ntree = 1000)

# use rb_fpts_rf to predict on test data
rb_fpts_rf_predictions <- round(predict(rb_fpts_rf, test_data), digits = 2)

# evaulated predictions 
rb_fpts_rf_performance <- round(postResample(rb_fpts_rf_predictions, test_data$fpts), digits = 3)
rb_fpts_rf_performance


# Define the tuning grid
tune_grid <- expand.grid(mtry = c(1, 2, 3, 4, 5), # Experiment with different mtry values
                         splitrule = "variance", 
                         min.node.size = c(5, 10, 15, 20, 25, 50, 100))

# Train the model with caret
rb_fpts_rf_tuned <- train(
  fpts ~  
    spread + total_line + #temp + wind + # game data
    attempts_game + ypa + td_game + rush_share + # rush usage
    targets_game + yprr + # rec usage
    grades_run + mtf_touch + # player grade
    def_rush_epa,
  data = train_data,
  method = "ranger",
  trControl = trainControl(method = "cv", number = 5), # 5-fold cross-validation
  tuneGrid = tune_grid
)

# View the best model
print(rb_fpts_rf_tuned$bestTune)

# Dotchart of variable importance as measured by a Random Forest
#varImpPlot(rb_fpts_rf)

# save model
save(rb_fpts_rf, file = "./04_models/rb/rb_fpts_rf.Rdata")


# 6.1.1 random forest model and tuning for rushing yards ----------------------

# variables to add / sub / store
# 

# train random forest model
rb_rushing_yards_rf <- randomForest(rushing_yards ~  
                             attempts_game + ypa + td_game + rush_share + # rush usage
                             targets_game + yprr + # rec usage
                             grades_run + mtf_touch + # player grade
                             def_rush_epa, 
                             data = train_data, 
                           mtry = 2, 
                           nodesize = 50,
                           ntree = 1000)

# use rb_rushing_yards_rf to predict on test data
rb_rushing_yards_rf_predictions <- round(predict(rb_rushing_yards_rf, test_data), digits = 2)

# evaulated predictions 
rb_rushing_yards_rf_performance <- round(postResample(rb_rushing_yards_rf_predictions, test_data$rushing_yards), digits = 3)
rb_rushing_yards_rf_performance


# Define the tuning grid
tune_grid <- expand.grid(mtry = c(1, 2, 3, 4, 5), # Experiment with different mtry values
                         splitrule = "variance", 
                         min.node.size = c(5, 10, 15, 20, 25, 50, 100))

# Train the model with caret
rb_rushing_yards_rf_tuned <- train(
  rushing_yards ~  
    spread + total_line + #temp + wind + # game data
    attempts_game + ypa + td_game + rush_share + # rush usage
    targets_game + yprr + # rec usage
    grades_run + mtf_touch + # player grade
    def_rush_epa,
  data = train_data,
  method = "ranger",
  trControl = trainControl(method = "cv", number = 5), # 5-fold cross-validation
  tuneGrid = tune_grid
)

# View the best model
print(rb_rushing_yards_rf_tuned$bestTune)

# Dotchart of variable importance as measured by a Random Forest
#varImpPlot(rb_rushing_yards_rf)

# save model
save(rb_rushing_yards_rf, file = "./04_models/rb/rb_rushing_yards_rf.Rdata")


# 6.1.2 random forest model and tuning for rush_attempts ----------------------

# variables to add / sub / store
# 

# train random forest model
rb_rush_attempt_rf <- randomForest(rush_attempt ~  
                                      spread + total_line + home + #temp + wind + # game data
                                      attempts_game + ypa + td_game + rush_share + # rush usage
                                      targets_game + yprr + # rec usage
                                      grades_run + mtf_touch + # player grade
                                      def_rush_epa, # defense
                                    data = train_data, 
                                    mtry = 4, 
                                    nodesize = 100,
                                    ntree = 1000)

# use rb_rush_attempt_rf to predict on test data
rb_rush_attempt_rf_predictions <- round(predict(rb_rush_attempt_rf, test_data), digits = 2)

# evaulated predictions 
rb_rush_attempt_rf_performance <- round(postResample(rb_rush_attempt_rf_predictions, test_data$rush_attempt), digits = 3)
rb_rush_attempt_rf_performance

# Define the tuning grid
tune_grid <- expand.grid(mtry = c(1, 2, 3, 4, 5), # Experiment with different mtry values
                         splitrule = "variance", 
                         min.node.size = c(5, 10, 15, 20, 25, 50, 100))

# Train the model with caret
rb_rush_attempt_rf_tuned <- train(
  rush_attempt ~  
    spread + total_line + #temp + wind + # game data
    attempts_game + ypa + td_game + rush_share + # rush usage
    targets_game + yprr + # rec usage
    grades_run + mtf_touch + # player grade
    def_rush_epa,
  data = train_data,
  method = "ranger",
  trControl = trainControl(method = "cv", number = 5), # 5-fold cross-validation
  tuneGrid = tune_grid
)

# View the best model
print(rb_rush_attempt_rf_tuned$bestTune)

# Dotchart of variable importance as measured by a Random Forest
#varImpPlot(rb_rush_attempt_rf)

# save model
save(rb_rush_attempt_rf, file = "./04_models/rb/rb_rush_attempt_rf.Rdata")

# 6.1.3 random forest model and tuning for rush td ----------------------

# variables to add / sub / store
# 

# train random forest model
rb_rush_touchdown_rf <- randomForest(rush_touchdown ~  
                                     spread + total_line + home + #temp + wind + # game data
                                     attempts_game + ypa + td_game + rush_share + # rush usage
                                     targets_game + yprr + # rec usage
                                     grades_run + mtf_touch + # player grade
                                     def_rush_epa, # defense
                                   data = train_data, 
                                   mtry = 1, 
                                   nodesize = 50,
                                   ntree = 1000)

# use rb_rush_touchdown_rf to predict on test data
rb_rush_touchdown_rf_predictions <- round(predict(rb_rush_touchdown_rf, test_data), digits = 2)

# evaulated predictions 
rb_rush_touchdown_rf_performance <- round(postResample(rb_rush_touchdown_rf_predictions, test_data$rush_touchdown), digits = 3)
rb_rush_touchdown_rf_performance

# Define the tuning grid
tune_grid <- expand.grid(mtry = c(1, 2, 3, 4, 5), # Experiment with different mtry values
                         splitrule = "variance", 
                         min.node.size = c(5, 10, 15, 20, 25, 50, 100))

# Train the model with caret
rb_rush_touchdown_rf_tuned <- train(
  rush_touchdown ~  
    spread + total_line + #temp + wind + # game data
    attempts_game + ypa + td_game + rush_share + # rush usage
    targets_game + yprr + # rec usage
    grades_run + mtf_touch + # player grade
    def_rush_epa,
  data = train_data,
  method = "ranger",
  trControl = trainControl(method = "cv", number = 5), # 5-fold cross-validation
  tuneGrid = tune_grid
)

# View the best model
print(rb_rush_touchdown_rf_tuned$bestTune)

# Dotchart of variable importance as measured by a Random Forest
#varImpPlot(rb_rush_touchdown_rf)

# save model
save(rb_rush_touchdown_rf, file = "./04_models/rb/rb_rush_touchdown_rf.Rdata")

# expiremental code blocks for models


# 6.1.4 random forest model and tuning for receptions ----------------------


# train random forest model
rb_receptions_rf <- randomForest(receptions ~  
                                       spread + total_line + home + #temp + wind + # game data
                                       attempts_game + ypa + td_game + rush_share + # rush usage
                                       receptions_game + yprr + # rec usage
                                       grades_run + mtf_touch + # player grade
                                       def_rush_epa, # defense
                                     data = train_data, 
                                     mtry = 2, 
                                     nodesize = 10,
                                     ntree = 1000)

# use rb_receptions_rf to predict on test data
rb_receptions_rf_predictions <- round(predict(rb_receptions_rf, test_data), digits = 2)

# evaulated predictions 
rb_receptions_rf_performance <- round(postResample(rb_receptions_rf_predictions, test_data$receptions), digits = 3)
rb_receptions_rf_performance

# Define the tuning grid
tune_grid <- expand.grid(mtry = c(1, 2, 3, 4, 5), # Experiment with different mtry values
                         splitrule = "variance", 
                         min.node.size = c(5, 10, 15, 20, 25, 50, 100))

# Train the model with caret
rb_receptions_rf_tuned <- train(
  receptions ~  
    spread + total_line + #temp + wind + # game data
    attempts_game + ypa + td_game + rush_share + # rush usage
    receptions_game + yprr + # rec usage
    grades_run + mtf_touch + # player grade
    def_rush_epa,
  data = train_data,
  method = "ranger",
  trControl = trainControl(method = "cv", number = 5), # 5-fold cross-validation
  tuneGrid = tune_grid
)

# View the best model
print(rb_receptions_rf_tuned$bestTune)

# Dotchart of variable importance as measured by a Random Forest
#varImpPlot(rb_receptions_rf)

# save model
save(rb_receptions_rf, file = "./04_models/rb/rb_receptions_rf.Rdata")

# 6.1.5 random forest model and tuning for receiving yards ----------------------

# variables to add / sub / store
# 

# train random forest model
rb_receiving_yards_rf <- randomForest(receiving_yards ~  
                                   spread + total_line + home + #temp + wind + # game data
                                   attempts_game + ypa + td_game + rush_share + # rush usage
                                   receptions_game + yprr + # rec usage
                                   grades_run + mtf_touch + # player grade
                                   def_rush_epa, # defense
                                 data = train_data, 
                                 mtry = 3, 
                                 nodesize = 100,
                                 ntree = 1000)

# use rb_receiving_yards_rf to predict on test data
rb_receiving_yards_rf_predictions <- round(predict(rb_receiving_yards_rf, test_data), digits = 2)

# evaulated predictions 
rb_receiving_yards_rf_performance <- round(postResample(rb_receiving_yards_rf_predictions, test_data$receiving_yards), digits = 3)
rb_receiving_yards_rf_performance

# Define the tuning grid
tune_grid <- expand.grid(mtry = c(1, 2, 3, 4, 5), # Experiment with different mtry values
                         splitrule = "variance", 
                         min.node.size = c(5, 10, 15, 20, 25, 50, 100))

# Train the model with caret
rb_receiving_yards_rf_tuned <- train(
  receiving_yards ~  
    spread + total_line + #temp + wind + # game data
    attempts_game + ypa + td_game + rush_share + # rush usage
    receptions_game + yprr + # rec usage
    grades_run + mtf_touch + # player grade
    def_rush_epa,
  data = train_data,
  method = "ranger",
  trControl = trainControl(method = "cv", number = 5), # 5-fold cross-validation
  tuneGrid = tune_grid
)

# View the best model
print(rb_receiving_yards_rf_tuned$bestTune)

# Dotchart of variable importance as measured by a Random Forest
#varImpPlot(rb_receiving_yards_rf)

# save model
save(rb_receiving_yards_rf, file = "./04_models/rb/rb_receiving_yards_rf.Rdata")

# 7.0 other model code dump -----------------------------------------------


other_models <- function() {
# 6.2 gradient boosting ---------------------------------------------------


# Load necessary libraries
library(xgboost)
library(caret)
library(data.table)

# Load your dataset
# Replace 'nfl_data.csv' with your dataset's file path
df <- numeric_contest_rb %>% 
  filter(depth_team == 1 | depth_team == 2)

# Define predictors and target variables
predictors <- c("spread" , "total_line" , "home" , #temp , wind , # game data
                  "attempts_game" , "ypa" , "td_game" , "rush_share" , # rush usage
                  "targets_game" , 'yprr' , # rec usage
                  'grades_run' , "mtf_touch" , # player grade
                  'def_rush_epa')  # Replace with actual predictors
targets <- c("attempts", "rushing_yards", "rush_touchdown")  # Target variables

# Split the data into training and testing sets
set.seed(42)
train_index <- createDataPartition(df[[targets[1]]], p = 0.8, list = FALSE)
train_data <- df[train_index, ]
test_data <- df[-train_index, ]

# Convert predictors to matrix format
X_train <- as.matrix(train_data[, predictors])
X_test <- as.matrix(test_data[, predictors])

# Initialize a list to store models and predictions
models <- list()
predictions <- list()
rmse_values <- c()

# Loop through each target variable
for (target in targets) {
  # Define the target variable
  y_train <- train_data[[target]]
  y_test <- test_data[[target]]
  
  # Create DMatrix for xgboost
  dtrain <- xgb.DMatrix(data = X_train, label = y_train)
  dtest <- xgb.DMatrix(data = X_test, label = y_test)
  
  # Set xgboost parameters
  params <- list(
    objective = "reg:squarederror",  # Regression task
    eta = 0.1,                      # Learning rate
    max_depth = 6,                  # Maximum depth of a tree
    subsample = 0.8,                # Subsample ratio of the training data
    colsample_bytree = 0.8          # Subsample ratio of columns when constructing each tree
  )
  
  # Train the xgboost model
  rb_fpts_xgb <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = 100,
    watchlist = list(train = dtrain, test = dtest),
    early_stopping_rounds = 10,
    verbose = 1
  )
  
  # Save the trained rb_fpts_xgb
  models[[target]] <- rb_fpts_xgb
  
  # Predict on test data
  pred <- predict(rb_fpts_xgb, newdata = dtest)
  predictions[[target]] <- pred
  
  # Calculate RMSE for the target variable
  rmse <- sqrt(mean((pred - y_test)^2))
  rmse_values <- c(rmse_values, rmse)
  
  cat("RMSE for", target, ":", rmse, "\n")
}

# Combine predictions into a data frame
predicted_df <- data.frame(
  rush_attempt_pred = predictions[["attempts"]],
  rushing_yards_pred = predictions[["rushing_yards"]],
  rush_touchdown_pred = predictions[["rush_touchdown"]]
)

# Add actual values for comparison
actual_df <- test_data[, targets]

# Print RMSE values for each target
cat("Overall RMSEs:\n")
print(data.frame(Target = targets, RMSE = rmse_values))

# View predictions vs actuals (first 10 rows)
head(data.frame(Actual = actual_df, Predicted = predicted_df), 10)

library(caret)

# Example grid search for hyperparameter tuning
tune_grid <- expand.grid(
  nrounds = c(50, 100, 200),
  eta = c(0.01, 0.1, 0.3),
  max_depth = c(4, 6, 8),
  colsample_bytree = c(0.7, 0.8, 1),
  subsample = c(0.7, 0.8, 1),
  min_child_weight = 1,
  gamma = 0
)

train_control <- trainControl(method = "cv", number = 5)
xgb_tuned <- train(
  X_train, y_train,
  method = "xgbTree",
  trControl = train_control,
  tuneGrid = tune_grid
)

save(rb_fpts_xgb, file = "./04_models/rb_fpts_xgb.Rdata")

# 6.3 tensorflow with python------------------------------------------------


library(keras)
library(tensorflow)
library(reticulate)

use_virtualenv("r-reticulate")  # Ensure correct virtual environment is used
tensorflow::tf$constant("Hello, TensorFlow!")

# Prepare data
# Assume 'train_data' is your training dataset
x_train <- as.matrix(train_data[, c("spread", "total_line", "attempts_game", 
                                    "yco_attempt_sd", "targets_game", "yprr", 
                                    "grades_run", "def_rush_epa")]) # 10 variables
y_train <- as.matrix(train_data[, c("rush_attempt", "rushing_yards", 
                                    "rush_touchdown", "receptions", 
                                    "receiving_yards", "rec_touchdown", "fpts")])

# Define the model
model <- keras_model_sequential() %>%
  layer_dense(units = 128, activation = 'relu', input_shape = ncol(x_train)) %>%
  layer_dropout(rate = 0.3) %>% # Prevent overfitting
  layer_dense(units = 64, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = ncol(y_train), activation = 'linear') # Multivariate output

# Compile the model
model %>% compile(
  optimizer = optimizer_adam(learning_rate = 0.001),
  loss = 'mean_squared_error',
  metrics = c('mean_absolute_error')
)

# Train the model
history <- model %>% fit(
  x_train, y_train,
  epochs = 100, # Adjust epochs for convergence
  batch_size = 32,
  validation_split = 0.2, # Use 20% of the data for validation
  verbose = 2
)

# Evaluate the model
x_test <- as.matrix(test_data[, c("spread", "total_line", "attempts_game", 
                                  "yco_attempt_sd", "targets_game", "yprr", 
                                  "grades_run", "def_rush_epa")])
y_test <- as.matrix(test_data[, c("rush_attempt", "rushing_yards", 
                                  "rush_touchdown", "receptions", 
                                  "receiving_yards", "rec_touchdown", "fpts")])

evaluation <- model %>% evaluate(x_test, y_test)
cat("Test Loss:", evaluation$loss, "\nTest MAE:", evaluation$mean_absolute_error)

# Make predictions
predictions <- model %>% predict(x_test)

# Combine predictions and actuals for analysis
results <- data.frame(
  actuals = y_test,
  predictions = predictions
)



# 6.3.1 benchmarking ------------------------------------------------------

library(xgboost)

# Convert data to DMatrix format
train_matrix <- xgb.DMatrix(data = as.matrix(train_data[, predictors]),
                            label = train_data[, target])

# Train with GPU support
xgb_model <- xgb.train(
  data = train_matrix,
  nrounds = 100,
  max_depth = 20,
  eta = 0.1,
  objective = "reg:squarederror",
  tree_method = "gpu_hist"  # Enable GPU support
)



library(h2o)

# Initialize H2O cluster
h2o.init()

# Convert train_data to H2O frame
train_h2o <- as.h2o(train_data)

# Define predictors and target
predictors <- colnames(train_data)[1:(ncol(train_data) - 1)]  # All except last column
target <- colnames(train_data)[ncol(train_data)]             # Last column is target

# Train a random forest model on CPU
system.time({
  rf_cpu <- h2o.randomForest(
    x = predictors, 
    y = target,
    training_frame = train_h2o,
    ntrees = 100,
    max_depth = 20
  )
})

# View model summary
summary(rf_cpu)

# Shutdown H2O cluster
h2o.shutdown(prompt = FALSE)


# Compare training times


# 6.3,2 multivariate random forest model ------------------------------------

# load cpu / gpu packages

# processing time packages
library(doParallel) # Foreach parallel adaptor for parallel package
library(h2o) # for cpu clustering
library(foreach) # looping construct


# Prepare the dataset
# Assume train_data is your training dataset and test_data is your test dataset
# Replace target column names with actual column names in your dataset
X_train <- as.matrix(train_data[, c("spread", "total_line", 
                                    "attempts_game", "yco_attempt_sd", 
                                    "targets_game", "yprr", 
                                    "grades_run", 
                                    "def_rush_epa")])

Y_train <- as.matrix(train_data[, c("rush_attempt","rushing_yards", "rush_touchdown", 
                                    "receptions", "receiving_yards", "rec_touchdown",
                                    "fpts")])

X_test <- as.matrix(test_data[, c("spread", "total_line", 
                                  "attempts_game", "yco_attempt_sd", 
                                  "targets_game", "yprr", 
                                  "grades_run", 
                                  "def_rush_epa")])

# Train the multivariate random forest model
set.seed(10) # For reproducibility



# set up parallel backend

num_cores <- detectCores() - 4
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Define total number of trees and chunk size per core
total_trees <- 100
trees_per_core <- floor(total_trees / num_cores)  # Ensure integer

# Ensure that total_trees is fully distributed among cores
# Handle any remainder trees (if total_trees %% num_cores != 0)
tree_remainder <- total_trees %% num_cores
ntree_list <- c(rep(trees_per_core, num_cores))
if (tree_remainder > 0) {
  ntree_list[1:tree_remainder] <- ntree_list[1:tree_remainder] + 1
}

# Train Multivariate Random Forest in parallel
forest_chunks <- foreach(ntree = ntree_list, 
                         .combine = rbind, # list or rbind
                         .packages = "MultivariateRandomForest") %dopar% {
                           build_forest_predict(
                             trainX = X_train,
                             trainY = Y_train,
                             testX = X_test,
                             n_tree = ntree,      # Subset of trees
                             m_feature = 2,       # Features per split
                             min_leaf = 5         # Minimum samples per leaf
                           )
                         }

# Stop the parallel backend
stopCluster(cl)

final_forest <- forest_chunks
test <- do.call(rbind, forest_chunks)

# Predict using each chunk
predictions_list <- lapply(forest_chunks, function(chunk) {
  # Assuming `chunk` is a trained forest object
  # Replace trainX and trainY with placeholders, if required
  build_forest_predict(
    trainX = NULL,       # No new training
    trainY = NULL,       # No new training
    testX = X_test,      # Test data for prediction
    n_tree = length(chunk),  # Use the number of trees in the chunk
    m_feature = 2,       # Match the training feature settings
    min_leaf = 5         # Match the training settings
  )
})

# Combine predictions (e.g., by averaging)
final_predictions <- Reduce("+", predictions_list) / length(predictions_list)

# View predictions
head(final_predictions)


library(Metrics)

target_columns <- c("rush_attempt","rushing_yards", "rush_touchdown", 
                    "receptions", "receiving_yards", "rec_touchdown",
                    "fpts") 
Y_test <- as.matrix(test_data[, target_columns])


# Assuming `final_predictions` contains predictions for test data
actual <- Y_test  # Actual target values

final_predictions <- forest_chunks

evaluation <- data.frame(
  Target = colnames(actual),
  R_squared = sapply(1:ncol(actual), function(i) {
    cor(final_predictions[, i], actual[, i])^2  # R² for each target
  }),
  RMSE = sapply(1:ncol(actual), function(i) {
    rmse(actual[, i], final_predictions[, i])  # RMSE for each target
  })
)

print(evaluation)


# Train Multivariate Random Forest model
mrf_model <- build_forest_predict(
  trainX = X_train,  # Predictor matrix
  trainY = Y_train,  # Target matrix
  testX = X_test,
  n_tree = 10, # Number of trees
  m_feature = 2, # Number of features at each split
  min_leaf = 5   # Minimum number of samples per leaf
)

# Predict on test data
Y_pred <- predict_forest(mrf_model, X_test)

# Convert predictions to a data frame for easier interpretation
predictions <- data.frame(
  Rushing_Yards = Y_pred[, 1],
  Rushing_Touchdowns = Y_pred[, 2],
  Fantasy_Points = Y_pred[, 3]
)

# View predictions
print(predictions)


# 6.4 train models - GPT ensemble --------------------------------------------


# list of models to train
models <- c("lm", "glm", # linear models 
            "gbm", # gradient boosting
            "svmRadial", # kernel trick
            "knn", # k nearest neighbors
            "rf") 

models_not_ready <- c("nnet", "rpart")

# set up control parameters
ctrl <- trainControl(method = "cv", 
                     number = 10, 
                     savePredictions = "all")

# Train each model and store the results.
rb_pts_models_v2 <- lapply(models, function(model){
  
  # begin
  print(paste(model, "start"))
  
  set.seed(1)
  
  fit <- train(fpts ~  spread + total_line + # game data
                 attempts_game + targets_game + # usage
                 grades_run + # player grade
                 def_rush_epa, # defense 
               data = train_data, 
               method = model, 
               trControl = ctrl)
  
  # save to list
  return(fit)
  
  # end
  print(paste(model, "end"))
  
})

#Evaluate Models on Test Data:
predictions <- lapply(rb_pts_models_v2, function(fit) predict(fit, test_data))

# get performance metrics
performance <- lapply(predictions, function(pred) postResample(pred, test_data$fpts))

# visualize and compare
performance_df <- as.data.frame(do.call(rbind, performance))

# add model names
performance_df <- cbind(models, performance_df) %>% 
  arrange(-Rsquared)

performance_df





}
