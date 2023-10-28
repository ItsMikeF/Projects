# create a list of all the rb slates

# 0.0 load packages -------------------------------------------------------

#load packages
suppressMessages({
  library(nflfastR) # pbp data
  library(nflreadr) # nfl schedule
  library(tidyverse) # ggplot2 dplyr tibble tidyr purrr forecats 
  library(glue) # interpreted literal strings
  library(caret) # data partition
  library(randomForest) # rf model
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

# load pbp
pbp <- load_pbp(2022:2023)

# calc rb fpts by game week
rb_fpts_pbp <- function(){
  # Get rushing stats
  rb_pbp <- pbp %>% 
    group_by(rusher, rusher_id, posteam, week, season) %>% 
    summarise(
      
      rush_attempt = sum(rush_attempt, na.rm = T),
      rushing_yards = sum(rushing_yards, na.rm = T),
      rush_touchdown = sum(rush_touchdown, na.rm = T),
      
      fumble = sum(fumble, na.rm = T)) %>% 
    drop_na() %>% 
    ungroup() %>% 
    mutate(join = paste(season, week, rusher_id, sep = "_"))
  
  # Get receiving stats
  wr_pbp <- pbp %>% 
    group_by(receiver, receiver_id, posteam, week, season) %>% 
    summarize(
      fumble = sum(fumble, na.rm = T), 
      
      receptions = sum(complete_pass, na.rm = T),
      receiving_yards = sum(receiving_yards, na.rm = T), 
      rec_touchdown = sum(pass_touchdown, na.rm = T)) %>% 
    drop_na() %>% 
    ungroup() %>% 
    mutate(join = paste(season, week, receiver_id, sep = "_"))
  
  # join stats and calc fpts
  rbs_fpts <<- rb_pbp %>% 
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
      fpts_ntile = ntile(fpts, 100)
    ) %>% 
    arrange(-fpts) %>% 
    mutate(join = paste(season, week, posteam, rusher, sep = "_"))
  
  # remove objects
  rm(rb_pbp, wr_pbp)
}
rb_fpts_pbp()

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
odds(2022:2023)

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
    
    # def pass epa
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
      mutate(away_team = alpha) %>% 
      separate(bravo, sep = " ", into = c("charlie", "delta"), extra = "drop") %>% 
      mutate(home_team = charlie, 
             opp = if_else(team == alpha, charlie, alpha), 
             home = if_else(team == home_team, 1, 0)) %>% 
      select(pos, name, salary, team, home, opp, home_team, away_team)
    
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
             contest_year = year, 
             contest_week = game_week,
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
             home,
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
             n_plays.y.y,
             off_rush_epa,
             off_rush_epa_rank,
             n_plays.y.x,
             def_rush_epa,
             def_rush_epa_rank,
             def, 
             rdef, 
             tack, 
             prsh, 
             cov, 
             rdef_rank,
             breakaway_percent,
             elusive_rating,
             tack_rank,
             grades_offense,
             yco_attempt,
             yprr,
             contest_year, 
             contest_week, 
             away_team, 
             home_team,
             join) %>%
      arrange(-sum_sd)
  }
  running_back()

})

# remove objects
rm(pbp_def, pbp_off)

# 4.0 create dataframe ----------------------------------------------------

# bind to single dataframe and process data
contests_rb <- bind_rows(contests_rb) %>% 
  drop_na() %>% 
  
  # changing name to pbp format
  separate(name, into = c("first_name", "last_name"), sep = " ", extra = "drop") %>% 
  mutate(player = paste0(substr(first_name, 1, 1), ".", last_name), 
         join = paste(contest_year, contest_week, team, player, sep = "_"), 
         name = paste(first_name, last_name)) %>%
  relocate(name, .before = "first_name") %>% 
  select(-c("first_name", "last_name")) %>%
  
  # joining fpts
  left_join(rbs_fpts %>% select(join, fpts, fpts_ntile), by=c("join")) %>%
  replace_na(list(fpts = 0, fpts_ntile = 0)) %>%
  
  # joining odds
  mutate(schedule_join = sprintf("%02d", contest_week), 
         schedule_join = paste(contest_year, schedule_join, away_team, home_team, sep = "_")) %>% 
  left_join(odds %>% select(game_id, total_line, away_spread, home_spread), 
            by = c("schedule_join" = "game_id")) %>% 
  mutate(spread = if_else(home == 1, home_spread, away_spread)) %>% 
  relocate(c(spread, total_line), .after = "team") %>% 
  
  # add sd columns
  mutate(def_rush_epa_sd = round((def_rush_epa - weighted.mean(def_rush_epa, n_plays.y.x, na.rm=T)) / sd(def_rush_epa, na.rm = T), digits = 2),
         off_rush_epa_sd = round((off_rush_epa - weighted.mean(off_rush_epa, n_plays.y.y, na.rm=T)) / sd(off_rush_epa, na.rm = T), digits = 2),
         def_sd = round((def - mean(def)) / sd(def, na.rm = T), digits = 2), 
         rdef_sd = round((rdef - mean(rdef)) / sd(rdef, na.rm = T), digits = 2),
         tack_sd = round((tack - mean(tack)) / sd(tack, na.rm = T), digits = 2),
         prsh_sd = round((prsh - mean(prsh)) / sd(prsh, na.rm = T), digits = 2),
         cov_sd = round((cov - mean(cov)) / sd(cov, na.rm = T), digits = 2),
         runBlockAdv_sd = round((runBlockAdv - mean(runBlockAdv, na.rm=T)) / sd(runBlockAdv, na.rm = T), digits = 2), 
         yco_attempt_sd = round((yco_attempt - mean(yco_attempt, na.rm=T)) / sd(yco_attempt, na.rm = T), digits = 2), 
         touches_game_sd = round((touches_game - mean(touches_game, na.rm=T)) / sd(touches_game, na.rm = T), digits = 2), 
         
         sum_sd = round(
           (0.05 * runBlockAdv_sd) +
             (0.20 * off_rush_epa_sd) -
             (0.15 * def_rush_epa_sd) - 
             (0.15 * rdef_sd) + 
             (0.05 * (yco_attempt_sd - tack_sd)) +
             (0.40 * touches_game_sd), 
           digits = 3))

# 5.0 eda -----------------------------------------------------------------

# find individual correlations
cor(contests_rb$sum_sd, 
    contests_rb$fpts)

# select only numeric columns
numeric_contest_rb <- contests_rb[, sapply(contests_rb, is.numeric)]

# find cor of all variables
cor(numeric_contest_rb)[,"fpts"]


# 6.0 split train test ----------------------------------------------------

model_data <- numeric_contest_rb %>% 
  filter(salary > 4000)

set.seed(1)

# split data
split_index <- createDataPartition(model_data$fpts, 
                                   p = 0.75, 
                                   list = F, 
                                   times = 1)

train_data <- model_data[split_index, ]
test_data <- model_data[-split_index, ]


# 6.1 train models - GPT------------------------------------------------------

# list of models to train
models <- c("lm", "rf", "glm", "gbm", "svmRadial", "nnet", "knn", "rpart")

# set up control parameters
ctrl <- trainControl(method = "cv", 
                     number = 10, 
                     savePredictions = "all")

# Train each model and store the results.
results <- list()

for (model in models) {
  
  print(paste(model, "start"))
  
  set.seed(1)
  
  fit <- train(fpts ~ salary + sum_sd + spread + total_line, 
               data = train_data, 
               method = model, 
               trControl = ctrl)
  
  results[[model]] <- fit
  
  print(paste(model, "end"))
  
}

#Evaluate Models on Test Data:
predictions <- lapply(results, function(fit) predict(fit, test_data))

# get performance metrics
performance <- lapply(predictions, function(pred) postResample(pred, test_data$fpts))

# visualize and compare
performance_df <- as.data.frame(do.call(rbind, performance))
performance_df


# 6.2 all variables -------------------------------------------------------

# list of models to train
models <- c("lm", "rf", "glm", "gbm", "svmRadial", "nnet", "knn", "rpart")

# set up control parameters
ctrl <- trainControl(method = "cv", 
                     number = 10, 
                     savePredictions = "all")

# Train each model and store the results.
rb_pts_models <- list()

for (model in models) {
  
  print(paste(model, "start"))
  
  set.seed(1)
  
  fit <- train(fpts ~ salary + spread + total_line + touches_game + runBlockAdv + def_rush_epa + off_rush_epa + rdef + yco_attempt + tack, 
               data = train_data, 
               method = model, 
               trControl = ctrl)
  
  rb_pts_models[[model]] <- fit
  
  print(paste(model, "end"))
  
}

#Evaluate Models on Test Data:
predictions <- lapply(rb_pts_models, function(fit) predict(fit, test_data))

# get performance metrics
performance <- lapply(predictions, function(pred) postResample(pred, test_data$fpts))

# visualize and compare
performance_df <- as.data.frame(do.call(rbind, performance))
performance_df


# 6.3 model selection -----------------------------------------------------

save(rb_pts_models, file = "./04_models/rb_pts_models.RData")

# 7.0 baseline salary model ------------------------------------------------

baseline_model <- lm(fpts ~ salary + spread + total_line, 
                     data = train_data)

baseline_proj <- predict(baseline_model, newdata = test_data)

baseline_results <- data.frame(Actual = test_data$fpts, 
                               Projections = baseline_proj)

baseline_mae <- mean(abs(baseline_results$Actual - baseline_results$Projections))

baseline_rmse <- sqrt(mean((baseline_results$Actual - baseline_results$Projections)^2))

print(paste("Mean Absolute Error: ", round(baseline_mae, 2)))
print(paste("Root Mean Squared Error: ", round(baseline_rmse, 2)))

# 8.0 linear model --------------------------------------------------------

lm1_model <- lm(fpts ~ salary + spread + total_line + touches_game + runBlockAdv + def_rush_epa + off_rush_epa + rdef + yco_attempt + tack, 
               data = train_data)

lm1_proj <- predict(lm1_model, newdata = test_data)

lm1_results <- data.frame(Actual = test_data$fpts, 
                         Projections = lm1_proj)

lm1_mae <- mean(abs(lm1_results$Actual - lm1_results$Projections))

lm1_rmse <- sqrt(mean((lm1_results$Actual - lm1_results$Projections)^2))

# Display results
print(paste("Mean Absolute Error: ", round(lm1_mae, 2)))
print(paste("Root Mean Squared Error: ", round(lm1_rmse, 2)))

# 8.1 linear model 2 ------------------------------------------------------

lm2_model <- lm(fpts ~ salary + spread + total_line + sum_sd , 
                data = train_data)

lm2_proj <- predict(lm2_model, newdata = test_data)

lm2_results <- data.frame(Actual = test_data$fpts, 
                          Projections = lm2_proj)

lm2_mae <- mean(abs(lm2_results$Actual - lm2_results$Projections))

lm2_rmse <- sqrt(mean((lm2_results$Actual - lm2_results$Projections)^2))

# Display results
print(paste("Mean Absolute Error: ", round(lm2_mae, 2)))
print(paste("Root Mean Squared Error: ", round(lm2_rmse, 2)))

# 9.0 random forest model -------------------------------------------------

rf_model <- randomForest(fpts ~ salary + sum_sd + spread + total_line,
                         data = train_data)

rf_model <- randomForest(fpts ~ salary,
                         data = train_data)

rf_proj <- predict(rf_model, 
                   newdata = test_data)

rf_results <- data.frame(Actual = test_data$fpts, 
                         Predicted = rf_proj)

rf_mae <- mean(abs(rf_results$Actual - rf_results$Predicted))

rf_rmse <- sqrt(mean((rf_results$Actual - rf_results$Predicted)^2))

# Display results
print(paste("Mean Absolute Error: ", round(rf_mae, 2)))
print(paste("Root Mean Squared Error: ", round(rf_rmse, 2)))
