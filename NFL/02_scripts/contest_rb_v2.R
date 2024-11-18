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
  library(xgboost) # xgb model
})


# 1.0 ---------------------------------------------------------------------


files <- function(){
  
  # define year
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
files()


# 2.0 load pbp and calc fpts ----------------------------------------------


# load pbp
pbp <- load_pbp(2022:nfl_year)

# calc 0.5ppr rb fpts by game week from since 2022
rb_fpts_pbp <- function(){
  # Get rushing stats
  rb_pbp <- pbp %>% 
    group_by(rusher, rusher_id, posteam, defteam, week, season) %>% 
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
    group_by(receiver, receiver_id, posteam, defteam, week, season) %>% 
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
      fpts_ntile = ntile(fpts, 100)) %>% 
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
           week = as.numeric(week),
           game_id = paste(season, week, away_team, home_team, sep = "_"), 
           away_join = paste(season, week, away_team, sep = "_"), 
           home_join = paste(season, week, home_team, sep = "_")
    ) %>% 
    select(-spread_line)
}
odds(2022:nfl_year)


# 2.2 load injuries -------------------------------------------------------


inj <- load_injuries(2022:nfl_year) %>% 
  mutate(inj_join = paste(season, week, team, full_name, sep = "_"))


# 3.0 load schedule -------------------------------------------------------


# load schedule
schedule <- load_schedules(2022:nfl_year)


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
             defteam = gsub('LA','LAR', defteam), 
             defteam = gsub('LARC','LAC', defteam), 
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
             posteam = gsub('LA','LAR', posteam), 
             posteam = gsub('LARC','LAC', posteam),
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
      filter(depth_position == "RB") %>% 
      select(1:5, 10, 12, 15) %>% 
      mutate(game_id = paste(season, week, club_code, sep = "_")) %>% 
      left_join(odds %>% select(away_team, home_team, home_spread, away_spread, home_join, total_line), by = c("game_id" = "home_join")) %>% 
      left_join(odds %>% select(away_team, home_team, home_spread, away_spread, away_join), by = c("game_id" = "away_join")) %>% 
      mutate(away_team = coalesce(away_team.x, away_team.y),
             home_team = coalesce(home_team.x, home_team.y),
             home_spread = coalesce(home_spread.x, home_spread.y),
             away_spread = coalesce(away_spread.x, away_spread.y)) %>% 
      select(-c(away_team.x, away_team.y, home_team.x, home_team.y, home_spread.x, home_spread.y, away_spread.x, away_spread.y)) %>% 
      drop_na() %>% # to remove bye week teams
      mutate(opp = if_else(club_code == home_team, away_team, home_team), 
             spread = if_else(club_code == home_team, home_spread, away_spread),
             home = if_else(club_code == home_team, 1,0)) %>% 
      select(-c(away_team, home_team, away_spread, home_spread))
    
    print(paste(x, ": Depth Charts"))
    print(depth_charts)
    
    # load pff rushing data
    rushing_summary <- read.csv(glue("{folder}/pff/rushing_summary.csv")) %>% 
      select(player, player_id, player_game_count, total_touches, elu_rush_mtf, 
             attempts, yco_attempt, gap_attempts, zone_attempts, ypa, first_downs, 
             grades_run, targets)
    
    # join data
    rb <- depth_charts %>%
      left_join(rushing_summary, by = c('full_name' = 'player')) %>% 
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

# remove pbp objects
rm(pbp_def, pbp_off)

# 4.0 create dataframe ----------------------------------------------------

# bind to single dataframe and process data
contests_rb_df <- bind_rows(contests_rb) %>% 
  
  # changing name to pbp format
  separate(full_name, into = c("first_name", "last_name"), sep = " ", extra = "drop") %>% 
  mutate(player = paste0(substr(first_name, 1, 1), ".", last_name), 
         join = paste(contest_year, contest_week, club_code, player, sep = "_"), 
         name = paste(first_name, last_name)) %>%
  relocate(name, .before = "first_name") %>% 
  select(-c("first_name", "last_name")) %>%
  
  # joining fpts
  left_join(rbs_fpts %>% select(join, fpts, fpts_ntile), by=c("join")) %>%
  replace_na(list(fpts = 0, fpts_ntile = 0)) %>%
  
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
  select(-id) %>% 
  filter(attempts > 10) %>% 
  filter(status_Out == 0) %>% 
  relocate(z_score, .after = game_id) %>% 
  relocate(fpts, .after = z_score) %>% 
  relocate(join, .after = name) %>% 
  arrange(-z_score)

names(contests_rb_df)
  
# count zeros in fpts column
contests_rb_df %>% summarize(zeros = sum(fpts == 0))

# add average rb fpts up to that game week

# 5.0 eda -----------------------------------------------------------------

contests_rb_df$z_score
contests_rb_df$fpts

# find individual correlations
cor(contests_rb_df$z_score, 
    contests_rb_df$fpts,
    use = "complete.obs")

# select only numeric columns
numeric_contest_rb <- contests_rb_df[, sapply(contests_rb_df, is.numeric)]

# find cor of all variables
cor_df <- as_tibble(cor(numeric_contest_rb)[,"fpts"])
cor_df

## gpt built code
# Compute the correlation matrix for numeric variables
cor_matrix <- cor(numeric_contest_rb, use = "complete.obs")

# Convert the correlation matrix to a dataframe
cor_df <- as.data.frame(cor_matrix)

# Optional: Add a column for row names (variable names) for reference
cor_df <- tibble::rownames_to_column(cor_df, var = "Variable")

# If you only want correlations with 'fpts' column
cor_fpts_df <- cor_df %>% select(Variable, fpts)


# 6.0 split train test ----------------------------------------------------

model_data <- numeric_contest_rb %>% 
  filter(touches_game > 5)

set.seed(123)

# split data
split_index <- createDataPartition(model_data$fpts, 
                                   p = 0.75, 
                                   list = F, 
                                   times = 1)

train_data <- model_data[split_index, ]
test_data <- model_data[-split_index, ]


# 6.1 train models - GPT------------------------------------------------------

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
results <- list()

for (model in models) {
  
  print(paste(model, "start"))
  
  set.seed(1)
  
  fit <- train(fpts ~  sum_sd + spread + total_line + 
                 status_NA + status_Questionable + status_Out, 
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
rb_pts_models_v2 <- lapply(models, function(model){
  
  print(paste(model, "start"))
  
  set.seed(1)
  
  fit <- train(fpts ~ salary + spread + total_line + 
                 attempts_game + targets_game + yco_attempt + 
                 runBlockAdv + off_rush_epa +
                 def_rush_epa +  rdef +  tack +
                 status_NA + status_Questionable + status_Out, 
               data = train_data, 
               method = model, 
               trControl = ctrl)
  
})
# removed temporarily
#status_Doubtful

names(rb_pts_models) <- models

#Evaluate Models on Test Data:
predictions <- lapply(rb_pts_models, function(fit) predict(fit, test_data))

# get performance metrics
performance <- lapply(predictions, function(pred) postResample(pred, test_data$fpts))

# visualize and compare
performance_df <- as.data.frame(do.call(rbind, performance))
performance_df


# 6.3 model selection -----------------------------------------------------

save(rb_pts_models_v2, file = "./04_models/rb_pts_models_v2.RData")

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
