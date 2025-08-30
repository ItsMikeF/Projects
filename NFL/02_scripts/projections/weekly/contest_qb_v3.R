# contest_qb_v3

#0.0 Load Packages -------------------------------------------------------
  #Suppress messages and load required packages
suppressMessages({
  #nflverse packages
  options(nflreadr.verbose = FALSE)
  library(nflfastR)    # For play-by-play data
  library(nflreadr)    # For NFL schedules and other data
  #Data manipulation packages
  library(tidyverse)   # Includes ggplot2, dplyr, tibble, tidyr, purrr, forcats
  library(glue)        # For interpreted literal strings
  #Modeling packages
  library(caret)       # For data partitioning
  library(randomForest) # Random forest model
  library(ranger)      # Fast implementation of random forest
  #Parallel processing
  library(future)
  library(future.apply)
})

#1.0 Define Contest Files Function ---------------------------------------
  files <- function(start_year = 2022, data_path = "./01_data/contests/") {
    data_start <<- start_year
    nfl_year <<- year(Sys.Date()) - 1
    List contest files
    contest_files <- list.files(path = data_path)
    Filter out unwanted files
    contest_files <- contest_files[!grepl("2021", contest_files)]  # Remove 2021 weeks
    contest_files <- contest_files[-which(grepl("2022_w08", contest_files))]  # Remove week 8 2022
    contest_files <- contest_files[!grepl("w01", contest_files)]  # Remove week 1s
    contest_files <- contest_files[!grepl("2024_21", contest_files)]  # Remove wrong folder name
    contest_files <<- contest_files
    rm(indices, envir = .GlobalEnv)  # Clean up if indices exists
  }
files(2022)  # Call with start year

#2.0 Load NFLverse Data --------------------------------------------------
  Load spreads and totals
odds <- function(years) {
  odds <<- load_schedules(years) %>%
    select(season, week, away_team, home_team, spread_line, total_line) %>%
    mutate(
      away_spread = spread_line,
      home_spread = spread_line * -1,
      week = sprintf("%02d", week),
      week = as.numeric(week),
      game_id = paste(season, week, away_team, home_team, sep = ""),
      away_join = paste(season, week, away_team, sep = ""),
      home_join = paste(season, week, home_team, sep = "_")
    ) %>%
    select(-spread_line)
}
odds(2022:nfl_year)
Load injuries
inj <- load_injuries(2022:nfl_year) %>%
  mutate(inj_join = paste(season, week, team, full_name, sep = "_"))
Load schedule
schedule <- load_schedules(2022:nfl_year)
2.1 Load PBP and Calculate FPTS -----------------------------------------
  Load play-by-play data once
pbp <- load_pbp(data_start:nfl_year) %>%
  mutate(weather = as.character(weather))
Calculate QB FPTS from PBP
qb_fpts_pbp <- function() {
  QB passing stats
  qb_pbp <- pbp %>%
    group_by(game_id, passer, passer_id, posteam) %>%
    summarize(
      epa = round(sum(qb_epa, na.rm = TRUE), digits = 2),
      snaps = n(),
      epa_per_play = round(epa / snaps, digits = 2),
      pass_attempt = sum(pass_attempt, na.rm = TRUE),
      passing_yards = sum(passing_yards, na.rm = TRUE),
      pass_touchdown = sum(pass_touchdown, na.rm = TRUE),
      interception = sum(interception, na.rm = TRUE),
      qb_scramble = sum(qb_scramble, na.rm = TRUE),
      rush_attempt = sum(rush_attempt, na.rm = TRUE),
      rushing_yards = sum(rushing_yards, na.rm = TRUE),
      rush_touchdown = sum(rush_touchdown, na.rm = TRUE),
      fumble = sum(fumble, na.rm = TRUE),
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
  Rusher stats
  rusher_pbp <- pbp %>%
    group_by(game_id, rusher, rusher_id) %>%
    summarise(
      rush_attempt = sum(rush_attempt, na.rm = TRUE),
      rushing_yards = sum(rushing_yards, na.rm = TRUE),
      rush_touchdown = sum(rush_touchdown, na.rm = TRUE),
      fumble = sum(fumble, na.rm = TRUE)
    ) %>%
    drop_na() %>%
    ungroup() %>%
    mutate(join = paste(game_id, rusher_id, sep = "_")) %>%
    select(-game_id)
  Join and calculate FPTS (DK scoring)
  qb_fpts <<- qb_pbp %>%
    left_join(rusher_pbp, by = "join") %>%
    mutate(
      across(c(rush_attempt.x, rush_attempt.y, rushing_yards.x, rushing_yards.y,
               rush_touchdown.x, rush_touchdown.y, fumble.x, fumble.y),
             ~ replace_na(., 0)),
      rush_attempt = rush_attempt.x + rush_attempt.y,
      rushing_yards = rushing_yards.x + rushing_yards.y,
      rush_touchdown = rush_touchdown.x + rush_touchdown.y,
      fumble = fumble.x + fumble.y
    ) %>%
    select(-c(rush_attempt.x, rush_attempt.y, rushing_yards.x, rushing_yards.y,
              rush_touchdown.x, rush_touchdown.y, fumble.x, fumble.y, join)) %>%
    relocate(c(rush_attempt, rushing_yards, rush_touchdown, fumble), .after = qb_scramble) %>%
    drop_na(passer) %>%
    mutate(
      big_rush = ifelse(rushing_yards > 100, 1, 0),
      big_pass = ifelse(passing_yards > 300, 1, 0),
      fpts = big_pass * 3 + big_rush * 3 + pass_touchdown * 4 + passing_yards * 0.04 +
        interception * -1 + rushing_yards * 0.1 + rush_touchdown * 6 + fumble * -1,
      fpts_ntile = ntile(fpts, 100)
    ) %>%
    arrange(-fpts) %>%
    mutate(join = tolower(paste(season, week, posteam, passer, sep = "_"))) %>%
    left_join(schedule %>% select(game_id, roof), by = "game_id") %>%
    mutate(
      temp = if_else(roof %in% c("closed", "dome"), 70, temp),
      wind = if_else(is.na(wind), 0, wind),
      weather_check = if_else(temp == 0 & wind == 0, 0, 1),
      dome_games = if_else(roof %in% c("dome", "closed"), 1, 0)
    ) %>%
    relocate(weather_check, .after = wind) %>%
    relocate(c(fpts, fpts_ntile, spread_line, total_line), .after = posteam) %>%
    relocate(c(snaps, epa, epa_per_play), .after = big_pass)
}
qb_fpts_pbp()

#3.0 Load and Join Contests in Parallel ----------------------------------
  plan(multisession)  # Enable parallel processing
contests_qb <- future_lapply(contest_files, function(x) {
  print(paste(x, ": Begin"))
  Extract year and week from file name
  game_year <- as.numeric(substr(x, 1, 4))
  game_week <- as.numeric(substr(x, 7, 8))
  Define folder
  folder <- glue("./01_data/contests/{x}")
  Filter PBP for the year (assuming pbp is global)
  pbp_year <- pbp %>% filter(season == game_year)
  Defensive EPA
  print(paste(x, ": Run epa_def"))
  epa_def <- function() {
    Pass EPA
    pbp_def_pass <- pbp_year %>%
      filter(pass == 1 & week < game_week) %>%
      group_by(defteam) %>%
      summarize(
        def_pass_epa = round(mean(epa), digits = 3),
        n_plays = n()
      ) %>%
      arrange(def_pass_epa) %>%
      mutate(
        def_pass_epa_rank = round(rank(def_pass_epa), 0),
        def_pass_epa_sd = round((def_pass_epa - weighted.mean(def_pass_epa, n_plays, na.rm = TRUE)) / sd(def_pass_epa, na.rm = TRUE), 2)
      )
    Rush EPA
    pbp_def_rush <- pbp_year %>%
      filter(rush == 1 & week < game_week) %>%
      group_by(defteam) %>%
      summarize(
        def_rush_epa = round(mean(epa), digits = 3),
        n_plays = n()
      ) %>%
      arrange(def_rush_epa) %>%
      mutate(
        def_rush_epa_rank = round(rank(def_rush_epa), 0),
        def_rush_epa_sd = round((def_rush_epa - weighted.mean(def_rush_epa, n_plays, na.rm = TRUE)) / sd(def_rush_epa, na.rm = TRUE), 2)
      )
    Combine
    pbp_def <<- pbp_def_pass %>%
      left_join(pbp_def_rush, by = "defteam") %>%
      mutate(
        total_plays = n_plays.x + n_plays.y,
        avg_rank = (def_rush_epa_rank + def_pass_epa_rank) / 2
      ) %>%
      select(defteam, def_rush_epa, def_rush_epa_rank, def_rush_epa_sd, def_pass_epa, def_pass_epa_rank, def_pass_epa_sd)
  }
  epa_def()
  Offensive EPA
  print(paste(x, ": Run epa_off"))
  epa_off <- function() {
    Pass EPA
    pbp_off_pass <- pbp_year %>%
      filter(pass == 1 & week < game_week) %>%
      group_by(posteam) %>%
      summarize(
        off_pass_epa = round(mean(epa), digits = 3),
        n_plays = n()
      ) %>%
      arrange(off_pass_epa) %>%
      mutate(
        off_pass_epa_rank = round(rank(-off_pass_epa), 0),
        off_pass_epa_sd = round((off_pass_epa - weighted.mean(off_pass_epa, n_plays, na.rm = TRUE)) / sd(off_pass_epa, na.rm = TRUE), 2)
      )
    Rush EPA
    pbp_off_rush <- pbp_year %>%
      filter(rush == 1 & week < game_week) %>%
      group_by(posteam) %>%
      summarize(
        off_rush_epa = round(mean(epa), digits = 3),
        n_plays = n()
      ) %>%
      arrange(off_rush_epa) %>%
      mutate(
        off_rush_epa_rank = round(rank(-off_rush_epa), 0),
        off_rush_epa_sd = round((off_rush_epa - weighted.mean(off_rush_epa, n_plays, na.rm = TRUE)) / sd(off_rush_epa, na.rm = TRUE), 2)
      )
    Combine
    pbp_off <<- pbp_off_pass %>%
      left_join(pbp_off_rush, by = "posteam") %>%
      mutate(
        total_plays = n_plays.x + n_plays.y,
        avg_rank = (off_rush_epa_rank + off_pass_epa_rank) / 2
      ) %>%
      select(posteam, off_rush_epa, off_rush_epa_rank, off_rush_epa_sd, off_pass_epa, off_pass_epa_rank, off_pass_epa_sd)
  }
  epa_off()
  PFF Defense Table
  print(paste(x, ": Run def table"))
  def_table <- function() {
    def <- read.csv(glue("{folder}/pff/defense_summary.csv"))
    def_table <- def %>%
      group_by(team_name) %>%
      summarise(
        def = round(weighted.mean(grades_defense, snap_counts_defense), 1),
        rdef = round(weighted.mean(grades_run_defense, snap_counts_run_defense), 1),
        tack = round(weighted.mean(grades_tackle, snap_counts_defense, na.rm = TRUE), 1),
        prsh = round(weighted.mean(grades_pass_rush_defense, snap_counts_pass_rush), 1),
        cov = round(weighted.mean(grades_coverage_defense, snap_counts_coverage), 1)
      )
    def_table <<- def_table %>%
      mutate(
        def_rank = round(rank(-def), 0),
        def_sd = round((def - mean(def)) / sd(def, na.rm = TRUE), 2),
        rdef_rank = round(rank(-rdef), 0),
        rdef_sd = round((rdef - mean(rdef)) / sd(rdef, na.rm = TRUE), 2),
        tack_rank = round(rank(-tack), 0),
        tack_sd = round((tack - mean(tack)) / sd(tack, na.rm = TRUE), 2),
        prsh_rank = round(rank(-prsh), 0),
        prsh_sd = round((prsh - mean(prsh)) / sd(prsh, na.rm = TRUE), 2),
        cov_rank = round(rank(-cov), 0),
        cov_sd = round((cov - mean(cov)) / sd(cov, na.rm = TRUE), 2),
        team_name = gsub('ARZ', 'ARI', team_name),
        team_name = gsub('BLT', 'BAL', team_name),
        team_name = gsub('CLV', 'CLE', team_name),
        team_name = gsub('HST', 'HOU', team_name),
        team_name = gsub('LA', 'LAR', team_name),
        team_name = gsub('LARC', 'LAC', team_name)
      )
  }
  def_table()
  Quarterback Data Processing
  print(paste(x, ": Run quarterback"))
  quarterback <- function() {
    Load depth charts
    depth_charts <- load_depth_charts(seasons = game_year) %>%
      filter(week == game_week, position == "QB", depth_position == "QB") %>%
      select(1:5, 10, 12, 15) %>%
      mutate(
        full_name = clean_player_names(full_name, lowercase = TRUE),
        game_id = paste(season, week, club_code, sep = "")
      ) %>%
      left_join(odds %>% select(away_team, home_team, home_spread, away_spread, home_join, total_line), by = c("game_id" = "home_join")) %>%
      left_join(odds %>% select(away_team, home_team, home_spread, away_spread, away_join, total_line), by = c("game_id" = "away_join")) %>%
      mutate(
        away_team = coalesce(away_team.x, away_team.y),
        home_team = coalesce(home_team.x, home_team.y),
        home_spread = coalesce(home_spread.x, home_spread.y),
        away_spread = coalesce(away_spread.x, away_spread.y),
        total_line = coalesce(total_line.x, total_line.y)
      ) %>%
      select(-c(away_team.x, away_team.y, home_team.x, home_team.y, home_spread.x, home_spread.y, away_spread.x, away_spread.y, total_line.x, total_line.y)) %>%
      mutate(
        opp = if_else(club_code == home_team, away_team, home_team),
        spread = if_else(club_code == home_team, home_spread, away_spread),
        home = if_else(club_code == home_team, 1, 0),
        game_id = paste(season, week, away_team, home_team, sep = "")
      ) %>%
      select(-c(away_team, home_team, away_spread, home_spread))
    print(paste(x, ": Depth Charts"))
    print(depth_charts)
    PFF Passing Data
    pff_pass <- function() {
      passing_summary <<- read.csv(glue("{folder}/pff/passing_summary.csv")) %>%
        mutate(
          player = clean_player_names(player, lowercase = TRUE),
          team_name = clean_team_abbrs(team_name),
          td_game = round(touchdowns / player_game_count, 1),
          pyards_game = round(yards / player_game_count, 1)
        ) %>%
        select(-position)
      pblk <<- read.csv(glue("{folder}/pff/line_pass_blocking_efficiency.csv")) %>%
        mutate(
          team_name = clean_team_abbrs(team_name),
          pressures_game = round(pressures_allowed / player_game_count, 1),
          pbe_rank = round(rank(-pbe), 0),
          pbe_sd = round((pbe - mean(pbe, na.rm = TRUE)) / sd(pbe, na.rm = TRUE), 2)
        ) %>%
        select(team_name, pbe, pressures_game)
      passing_pressure <<- read.csv(glue("{folder}/pff/passing_pressure.csv")) %>%
        mutate(player = clean_player_names(player, lowercase = TRUE)) %>%
        select(player, no_blitz_grades_pass, blitz_grades_pass)
      receiving_summary <<- read.csv(glue("{folder}/pff/receiving_summary.csv")) %>%
        mutate(
          player = clean_player_names(player, lowercase = TRUE),
          team_name = clean_team_abbrs(team_name),
          td_game = round(touchdowns / player_game_count, 1),
          rec_yards_game = round(yards / player_game_count, 1)
        )
      receiving_summary_group <<- receiving_summary %>%
        group_by(team_name) %>%
        summarize(
          team_rec = round(weighted.mean(grades_pass_route, routes), 1),
          team_yprr = round(weighted.mean(yprr, routes), 2)
        )
      qb_ids <- pbp %>% select(passer_id, passer) %>% drop_na() %>% unique()
    }
    pff_pass()
    print(paste(x, ": PFF Data loaded"))
    Join data
    qb <<- depth_charts %>%
      left_join(passing_summary, by = c('full_name' = 'player')) %>%
      left_join(pblk, by = c('club_code' = 'team_name')) %>%
      left_join(passing_pressure, by = c('full_name' = 'player')) %>%
      left_join(receiving_summary_group, by = "club_code") %>%  # Note: Assuming club_code is team_name
      mutate(full_name = str_to_title(full_name)) %>%
      left_join(pbp_def, by = c('opp' = 'defteam')) %>%
      left_join(pbp_off, by = c('club_code' = 'posteam')) %>%
      left_join(def_table, by = c('opp' = 'team_name')) %>%
      left_join(schedule %>% select(game_id, roof, temp, wind), by = "game_id") %>%
      mutate(
        dome = if_else(roof == "dome", 1, 0),
        dome = if_else(is.na(dome), 0, dome),
        contest_year = game_year,  # Fixed to game_year
        contest_week = game_week,
        contest = x
      ) %>%
      separate(contest, into = c("folder", "contest"), sep = "_") %>%  # Adjusted sep if needed
      mutate(z_score = round(0.20 * off_pass_epa_sd - 0.20 * def_pass_epa_sd - 0.20 * cov_sd, 3))
  }
  quarterback()
  return(qb)  # Return the qb data frame
})
plan(sequential)  # Reset to sequential
Clean up unnecessary objects
rm(pbp_def, pbp_off, passing_summary, passing_pressure, receiving_summary, receiving_summary_group, pblk)

#4.0 Process QB Data Frame -----------------------------------------------
  process_qb_df <- function() {
    contests_qb_df <<- bind_rows(contests_qb) %>%
      filter(week != 2) %>%  # Remove week 2s (bad data)
      separate(full_name, into = c("first_name", "last_name"), sep = " ", extra = "drop") %>%
      mutate(
        player = paste0(substr(first_name, 1, 1), ".", last_name),
        join = tolower(paste(season, week, club_code, player, sep = "")),
        name = paste(first_name, last_name)
      ) %>%
      relocate(name, .before = first_name) %>%
      select(-c(first_name, last_name)) %>%
      left_join(qb_fpts %>% select(join, fpts, fpts_ntile, temp, wind, pass_attempt, passing_yards, pass_touchdown, rush_attempt, rushing_yards, rush_touchdown, fumble), by = "join") %>%
      replace_na(list(fpts = 0, fpts_ntile = 0)) %>%
      mutate(fpts_ntile = ntile(fpts, 100)) %>%
      mutate(
        def_sd = round((def - mean(def)) / sd(def, na.rm = TRUE), 2),
        rdef_sd = round((rdef - mean(rdef)) / sd(rdef, na.rm = TRUE), 2),
        tack_sd = round((tack - mean(tack)) / sd(tack, na.rm = TRUE), 2),
        prsh_sd = round((prsh - mean(prsh)) / sd(prsh, na.rm = TRUE), 2),
        cov_sd = round((cov - mean(cov)) / sd(cov, na.rm = TRUE), 2),
        inj_join = paste(contest_year, contest_week, club_code, name, sep = "")
      ) %>%
      left_join(inj %>% select(inj_join, report_status, practice_status), by = "inj_join") %>%
      mutate(report_status = as.factor(report_status)) %>%
      pivot_wider(names_from = report_status, values_from = report_status, names_prefix = "status_", values_fill = 0, values_fn = ~1) %>%
      mutate(depth_team = as.numeric(depth_team)) %>%
      relocate(c(z_score, fpts, fpts_ntile, pass_attempt, passing_yards, pass_touchdown), .after = game_id) %>%
      relocate(c(off_rush_epa_sd, def_rush_epa_sd, cov_sd), .after = home) %>%
      arrange(-fpts) %>%
      filter(attempts > 10) %>%
      filter(status_Out == 0 | is.na(status_Out)) %>%  # Handle NA
      filter(fpts != 0)
  }
process_qb_df()
names(contests_qb_df)  # Print names for verification
 #5.0 Correlation Table ---------------------------------------------------
  correlation_table <- function() {
    numeric_contest_qb <<- contests_qb_df[, sapply(contests_qb_df, is.numeric)]
    cor_matrix <- cor(numeric_contest_qb, use = "complete.obs")
    cor_df <- as.data.frame(cor_matrix)
    cor_df <- rownames_to_column(cor_df, var = "Variable")
    cor_fpts_df <<- cor_df %>%
      select(Variable, fpts) %>%
      arrange(-fpts)
  }
correlation_table()

#6.0 Split Train/Test Data -----------------------------------------------
  model_data <- numeric_contest_qb %>%
  filter(depth_team == 1) %>%
  filter(pass_attempt > 10) %>%
  filter(passing_yards > 150)
set.seed(10)
split_index <- createDataPartition(model_data$fpts, p = 0.75, list = FALSE)
train_data <- model_data[split_index, ]
test_data <- model_data[-split_index, ]
Reusable RF Training Function -------------------------------------------
  train_rf_model <- function(formula, data, test_data, target_var, model_name, mtry = 2, nodesize = 10, ntree = 1000) {
    Train initial RF
    rf_model <- randomForest(formula, data = data, mtry = mtry, nodesize = nodesize, ntree = ntree)
    Predict and evaluate
    predictions <- round(predict(rf_model, test_data), 2)
    performance <- round(postResample(predictions, test_data[[target_var]]), 3)
    print(performance)
    Tuning
    tune_grid <- expand.grid(
      mtry = 1:5,
      splitrule = "variance",
      min.node.size = c(5, 10, 15, 20, 25, 50, 100)
    )
    tuned_model <- train(
      formula,
      data = data,
      method = "ranger",
      trControl = trainControl(method = "cv", number = 5),
      tuneGrid = tune_grid
    )
    print(tuned_model$bestTune)
    Var importance
    varImpPlot(rf_model)
    Save model
    save(rf_model, file = glue("./04_models/qb/{model_name}.Rdata"))
    return(list(model = rf_model, tuned = tuned_model, performance = performance))
  }

#6.1 FPTS Model ----------------------------------------------------------
  fpts_formula <- fpts ~ spread + total_line + dome + pyards_game + ypa + td_game + team_yprr + grades_pass + def_pass_epa
qb_fpts_results <- train_rf_model(fpts_formula, train_data, test_data, "fpts", "qb_fpts_rf", mtry = 2)

#6.2 Passing Yards Model -------------------------------------------------
  passing_yards_formula <- passing_yards ~ spread + total_line + dome + pyards_game + ypa + td_game + team_yprr + grades_pass + def_pass_epa
qb_passing_yards_results <- train_rf_model(passing_yards_formula, train_data, test_data, "passing_yards", "qb_passing_yards_rf", mtry = 1)

#6.3 Pass Touchdown Model ------------------------------------------------
  pass_touchdown_formula <- pass_touchdown ~ spread + total_line + dome + pyards_game + ypa + td_game + team_yprr + grades_pass + def_pass_epa
qb_pass_touchdown_results <- train_rf_model(pass_touchdown_formula, train_data, test_data, "pass_touchdown", "qb_pass_touchdown_rf", mtry = 1)

#6.4 Pass Attempt Model --------------------------------------------------
  pass_attempt_formula <- pass_attempt ~ spread + total_line + dome + pyards_game + ypa + td_game + team_yprr + grades_pass + def_pass_epa
qb_pass_attempt_results <- train_rf_model(pass_attempt_formula, train_data, test_data, "pass_attempt", "qb_pass_attempt_rf", mtry = 1)

#6.5 Rush Attempt Model --------------------------------------------------
  rush_attempt_formula <- rush_attempt ~ spread + total_line + dome + pyards_game + ypa + td_game + team_yprr + grades_pass + def_rush_epa
qb_rush_attempt_results <- train_rf_model(rush_attempt_formula, train_data, test_data, "rush_attempt", "qb_rush_attempt_rf", mtry = 3, nodesize = 5)

#6.6 Rushing Yards Model -------------------------------------------------
  rushing_yards_formula <- rushing_yards ~ spread + total_line + dome + pyards_game + ypa + td_game + team_yprr + grades_pass + def_pass_epa
qb_rushing_yards_results <- train_rf_model(rushing_yards_formula, train_data, test_data, "rushing_yards", "qb_rushing_yards_rf", mtry = 1)

#6.7 Rush Touchdown Model ------------------------------------------------
  rush_touchdown_formula <- rush_touchdown ~ spread + total_line + dome + pyards_game + ypa + td_game + team_yprr + grades_pass + def_rush_epa
qb_rush_touchdown_results <- train_rf_model(rush_touchdown_formula, train_data, test_data, "rush_touchdown", "qb_rush_touchdown_rf", mtry = 1)