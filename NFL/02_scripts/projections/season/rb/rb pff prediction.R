# project rb pff grade

# ---- packages ----
suppressPackageStartupMessages({
  library(tidyverse)
  library(nflverse)
  library(randomForest)
  library(glue)
})

# ---- 0) Load & filter base data ----
ol_grades_team <- readRDS("./01_data/season_grades/nfl/ol_grades_team.RDS") %>% 
  select(3:5)

seasons <- 2012:2024

# load rb grades
rb_grades <- readRDS("./01_data/season_grades/nfl/rb_grades.RDS")

# use funciton if rb is not loaded
calc_rb_grades <- function(){
  
  rb_list <- lapply(seasons, function(season) {
    read.csv(glue("./01_data/season_grades/nfl/{season}/rushing_summary.csv")) %>%
      mutate(season = season)
})

rb_grades <- bind_rows(rb_list) %>%
  select(player, player_id, position, team_name, season, player_game_count,
         grades_run, grades_offense, grades_hands_fumble, grades_pass_block, grades_run_block,
         attempts, run_plays, yards, yards_after_contact, first_downs, touchdowns, fumbles, 
         breakaway_attempts, breakaway_percent, breakaway_yards, 
         avoided_tackles, elusive_rating) %>%
  filter(position %in% c("HB")) %>%
  arrange(-grades_run) %>% 
  filter(attempts > 54) %>% # median of dataset
  mutate(join = paste(season, team_name, sep = "_")) %>% 
  left_join(ol_grades_team, by = c("join"))

# save for reuse
saveRDS(rb_grades, file = "./01_data/season_grades/nfl/rb_grades.RDS")
}
calc_rb_grades()

# Load roster data 
rosters <- load_rosters(2025) %>% 
  select(season, team, position, full_name, birth_date, height, weight, gsis_id, years_exp) %>% 
  mutate(age = season - year(birth_date)) %>% 
  filter(position == "RB")

# project RB pff grades and stats

# ---- packages ----
library(tidyverse)
library(randomForest)
library(glue)

# ---- 0) Load & filter base data ----
# Assume rb_grades is already loaded as per previous code
# If not, load it:
# rb_grades <- readRDS("./01_data/season_grades/nfl/rb_grades.RDS")

# Assume rosters is loaded; if not, load your 2025 roster data
# For this script, assuming rosters is an nflverse roster or similar with columns: season=2025, team, position, full_name, etc.

# Filter rb_grades for eligibility
rb_grades <- rb_grades %>%
  filter(position == "HB") %>%
  filter(attempts > 54)

# ---- 1) Feature engineering: predict NEXT season (t+1) from current season (t) + lags ----
fe <- rb_grades %>%
  arrange(player, season) %>%  # Use player (name) for grouping since rosters uses full_name
  group_by(player) %>%
  mutate(
    seasons_played = row_number(),
    # current season values (t)
    run_grade_t = grades_run,
    off_grade_t = grades_offense,
    att_t       = attempts,
    rp_t        = run_plays,
    yards_t     = yards,
    td_t        = touchdowns,
    
    # lags (relative to t)
    run_grade_lag1 = lag(run_grade_t, 1),
    run_grade_lag2 = lag(run_grade_t, 2),
    run_grade_lag3 = lag(run_grade_t, 3),
    off_grade_lag1 = lag(off_grade_t, 1),
    off_grade_lag2 = lag(off_grade_t, 2),
    off_grade_lag3 = lag(off_grade_t, 3),
    att_lag1       = lag(att_t, 1),
    att_lag2       = lag(att_t, 2),
    att_lag3       = lag(att_t, 3),
    rp_lag1        = lag(rp_t, 1),
    rp_lag2        = lag(rp_t, 2),
    rp_lag3        = lag(rp_t, 3),
    yards_lag1     = lag(yards_t, 1),
    yards_lag2     = lag(yards_t, 2),
    yards_lag3     = lag(yards_t, 3),
    td_lag1        = lag(td_t, 1),
    td_lag2        = lag(td_t, 2),
    td_lag3        = lag(td_t, 3),
    
    # simple trend over last ~3 seasons; 0 if not enough history
    run_grade_trend3 = if_else(seasons_played >= 3, (run_grade_t - run_grade_lag2)/2, 0),
    off_grade_trend3 = if_else(seasons_played >= 3, (off_grade_t - off_grade_lag2)/2, 0),
    yards_trend3     = if_else(seasons_played >= 3, (yards_t - yards_lag2)/2, 0),
    td_trend3        = if_else(seasons_played >= 3, (td_t - td_lag2)/2, 0),
    
    # targets: next season (t+1)
    run_grade_next = lead(run_grade_t, 1),
    off_grade_next = lead(off_grade_t, 1),
    yards_next     = lead(yards_t, 1),
    td_next        = lead(td_t, 1)
  ) %>%
  ungroup()

# ---- 2) Medians for imputation (no position subgroups since all HB/RB) ----
meds <- fe %>%
  summarise(
    med_run_grade = median(run_grade_t, na.rm = TRUE),
    med_off_grade = median(off_grade_t, na.rm = TRUE),
    med_att       = median(att_t, na.rm = TRUE),
    med_rp        = median(rp_t, na.rm = TRUE),
    med_yards     = median(yards_t, na.rm = TRUE),
    med_td        = median(td_t, na.rm = TRUE),
    .groups = "drop"
  )

# Cascade-fill helper: seed lag1 from a median if missing, then lag2<-lag1, lag3<-lag2
bf3 <- function(l1, l2, l3, seed) {
  l1i <- dplyr::coalesce(l1, seed)
  l2i <- dplyr::coalesce(l2, l1i)
  l3i <- dplyr::coalesce(l3, l2i)
  tibble(l1 = l1i, l2 = l2i, l3 = l3i)
}

# Robust numeric imputer (kills NaN/Inf and fills any remaining gaps with column median)
impute_df <- function(df){
  df %>%
    mutate(across(where(is.numeric), ~replace(., !is.finite(.), NA_real_))) %>%
    mutate(across(where(is.numeric), ~{
      x <- .
      if (all(is.na(x))) 0 else replace(x, is.na(x), median(x, na.rm = TRUE))
    }))
}

# Predictors we’ll use
preds <- c(
  "run_grade_t", "run_grade_lag1", "run_grade_lag2", "run_grade_lag3",
  "off_grade_t", "off_grade_lag1", "off_grade_lag2", "off_grade_lag3",
  "att_t", "att_lag1", "att_lag2", "att_lag3",
  "rp_t", "rp_lag1", "rp_lag2", "rp_lag3",
  "yards_t", "yards_lag1", "yards_lag2", "yards_lag3",
  "td_t", "td_lag1", "td_lag2", "td_lag3",
  "run_grade_trend3", "off_grade_trend3", "yards_trend3", "td_trend3",
  "seasons_played"
)

# ---- 3) Build TRAIN set (rows that have a known next season) + impute ----
train <- fe %>%
  filter(!is.na(run_grade_next) & !is.na(off_grade_next) & !is.na(yards_next) & !is.na(td_next)) %>%
  mutate(
    run_grade_t = coalesce(run_grade_t, meds$med_run_grade),
    off_grade_t = coalesce(off_grade_t, meds$med_off_grade),
    att_t       = coalesce(att_t, meds$med_att),
    rp_t        = coalesce(rp_t, meds$med_rp),
    yards_t     = coalesce(yards_t, meds$med_yards),
    td_t        = coalesce(td_t, meds$med_td)
  ) %>%
  rowwise() %>%
  mutate(
    {tmp <- bf3(run_grade_lag1, run_grade_lag2, run_grade_lag3, meds$med_run_grade)
    run_grade_lag1 = tmp$l1; run_grade_lag2 = tmp$l2; run_grade_lag3 = tmp$l3},
    {tmp <- bf3(off_grade_lag1, off_grade_lag2, off_grade_lag3, meds$med_off_grade)
    off_grade_lag1 = tmp$l1; off_grade_lag2 = tmp$l2; off_grade_lag3 = tmp$l3},
    {tmp <- bf3(att_lag1, att_lag2, att_lag3, meds$med_att)
    att_lag1 = tmp$l1; att_lag2 = tmp$l2; att_lag3 = tmp$l3},
    {tmp <- bf3(rp_lag1, rp_lag2, rp_lag3, meds$med_rp)
    rp_lag1 = tmp$l1; rp_lag2 = tmp$l2; rp_lag3 = tmp$l3},
    {tmp <- bf3(yards_lag1, yards_lag2, yards_lag3, meds$med_yards)
    yards_lag1 = tmp$l1; yards_lag2 = tmp$l2; yards_lag3 = tmp$l3},
    {tmp <- bf3(td_lag1, td_lag2, td_lag3, meds$med_td)
    td_lag1 = tmp$l1; td_lag2 = tmp$l2; td_lag3 = tmp$l3}
  ) %>%
  ungroup() %>%
  # recompute trends safely
  mutate(
    run_grade_trend3 = if_else(is.na(run_grade_lag2), 0, (run_grade_t - run_grade_lag2)/2),
    off_grade_trend3 = if_else(is.na(off_grade_lag2), 0, (off_grade_t - off_grade_lag2)/2),
    yards_trend3     = if_else(is.na(yards_lag2), 0, (yards_t - yards_lag2)/2),
    td_trend3        = if_else(is.na(td_lag2), 0, (td_t - td_lag2)/2)
  )

X <- train %>% select(all_of(preds)) %>% impute_df() %>% as.data.frame()
y_run_grade <- train$run_grade_next
y_off_grade <- train$off_grade_next
y_yards     <- train$yards_next
y_td        <- train$td_next

# Align and guarantee no NAs
keep_cols <- names(X)[colSums(is.na(X)) == 0]
X <- X[keep_cols]
stopifnot(sum(is.na(X)) == 0)

# ---- 4) Fit Random Forests ----
set.seed(42)
rf_run_grade <- randomForest(x = X, y = y_run_grade, ntree = 800)
rf_off_grade <- randomForest(x = X, y = y_off_grade, ntree = 800)
rf_yards     <- randomForest(x = X, y = y_yards, ntree = 800)
rf_td        <- randomForest(x = X, y = y_td, ntree = 800)

# ---- 5) Build 2025 PREDICTION frame from most recent seasons for 2025 roster players + same imputation ----
# Filter rosters to RBs
rosters_rb <- rosters %>%
  filter(position == "RB") %>%
  select(full_name, team, age, years_exp, gsis_id)  # Select relevant columns

# Get most recent season data for each player in rosters (match on player == full_name)
pred_2025 <- fe %>%
  filter(player %in% rosters_rb$full_name) %>%
  group_by(player) %>%
  slice_tail(n = 1) %>%  # Most recent season per player
  ungroup() %>%
  left_join(rosters_rb, by = c("player" = "full_name")) %>%  # Add 2025 team and other info
  mutate(
    run_grade_t = coalesce(run_grade_t, meds$med_run_grade),
    off_grade_t = coalesce(off_grade_t, meds$med_off_grade),
    att_t       = coalesce(att_t, meds$med_att),
    rp_t        = coalesce(rp_t, meds$med_rp),
    yards_t     = coalesce(yards_t, meds$med_yards),
    td_t        = coalesce(td_t, meds$med_td)
  ) %>%
  rowwise() %>%
  mutate(
    {tmp <- bf3(run_grade_lag1, run_grade_lag2, run_grade_lag3, meds$med_run_grade)
    run_grade_lag1 = tmp$l1; run_grade_lag2 = tmp$l2; run_grade_lag3 = tmp$l3},
    {tmp <- bf3(off_grade_lag1, off_grade_lag2, off_grade_lag3, meds$med_off_grade)
    off_grade_lag1 = tmp$l1; off_grade_lag2 = tmp$l2; off_grade_lag3 = tmp$l3},
    {tmp <- bf3(att_lag1, att_lag2, att_lag3, meds$med_att)
    att_lag1 = tmp$l1; att_lag2 = tmp$l2; att_lag3 = tmp$l3},
    {tmp <- bf3(rp_lag1, rp_lag2, rp_lag3, meds$med_rp)
    rp_lag1 = tmp$l1; rp_lag2 = tmp$l2; rp_lag3 = tmp$l3},
    {tmp <- bf3(yards_lag1, yards_lag2, yards_lag3, meds$med_yards)
    yards_lag1 = tmp$l1; yards_lag2 = tmp$l2; yards_lag3 = tmp$l3},
    {tmp <- bf3(td_lag1, td_lag2, td_lag3, meds$med_td)
    td_lag1 = tmp$l1; td_lag2 = tmp$l2; td_lag3 = tmp$l3}
  ) %>%
  ungroup() %>%
  # recompute trends safely
  mutate(
    run_grade_trend3 = if_else(is.na(run_grade_lag2), 0, (run_grade_t - run_grade_lag2)/2),
    off_grade_trend3 = if_else(is.na(off_grade_lag2), 0, (off_grade_t - off_grade_lag2)/2),
    yards_trend3     = if_else(is.na(yards_lag2), 0, (yards_t - yards_lag2)/2),
    td_trend3        = if_else(is.na(td_lag2), 0, (td_t - td_lag2)/2)
  )

# For rookies/new players with no history, add rows with all medians and seasons_played=0
missing_players <- setdiff(rosters_rb$full_name, pred_2025$player)
if (length(missing_players) > 0) {
  missing_df <- rosters_rb %>%
    filter(full_name %in% missing_players) %>%
    mutate(
      player = full_name,
      player_id = NA,  # or some placeholder
      position = "HB",
      team_name = NA,  # last team unknown
      season = NA,
      player_game_count = NA,
      seasons_played = 0,
      run_grade_t = meds$med_run_grade,
      off_grade_t = meds$med_off_grade,
      att_t = meds$med_att,
      rp_t = meds$med_rp,
      yards_t = meds$med_yards,
      td_t = meds$med_td,
      run_grade_lag1 = meds$med_run_grade,
      run_grade_lag2 = meds$med_run_grade,
      run_grade_lag3 = meds$med_run_grade,
      off_grade_lag1 = meds$med_off_grade,
      off_grade_lag2 = meds$med_off_grade,
      off_grade_lag3 = meds$med_off_grade,
      att_lag1 = meds$med_att,
      att_lag2 = meds$med_att,
      att_lag3 = meds$med_att,
      rp_lag1 = meds$med_rp,
      rp_lag2 = meds$med_rp,
      rp_lag3 = meds$med_rp,
      yards_lag1 = meds$med_yards,
      yards_lag2 = meds$med_yards,
      yards_lag3 = meds$med_yards,
      td_lag1 = meds$med_td,
      td_lag2 = meds$med_td,
      td_lag3 = meds$med_td,
      run_grade_trend3 = 0,
      off_grade_trend3 = 0,
      yards_trend3 = 0,
      td_trend3 = 0
    )
  pred_2025 <- bind_rows(pred_2025, missing_df)
}

X_pred <- pred_2025 %>% select(all_of(preds)) %>% impute_df() %>% as.data.frame()
X_pred <- X_pred[keep_cols]  # align to training columns

# clamp helper to keep within reasonable bounds (adjust as needed; e.g., grades 0-100, yards/td non-negative)
clip <- function(x, lo = 0, hi = Inf) pmin(pmax(x, lo), hi)

pred_2025 <- pred_2025 %>%
  mutate(
    pred_grades_run_2025 = clip(predict(rf_run_grade, newdata = X_pred), 0, 99.9),
    pred_grades_offense_2025 = clip(predict(rf_off_grade, newdata = X_pred), 0, 99.9),
    pred_yards_2025 = clip(predict(rf_yards, newdata = X_pred), 0),
    pred_touchdowns_2025 = clip(predict(rf_td, newdata = X_pred), 0)
  )

# ---- 6) OPTIONAL: calibration to keep distribution tails ----
cal_stats <- train %>%
  summarise(
    mu_run_grade = mean(run_grade_next), sd_run_grade = sd(run_grade_next),
    mu_off_grade = mean(off_grade_next), sd_off_grade = sd(off_grade_next),
    mu_yards = mean(yards_next), sd_yards = sd(yards_next),
    mu_td = mean(td_next), sd_td = sd(td_next),
    .groups = "drop"
  )

pred_2025 <- pred_2025 %>%
  mutate(
    mu_hat_run_grade = mean(pred_grades_run_2025),
    sd_hat_run_grade = sd(pred_grades_run_2025),
    mu_hat_off_grade = mean(pred_grades_offense_2025),
    sd_hat_off_grade = sd(pred_grades_offense_2025),
    mu_hat_yards = mean(pred_yards_2025),
    sd_hat_yards = sd(pred_yards_2025),
    mu_hat_td = mean(pred_touchdowns_2025),
    sd_hat_td = sd(pred_touchdowns_2025)
  ) %>%
  rowwise() %>%
  mutate(
    pred_grades_run_2025_cal = clip((pred_grades_run_2025 - mu_hat_run_grade) * ifelse(sd_hat_run_grade > 0, cal_stats$sd_run_grade / sd_hat_run_grade, 1) + cal_stats$mu_run_grade, 0, 99.9),
    pred_grades_offense_2025_cal = clip((pred_grades_offense_2025 - mu_hat_off_grade) * ifelse(sd_hat_off_grade > 0, cal_stats$sd_off_grade / sd_hat_off_grade, 1) + cal_stats$mu_off_grade, 0, 99.9),
    pred_yards_2025_cal = clip((pred_yards_2025 - mu_hat_yards) * ifelse(sd_hat_yards > 0, cal_stats$sd_yards / sd_hat_yards, 1) + cal_stats$mu_yards, 0),
    pred_touchdowns_2025_cal = clip((pred_touchdowns_2025 - mu_hat_td) * ifelse(sd_hat_td > 0, cal_stats$sd_td / sd_hat_td, 1) + cal_stats$mu_td, 0)
  ) %>%
  ungroup()

# ---- 7) Final table ----
preds_out <- pred_2025 %>%
  transmute(
    player, player_id, gsis_id, position = "RB", team_2025 = team,
    last_season = season, run_grade_last = run_grade_t, off_grade_last = off_grade_t,
    yards_last = yards_t, td_last = td_t,
    pred_grades_run_2025, pred_grades_offense_2025, pred_yards_2025, pred_touchdowns_2025,
    pred_grades_run_2025_cal, pred_grades_offense_2025_cal, pred_yards_2025_cal, pred_touchdowns_2025_cal
  )

# examples:
# View head
head(preds_out)

# Check a specific player (e.g., Derrick Henry)
preds_out %>% filter(player == "Derrick Henry")

save(preds_out, file = "./05_outputs/season_projections/2025_rb_stats.RDS")