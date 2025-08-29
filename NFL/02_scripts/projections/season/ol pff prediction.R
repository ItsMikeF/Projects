# project OL pff grade

# ---- packages ----
library(tidyverse)
library(randomForest)
library(glue)

# ---- 0) Load & filter base data ----
seasons <- 2012:2024

ol_list <- lapply(seasons, function(season) {
  read.csv(glue("./01_data/season_grades/nfl/{season}/offense_blocking.csv")) %>%
    mutate(season = season)
})

ol_grades_player <- bind_rows(ol_list) %>%
  select(player, player_id, position, team_name, season, player_game_count,
         grades_pass_block, snap_counts_pass_block,
         grades_run_block,  snap_counts_run_block) %>%
  filter(position %in% c("G","T","C")) %>%
  filter(snap_counts_pass_block > 100)

# save for reuse
saveRDS(ol_grades_player, file = "./01_data/season_grades/nfl/ol_grades_player.RDS")

# ---- 1) Feature engineering: predict NEXT season (t+1) from current season (t) + lags ----
fe <- ol_grades_player %>%
  arrange(player_id, season) %>%
  group_by(player_id) %>%
  mutate(
    seasons_played = row_number(),
    # current season values (t)
    pass_t = grades_pass_block,
    run_t  = grades_run_block,
    sp_t   = snap_counts_pass_block,
    sr_t   = snap_counts_run_block,
    
    # lags (relative to t)
    pass_lag1 = lag(pass_t, 1),
    pass_lag2 = lag(pass_t, 2),
    pass_lag3 = lag(pass_t, 3),
    run_lag1  = lag(run_t, 1),
    run_lag2  = lag(run_t, 2),
    run_lag3  = lag(run_t, 3),
    sp_lag1   = lag(sp_t, 1),
    sp_lag2   = lag(sp_t, 2),
    sp_lag3   = lag(sp_t, 3),
    sr_lag1   = lag(sr_t, 1),
    sr_lag2   = lag(sr_t, 2),
    sr_lag3   = lag(sr_t, 3),
    
    # simple trend over last ~3 seasons; 0 if not enough history
    pass_trend3 = if_else(seasons_played >= 3, (pass_t - pass_lag2)/2, 0),
    run_trend3  = if_else(seasons_played >= 3, (run_t  - run_lag2)/2, 0),
    
    # targets: next season (t+1)
    pass_next = lead(pass_t, 1),
    run_next  = lead(run_t,  1)
  ) %>%
  ungroup()

# ---- 2) Position medians for imputation ----
pos_meds <- fe %>%
  group_by(position) %>%
  summarise(
    med_pass = median(pass_t, na.rm = TRUE),
    med_run  = median(run_t,  na.rm = TRUE),
    med_sp   = median(sp_t,   na.rm = TRUE),
    med_sr   = median(sr_t,   na.rm = TRUE),
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
  "pass_t","pass_lag1","pass_lag2","pass_lag3",
  "run_t","run_lag1","run_lag2","run_lag3",
  "sp_t","sp_lag1","sp_lag2","sp_lag3",
  "sr_t","sr_lag1","sr_lag2","sr_lag3",
  "pass_trend3","run_trend3","seasons_played"
)

# ---- 3) Build TRAIN set (rows that have a known next season) + impute ----
train <- fe %>%
  filter(!is.na(pass_next) & !is.na(run_next)) %>%
  left_join(pos_meds, by = "position") %>%
  # seed current-season values if missing
  mutate(
    pass_t = coalesce(pass_t, med_pass),
    run_t  = coalesce(run_t,  med_run),
    sp_t   = coalesce(sp_t,   med_sp),
    sr_t   = coalesce(sr_t,   med_sr)
  ) %>%
  rowwise() %>%
  mutate(
    {tmp <- bf3(pass_lag1, pass_lag2, pass_lag3, med_pass)
    pass_lag1 = tmp$l1; pass_lag2 = tmp$l2; pass_lag3 = tmp$l3},
    {tmp <- bf3(run_lag1, run_lag2, run_lag3, med_run)
    run_lag1  = tmp$l1; run_lag2  = tmp$l2; run_lag3  = tmp$l3},
    {tmp <- bf3(sp_lag1, sp_lag2, sp_lag3, med_sp)
    sp_lag1   = tmp$l1; sp_lag2   = tmp$l2; sp_lag3   = tmp$l3},
    {tmp <- bf3(sr_lag1, sr_lag2, sr_lag3, med_sr)
    sr_lag1   = tmp$l1; sr_lag2   = tmp$l2; sr_lag3   = tmp$l3}
  ) %>%
  ungroup() %>%
  # recompute trend safely
  mutate(
    pass_trend3 = if_else(is.na(pass_lag2), 0, (pass_t - pass_lag2)/2),
    run_trend3  = if_else(is.na(run_lag2),  0, (run_t  - run_lag2)/2)
  )

X_pass <- train %>% select(all_of(preds)) %>% impute_df() %>% as.data.frame()
X_run  <- X_pass
y_pass <- train$pass_next
y_run  <- train$run_next

# Align and guarantee no NAs
keep_cols <- names(X_pass)[colSums(is.na(X_pass)) == 0]
X_pass <- X_pass[keep_cols]
X_run  <- X_run [keep_cols]
stopifnot(sum(is.na(X_pass)) == 0)

# ---- 4) Fit Random Forests ----
set.seed(42)
rf_pass <- randomForest(x = X_pass, y = y_pass, ntree = 800)
rf_run  <- randomForest(x = X_run,  y = y_run,  ntree = 800)

# ---- 5) Build 2025 PREDICTION frame from 2024 rows + same imputation ----
pred_2025 <- fe %>%
  filter(season == 2024) %>%
  left_join(pos_meds, by = "position") %>%
  mutate(
    pass_t = coalesce(pass_t, med_pass),
    run_t  = coalesce(run_t,  med_run),
    sp_t   = coalesce(sp_t,   med_sp),
    sr_t   = coalesce(sr_t,   med_sr)
  ) %>%
  rowwise() %>%
  mutate(
    {tmp <- bf3(pass_lag1, pass_lag2, pass_lag3, med_pass)
    pass_lag1 = tmp$l1; pass_lag2 = tmp$l2; pass_lag3 = tmp$l3},
    {tmp <- bf3(run_lag1, run_lag2, run_lag3, med_run)
    run_lag1  = tmp$l1; run_lag2  = tmp$l2; run_lag3  = tmp$l3},
    {tmp <- bf3(sp_lag1, sp_lag2, sp_lag3, med_sp)
    sp_lag1   = tmp$l1; sp_lag2   = tmp$l2; sp_lag3   = tmp$l3},
    {tmp <- bf3(sr_lag1, sr_lag2, sr_lag3, med_sr)
    sr_lag1   = tmp$l1; sr_lag2   = tmp$l2; sr_lag3   = tmp$l3}
  ) %>%
  ungroup() %>%
  mutate(
    pass_trend3 = if_else(is.na(pass_lag2), 0, (pass_t - pass_lag2)/2),
    run_trend3  = if_else(is.na(run_lag2),  0, (run_t  - run_lag2)/2)
  )

X_pred <- pred_2025 %>% select(all_of(preds)) %>% impute_df() %>% as.data.frame()
X_pred <- X_pred[keep_cols]  # align to training columns

# clamp helper to keep within PFF bounds
clip <- function(x, lo = 0, hi = 99.9) pmin(pmax(x, lo), hi)

pred_2025 <- pred_2025 %>%
  mutate(
    pred_pass_block_2025 = clip(predict(rf_pass, newdata = X_pred)),
    pred_run_block_2025  = clip(predict(rf_run,  newdata = X_pred))
  )

# ---- 6) OPTIONAL: position-wise calibration to keep elite tails ----
cal_stats <- train %>%
  group_by(position) %>%
  summarise(
    mu_run = mean(run_next), sd_run = sd(run_next),
    mu_pass = mean(pass_next), sd_pass = sd(pass_next),
    .groups = "drop"
  )

pred_2025 <- pred_2025 %>%
  group_by(position) %>%
  mutate(
    mu_hat_run  = mean(pred_run_block_2025),
    sd_hat_run  = sd(pred_run_block_2025),
    mu_hat_pass = mean(pred_pass_block_2025),
    sd_hat_pass = sd(pred_pass_block_2025)
  ) %>%
  ungroup() %>%
  left_join(cal_stats, by = "position") %>%
  mutate(
    pred_run_block_2025_cal  =
      clip((pred_run_block_2025  - mu_hat_run ) * ifelse(sd_hat_run > 0,  sd_run/sd_hat_run, 1) + mu_run),
    pred_pass_block_2025_cal =
      clip((pred_pass_block_2025 - mu_hat_pass) * ifelse(sd_hat_pass > 0, sd_pass/sd_hat_pass, 1) + mu_pass)
  )

# ---- 7) Final table ----
preds_out <- pred_2025 %>%
  transmute(
    player, player_id, position, team_name,
    season_2024 = season, pass_2024 = pass_t, run_2024 = run_t,
    pred_pass_block_2025, pred_run_block_2025,
    pred_pass_block_2025_cal, pred_run_block_2025_cal
  )

# examples:
# View head
head(preds_out)

# Check a specific player (e.g., Trent Williams)
preds_out %>% filter(player == "Trent Williams")

save(preds_out, file = "./05_outputs/season_projections/2025_ol_pff_grades.RDS")
