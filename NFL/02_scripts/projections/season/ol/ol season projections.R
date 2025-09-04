# predict ol rankings for 2025

# ---- packages ----
library(tidyverse)
library(randomForest)

# load pff predicted grades
ol_grades <- load("./05_outputs/season_projections/2025_ol_pff_grades.RDS")
preds_out_select <- preds_out %>% select(player, pred_pass_block_2025_cal, pred_run_block_2025_cal)

# espn depth charts from espn depth charts script
load("./01_data/depth_chart/espn_depth_chart_2025.RDS")

# load ol grades
load("./01_data/season_grades/nfl/ol_grades.RDS")

# calculate OL weights 
ol_weights <- function() {
  # PASS (LT > RT by APY ratio)
  lt_apy <- 28.5
  rt_apy <- 17.5
  tackle_share_pass <- 0.60
  ratio_lr <- lt_apy / rt_apy
  
  w_LT1_pass <- tackle_share_pass * (ratio_lr / (1 + ratio_lr))
  w_RT1_pass <- tackle_share_pass * (1 / (1 + ratio_lr))
  w_LG1_pass <- 0.13
  w_C1_pass  <- 0.14
  w_RG1_pass <- 0.13
  
  # RUN (IOL-heavy; each Guard > Center by APY ratio)
  g_apy <- 23.5
  c_apy <- 18.0
  interior_share_run <- 0.80
  
  ratio_gc <- g_apy / c_apy
  uC <- 1
  uG <- ratio_gc
  sum_units <- uC + 2*uG
  
  w_C1_run  <- interior_share_run * (uC / sum_units)
  w_G_run   <- interior_share_run * (uG / sum_units)  # LG1 & RG1
  w_LT1_run <- (1 - interior_share_run) / 2
  w_RT1_run <- (1 - interior_share_run) / 2
  
  # Build + round
  weights <- tibble::tribble(
    ~pos,  ~w_pass,       ~w_run,
    "LT1", w_LT1_pass,    w_LT1_run,
    "LG1", w_LG1_pass,    w_G_run,
    "C1",  w_C1_pass,     w_C1_run,
    "RG1", w_RG1_pass,    w_G_run,
    "RT1", w_RT1_pass,    w_RT1_run
  ) %>%
    mutate(across(c(w_pass, w_run), ~ round(.x, 2)))
  
  # Optional: force columns to sum to 1.00 exactly (re-normalize then re-round)
  # weights <<- weights %>%
  #   mutate(w_pass = w_pass / sum(w_pass),
  #          w_run  = w_run  / sum(w_run)) %>%
  #   mutate(across(c(w_pass, w_run), ~ round(.x, 2)))
}
weights <- ol_weights()

# filter and join
ol_team <- nfl_depth_full %>%
  dplyr::filter(pos %in% c("LT1", "LG1", "C1", "RG1", "RT1")) %>% 
  left_join(preds_out_select, by=c("player")) %>%
  left_join(weights, by = c("pos" = "pos")) %>%
  mutate(
    w_pass_eff = if_else(is.na(pred_pass_block_2025_cal), 0, w_pass),
    w_run_eff  = if_else(is.na(pred_run_block_2025_cal),  0, w_run)
  ) %>%
  group_by(team) %>%
  summarise(
    team_pass_block = ifelse(sum(w_pass_eff) > 0,
                             sum(w_pass_eff * pred_pass_block_2025_cal, na.rm = TRUE) / sum(w_pass_eff),
                             NA_real_),
    team_run_block  = ifelse(sum(w_run_eff) > 0,
                             sum(w_run_eff  * pred_run_block_2025_cal,  na.rm = TRUE) / sum(w_run_eff),
                             NA_real_)
  ) %>%
  ungroup() %>%
  mutate(
    pass_rank = dense_rank(desc(team_pass_block)),
    run_rank  = dense_rank(desc(team_run_block)),
    team_ol_overall = 0.60 * team_pass_block + 0.40 * team_run_block,
    overall_rank = dense_rank(desc(team_ol_overall))
  ) %>%
  arrange(overall_rank)

save(ol_team, file = "./05_outputs/season_projections/ol_team.Rdata")


# 2.0 load to google sheets -----------------------------------------------


library(googlesheets4)

# write to an existing sheet by URL
sheet_url <- "https://docs.google.com/spreadsheets/d/1aoYlyW1YQE0qjZ57vofKoHYl-Zt8Y0uMbWIBsgnivxw/edit?gid=1344770493#gid=1344770493"
sheet_write(ol_team, ss = sheet_url, sheet = "OL Rankings")

sheet_url <- "https://docs.google.com/spreadsheets/d/1aoYlyW1YQE0qjZ57vofKoHYl-Zt8Y0uMbWIBsgnivxw/edit?gid=887398306#gid=887398306"
sheet_write(preds_out, ss = sheet_url , sheet = "Players")
