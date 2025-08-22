# Load libraries
library(tidyverse)
library(nflverse)
library(tidymodels)

# Load play-by-play data (2020-2024)
pbp <- load_pbp(seasons = 2020:2024)

# Load roster data to identify rookies
rosters <- load_rosters(seasons = 2020:2025)

# Load depth charts
depth_charts <- load_depth_charts(2020:2024) %>% 
  mutate(depth_team = as.numeric(depth_team))

# Calculate player depth chart position
player_depth <- depth_charts %>% 
  group_by(gsis_id) %>%
  summarize(avg_depth = round(mean(depth_team, na.rm = TRUE), digits = 1))

# espn depth charts from espn depth charts script
load("./01_data/depth_chart/espn_depth_chart_2025.Rdata")

# load cfb qb from cfb test
load("./01_data/cfb/cfb_pbp_qb_weighted.Rdata")

# Load draft data to confirm rookie status
draft_picks <- load_draft_picks(seasons = 2020:2025)

# Define rookies function
def_rookies <- function() {
  rookies <<- rosters %>%
    filter(status == "ACT") %>%
    filter(years_exp == 0) %>% 
    select(1:3, 6, 7, 10:14) %>%
    # Filter for fantasy positions
    filter(position %in% c("QB", "RB", "WR", "TE")) %>% 
    # Add depth chart position
    left_join(player_depth, by = "gsis_id") %>% 
    # Join with draft data for draft position
    left_join(draft_picks %>% select(2, 3, 8), by = c("full_name" = "pfr_player_name")) %>% 
    # Add join column for NFL fpts
    mutate(join = paste(season, gsis_id, sep = "_"))
}
def_rookies()

# Calculate QB fantasy points by season
qb_fpts_pbp <- function() {
  # Load regular season data
  qb_pbp <- pbp %>% 
    filter(season_type == "REG") %>% 
    group_by(season, passer, passer_id, posteam) %>% 
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
      posteam = last(posteam), 
      season = last(season),
      .groups = "drop"
    ) %>% 
    mutate(join = paste(season, passer_id, sep = "_"))
  
  # Get rusher rushing stats
  rusher_pbp <- pbp %>% 
    group_by(season, rusher, rusher_id) %>% 
    summarise(
      rush_attempt = sum(rush_attempt, na.rm = TRUE),
      rushing_yards = sum(rushing_yards, na.rm = TRUE),
      rush_touchdown = sum(rush_touchdown, na.rm = TRUE),
      fumble = sum(fumble, na.rm = TRUE),
      .groups = "drop"
    ) %>% 
    drop_na() %>% 
    mutate(join = paste(season, rusher_id, sep = "_")) %>% 
    select(-season)
  
  # Join stats and calculate fpts (DraftKings scoring)
  qb_fpts <<- qb_pbp %>% 
    left_join(rusher_pbp, by = "join") %>% 
    mutate(across(c(rush_attempt.x, rush_attempt.y, 
                    rushing_yards.x, rushing_yards.y, 
                    rush_touchdown.x, rush_touchdown.y, 
                    fumble.x, fumble.y), 
                  ~ replace_na(.x, 0))) %>% 
    mutate(
      rush_attempt = rush_attempt.x + rush_attempt.y, 
      rushing_yards = rushing_yards.x + rushing_yards.y,
      rush_touchdown = rush_touchdown.x + rush_touchdown.y,
      fumble = fumble.x + fumble.y
    ) %>% 
    select(-c("rush_attempt.x", "rush_attempt.y", "rushing_yards.x", "rushing_yards.y", 
              "rush_touchdown.x", "rush_touchdown.y", "fumble.x", "fumble.y", "join")) %>% 
    relocate(c("rush_attempt", "rushing_yards", "rush_touchdown", "fumble"), .after = qb_scramble) %>% 
    drop_na(passer) %>% 
    mutate(
      fpts = pass_touchdown * 4 +
        passing_yards * 0.04 +
        interception * -1 +
        rushing_yards * 0.1 +
        rush_touchdown * 6 +
        fumble * -1, 
      fpts_ntile = ntile(fpts, 100)
    ) %>% 
    arrange(-fpts) %>% 
    mutate(join = tolower(paste(season, passer_id, sep = "_"))) %>%
    select(-season) %>% 
    relocate(c("fpts", "fpts_ntile"), .after = posteam) %>% 
    relocate(c("snaps", "epa", "epa_per_play"), .after = fpts_ntile)
}
qb_fpts_pbp()

# QB position group split
def_rookies_qb <- function() {
  rookies_qb <<- rookies %>% 
    filter(position == "QB") %>% 
    left_join(qb_fpts, by = "join") %>% 
    arrange(-fpts) %>% 
    left_join(cfb_pbp_qb_weighted, by = c("full_name" = "passer_player_name"))
}
def_rookies_qb()

# Prepare training data (seasons before 2025)
train_data <- rookies_qb %>%
  filter(season < 2025) %>%
  filter(!is.na(fpts)) %>%
  select(fpts, starts_with("cfb_"), height, weight, round, pick, avg_depth) %>%
  mutate(across(where(is.numeric), ~replace_na(.x, median(.x, na.rm = TRUE))))

# Prepare 2025 data for prediction
pred_data <- rookies_qb %>%
  filter(season == 2025) %>%
  select(full_name, gsis_id, fpts, starts_with("cfb_"), height, weight, round, pick, avg_depth) %>%
  mutate(across(where(is.numeric) & !c(fpts), ~replace_na(.x, median(.x, na.rm = TRUE))))

# Define the model
lm_spec <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

# Define preprocessing recipe (corrected)
recipe <- recipe(fpts ~ ., data = train_data) %>%
  # Only remove columns that might exist
  step_rm(any_of(c("full_name", "gsis_id", "join", "passer", "passer_id", "rusher", "rusher_id"))) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_impute_median(all_numeric_predictors())

# Create workflow
workflow <- workflow() %>%
  add_model(lm_spec) %>%
  add_recipe(recipe)

# Set up cross-validation
set.seed(123)
folds <- vfold_cv(train_data, v = 5)

# Fit and evaluate the model
fit_results <- workflow %>%
  fit_resamples(folds, metrics = metric_set(rmse, mae, rsq))

# Collect metrics
collect_metrics(fit_results)

# Make predictions for 2025 rookies
final_fit <- workflow %>%
  fit(data = train_data)

predictions <- final_fit %>%
  predict(new_data = pred_data) %>%
  bind_cols(pred_data %>% select(full_name, gsis_id, fpts)) %>%
  rename(pred_fpts = .pred)

# View predictions
predictions %>%
  select(full_name, gsis_id, pred_fpts, fpts) %>%
  arrange(desc(pred_fpts))

# Other positions (commented out, but no changes needed)
{
  rookies_rb <- rookies %>% filter(position == "RB")
  rookies_wr <- rookies %>% filter(position == "WR")
  rookies_te <- rookies %>% filter(position == "TE")
  }