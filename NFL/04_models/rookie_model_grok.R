# nfl rookie model


# 1.0 load packages and data ----------------------------------------------


# Load libraries
suppressMessages({
  library(tidyverse)
  library(nflverse)
  library(tidymodels)
  library(glmnet) # generalized linear model
})

# Load play-by-play data (2020-2024)
pbp <- load_pbp(seasons = 2020:2024)

# Load roster data to identify rookies
rosters <- load_rosters(seasons = 2020:2025)

# load depth charts
depth_charts <- load_depth_charts(2020:2024) %>% 
  mutate(depth_team = as.numeric(depth_team))

# calc player depth chart position
player_depth <- depth_charts %>% 
  group_by(gsis_id, season) %>%
  summarize(avg_depth = round(mean(depth_team, na.rm = T), digits = 1), 
            .groups = "drop") %>% 
  mutate(season_gsis = paste(season, gsis_id, sep = "_"))

# Load draft data to confirm rookie status
draft_picks <- load_draft_picks(seasons = 2020:2025)

# espn depth charts from espn depth charts script
load("./01_data/depth_chart/espn_depth_chart_2025.Rdata")

nfl_depth_full <- nfl_depth_full %>% 
  mutate(team_depth = as.numeric(substr(pos, nchar(pos), nchar(pos))))

# load cfb qb from cfb test
load("./01_data/cfb/cfb_pbp_qb_weighted.Rdata")

# to be added in 
{
  # Load combine data 
  combine <- load_combine(seasons = 2020:2025) # to be added in 
  
  # load nfl schedule
  nfl_schedule <- load_schedules(2025) # to be added in
}

# Filter rookies from rosters
get_rookies <- function() {
  rookies <<- rosters %>%
    filter(status == "ACT") %>%
    filter(years_exp == 0) %>% 
    select(1:3, 6, 7,10:14) %>%
    
    # filter for fantasy positions
    filter(position %in% c("QB", "RB", "WR", "TE")) %>% 
    
    # add join column
    mutate(season_gsis = paste(season, gsis_id, sep = "_")) %>% 
    
    # add depth chart position
    left_join(player_depth %>% select(season_gsis, avg_depth), by = c("season_gsis")) %>% 
    
    # add espn depth chart position, needed for rookies offseason
    left_join(nfl_depth_full %>% select(player, team_depth), 
              by=c("full_name" = "player")) %>% 
    
    # Join with draft data for draft position
    left_join(draft_picks %>% select(2,3,8), by = c("full_name"="pfr_player_name")) %>% 
    
    # add join column for nfl fpts
    mutate(join = paste(season, gsis_id, sep = "_"))
  
}
get_rookies()

# calc nfl qb fpts by season
qb_fpts_pbp <- function() {
  
  # Load regular season data
  qb_pbp <- pbp %>% 
    filter(season_type == "REG") %>% 
    group_by(season, passer, passer_id, posteam) %>% 
    summarize(
      
      games_played = length(unique(game_id[pass_attempt > 0])),
      
      epa = round(sum(qb_epa, na.rm = T), digits = 2),
      snaps = n(),
      epa_per_play = round(epa/snaps, digits = 2),
      
      pass_attempt = sum(pass_attempt, na.rm = T), 
      passing_yards = sum(passing_yards, na.rm = T), 
      pass_touchdown = sum(pass_touchdown, na.rm = T), 
      interception = sum(interception, na.rm = T), 
      
      qb_scramble = sum(qb_scramble, na.rm = T),
      rush_attempt = sum(rush_attempt, na.rm = T),
      rushing_yards = sum(rushing_yards, na.rm = T),
      rush_touchdown = sum(rush_touchdown, na.rm = T),
      
      fumble = sum(fumble, na.rm = T),
      
      # use to get last non-missing values
      posteam = last(posteam), 
      season = last(season)
      
    ) %>% 
    ungroup() %>% 
    mutate(join = paste(season, passer_id, sep = "_"))
  
  # Get rusher rushing stats
  rusher_pbp <- pbp %>% 
    group_by(season, rusher, rusher_id) %>% 
    summarise(
      
      rush_attempt = sum(rush_attempt, na.rm = T),
      rushing_yards = sum(rushing_yards, na.rm = T),
      rush_touchdown = sum(rush_touchdown, na.rm = T),
      
      fumble = sum(fumble, na.rm = T)) %>% 
    
    drop_na() %>% 
    ungroup() %>% 
    mutate(join = paste(season, rusher_id, sep = "_")) %>% 
    select(-c("season"))
  
  # join stats and calc fpts, dk scoring
  qb_fpts <<- qb_pbp %>% 
    left_join(rusher_pbp, by=c("join")) %>% # add passer + rusher stat 
    
    # add zeros so that the columns can be added
    mutate(across(c(rush_attempt.x, rush_attempt.y, 
                    rushing_yards.x, rushing_yards.y, 
                    rush_touchdown.x, rush_touchdown.y, 
                    fumble.x, fumble.y), 
                  ~ replace_na(., 0))) %>% 
    
    # add the columns from scrambles + designed rushes
    mutate(rush_attempt = rush_attempt.x + rush_attempt.y, 
           rushing_yards = rushing_yards.x + rushing_yards.y,
           rush_touchdown = rush_touchdown.x + rush_touchdown.y,
           fumble = fumble.x + fumble.y) %>% 
    
    # remove duplicated columns
    select(-c("rush_attempt.x", "rush_attempt.y", "rushing_yards.x", "rushing_yards.y", 
              "rush_touchdown.x", "rush_touchdown.y", "fumble.x", "fumble.y", "join")) %>% 
    
    relocate(c("rush_attempt", "rushing_yards", "rush_touchdown", "fumble"), .after = qb_scramble) %>% 
    
    drop_na(passer) %>% 
    mutate(
      #big_rush = ifelse(rushing_yards > 100, 1,0), removed for season long projection
      #big_pass = ifelse(passing_yards > 300, 1,0), removed for season long projection
      fpts = 
        
        #big_pass * 3 + removed for season long projection
        #big_rush * 3 + removed for season long projection
        
        pass_touchdown * 4 +
        passing_yards * .04 +
        interception * -1 +
        
        rushing_yards * .1 +
        rush_touchdown * 6 +
        fumble * -1, 
      
      fpts_ntile = ntile(fpts, 100)
    ) %>% 
    arrange(-fpts) %>% 
    
    mutate(join = tolower(paste(season, passer_id, sep = "_"))) %>%
    
    # remove season to prevent duplicating
    select(-season) %>% 
    
    relocate(c("fpts", "fpts_ntile"), .after = posteam) %>% 
    relocate(c("snaps", "epa", "epa_per_play"), .after = fpts_ntile)
  
}
qb_fpts_pbp()

# nfl rookie qbs
get_rookies_qb <- function() {
  
  # filter for qbs
  rookies_qb <- rookies %>% 
  filter(position == "QB")
  
  # filter for 2025 rooky qb that are either 1s or 2s
  rookies_qb_2025 <- rookies_qb %>% 
    filter(season == 2025) %>% 
    filter(!is.na(team_depth)) %>% 
    filter(team_depth < 1.5)
  
  # filter out back ups
  rookies_qb <- rookies_qb %>% 
    filter(season < 2025) %>% 
    filter(avg_depth < 1.5)
  
  rookies_qb <- rbind(rookies_qb, rookies_qb_2025) %>% 
    mutate(avg_depth = coalesce(avg_depth, team_depth))
    
  # join nfl qb fpts
  rookies_qb <<- rookies_qb %>% 
  left_join(qb_fpts, by = c("join")) %>% 
  arrange(-fpts) %>% 
  
  # add cfb stats
  left_join(cfb_pbp_qb_weighted, by = c("full_name" = "passer_player_name"))
}
get_rookies_qb()


# 4.0 grok time -----------------------------------------------------------


# Prepare training data (excluding 2025 and rows with NA in target stats)
train_data <- rookies_qb %>%
  filter(season < 2025) %>%
  filter(complete.cases(select(., fpts, snaps, epa, epa_per_play, pass_attempt, passing_yards, pass_touchdown, interception))) %>%
  select(fpts, snaps, epa, epa_per_play, pass_attempt, passing_yards, pass_touchdown, interception,
         cfb_fpts, cfb_ypa, cfb_td_int_ratio, height, weight, avg_depth) %>%
  mutate(across(where(is.numeric), ~replace_na(.x, median(.x, na.rm = TRUE))))

# Prepare 2025 data for prediction
pred_data <- rookies_qb %>%
  filter(season == 2025) %>%
  select(full_name, gsis_id, fpts, snaps, epa, epa_per_play, pass_attempt, passing_yards, pass_touchdown, interception,
         cfb_fpts, cfb_ypa, cfb_td_int_ratio, height, weight, avg_depth) %>%
  mutate(across(where(is.numeric) & !c(fpts, snaps, epa, epa_per_play, pass_attempt, passing_yards, pass_touchdown, interception),
                ~replace_na(.x, median(.x, na.rm = TRUE))))

# Function to create and fit model for each stat
fit_stat_model <- function(stat) {
  # Define the model with regularization
  lm_spec <- linear_reg(penalty = 0.1) %>%
    set_engine("glmnet") %>%
    set_mode("regression")
  
  # Define the recipe for the specific stat
  recipe <- recipe(as.formula(paste(stat, "~ .")), data = train_data) %>%
    step_normalize(all_numeric_predictors()) %>%
    step_impute_median(all_numeric_predictors()) # Removed step_rm(full_name, gsis_id)
  
  # Create workflow
  workflow <- workflow() %>%
    add_model(lm_spec) %>%
    add_recipe(recipe)
  
  # Fit the model
  final_fit <- workflow %>%
    fit(train_data)
  
  # Generate predictions
  predictions <- predict(final_fit, pred_data) %>%
    bind_cols(pred_data %>% select(full_name, gsis_id))
  
  # Rename prediction column
  colnames(predictions)[1] <- paste0("pred_", stat)
  return(predictions)
}

# List of stats to predict
stats_to_predict <- c("fpts", "snaps", "epa", "epa_per_play", "pass_attempt", "passing_yards", "pass_touchdown", "interception")

# Fit models and combine predictions
predictions_list <- lapply(stats_to_predict, fit_stat_model)
prediction_results <- reduce(predictions_list, full_join, by = c("full_name", "gsis_id"))

# Display the results
print(prediction_results)



# other positions
{
  rookies_rb <- rookies %>% filter(position == "RB")
  rookies_wr <- rookies %>% filter(position == "WR")
  rookies_te <- rookies %>% filter(position == "TE")
}