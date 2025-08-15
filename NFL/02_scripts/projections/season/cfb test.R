# college football data


# 1.0 load packages and data ----------------------------------------------


# Load libraries
suppressMessages({
  # load packages
  library(nflverse)
  library(cfbfastR)
  library(tidyverse)
  library(tidymodels)
})


# load pbp
#cfb_pbp <- load_cfb_pbp(seasons = 2015:2024)
#save(cfb_pbp, file = "./01_data/cfb/cfb_pbp_2015_2024.Rdata")

# load cfb pbp from local save
load("./01_data/cfb/cfb_pbp_2015_2024.Rdata")


# 2.0 QB -----------------------------------------------------------------


# Step 1: Calculate yearly sums
cfb_yearly_stats <- cfb_pbp %>%
  group_by(passer_player_name, year) %>% # removed pos team
  summarize(
    cfb_completions_year = sum(completion, na.rm = TRUE),
    cfb_pass_attempts_year = sum(pass_attempt, na.rm = TRUE),
    cfb_pass_yards_year = sum(yds_receiving, na.rm = TRUE),
    cfb_pass_td_year = sum(pass_td, na.rm = TRUE),
    cfb_interception_year = sum(int, na.rm = TRUE),
    epa_year = sum(EPA, na.rm = TRUE),
    .groups = "drop"
  )

# Step 2: Calculate the total weight sum per group
weight_sums <- cfb_yearly_stats %>%
  group_by(passer_player_name) %>% # removed pos team
  summarise(n_seasons = n(), .groups = "drop") %>%
  drop_na(passer_player_name) %>% 
  mutate(total_weight = n_seasons * (n_seasons + 1) / 2)

# Step 3: Join back and calculate weighted means with corrected weights
cfb_pbp_qb_weighted <- cfb_yearly_stats %>%
  group_by(passer_player_name) %>% # removed pos team
  arrange(year) %>%
  left_join(weight_sums, by = c("passer_player_name")) %>% # removed pos team
  mutate(
    season_order = row_number(),  # Assign order (1 for first season, up to n for last)
    weight = (season_order) / total_weight,  # Corrected to increase with season_order
    weighted_completions = cfb_completions_year * weight
  ) %>%
  summarise(
    cfb_completions_wmean = round(sum(cfb_completions_year * weight, na.rm = TRUE), digits = 1),
    cfb_pass_attempts_wmean = round(sum(cfb_pass_attempts_year * weight, na.rm = TRUE), digits = 1),
    cfb_pass_yards_wmean = round(sum(cfb_pass_yards_year * weight, na.rm = TRUE), digits = 1),
    cfb_pass_td_wmean = round(sum(cfb_pass_td_year * weight, na.rm = TRUE), digits = 1),
    cfb_interception_wmean = round(sum(cfb_interception_year * weight, na.rm = TRUE), digits = 1),
    epa_wmean = round(sum(epa_year * weight, na.rm = TRUE), digits = 2),
    .groups = "drop"
  )

# Step 4: Add total sums for comparison
cfb_pbp_qb_weighted <- cfb_pbp_qb_weighted %>%
  left_join(
    cfb_yearly_stats %>%
      group_by(passer_player_name) %>% # removed pos team
      summarise(
        cfb_completions_sum = round(sum(cfb_completions_year, na.rm = TRUE), digits = 1),
        cfb_pass_attempts_sum = round(sum(cfb_pass_attempts_year, na.rm = TRUE), digits = 1),
        cfb_pass_yards_sum = round(sum(cfb_pass_yards_year, na.rm = TRUE), digits = 1),
        cfb_pass_td_sum = round(sum(cfb_pass_td_year, na.rm = TRUE), digits = 1),
        cfb_interception_sum = round(sum(cfb_interception_year, na.rm = TRUE), digits = 1),
        epa_sum = round(sum(epa_year, na.rm = TRUE), digits = 2),
        .groups = "drop"
      ),
    by = c("passer_player_name") # removed pos team
  ) %>%
  
  mutate(epa_per_play_wmean = round(epa_wmean / cfb_pass_attempts_wmean, digits = 3), 
         cfb_comp_percent = round(cfb_completions_sum / cfb_pass_attempts_sum, digits = 3),
         cfb_ypa = round(cfb_pass_yards_sum / cfb_pass_attempts_sum, digits = 2),
         cfb_td_int_ratio = round(cfb_pass_td_sum/ cfb_interception_sum, digits = 2),
         
         cfb_fpts = 
           cfb_pass_td_sum * 4 +
           cfb_pass_yards_sum * .04 +
           cfb_interception_sum * -1) %>% 
  
  relocate(cfb_fpts, .after = passer_player_name) %>% 
  drop_na(passer_player_name) %>% 
  filter(cfb_pass_attempts_sum > 100) %>% 
  arrange(-cfb_fpts)

# save
save(cfb_yearly_stats, file = "./01_data/cfb/cfb_yearly_stats.Rdata")
save(cfb_pbp_qb_weighted, file = "./01_data/cfb/cfb_pbp_qb_weighted.Rdata")

# troubleshooting code
{# qb lookup a specific name
qb_lookup <- cfb_pbp %>% 
  group_by(passer_player_name, pos_team, def_pos_team, year, week) %>% 
  filter(passer_player_name == "Cam Ward") %>% 
  summarize(
    
    completions = sum(completion, na.rm = T),
    pass_attempt = sum(pass_attempt, na.rm = T), 
    pass_yards = sum(yds_receiving, na.rm = T), 
    pass_touchdown = sum(pass_td, na.rm = T), 
    interception = sum(int, na.rm = T), 
    
    epa = round(sum(EPA, na.rm = T), digits = 2),
    epa_per_play = round(epa/pass_attempt, digits = 2),
    
    .groups = "drop"
    
    ) %>% 
  mutate(fpts = 
           pass_touchdown * 4 +
           pass_yards * .04 +
           interception * -1) %>% 
  arrange(week)


# view game log
qb_week1 <- cfb_pbp %>% 
  filter(passer_player_name == "Cam Ward") %>% 
  filter(week == 1)
}

# 3.0 RB ------------------------------------------------------------------


# rb
cfb_pbp_rb <- cfb_pbp %>% 
  #filter(rush == 1) %>% 
  group_by(rusher_player_name, pos_team) %>% 
  summarise(
    
    epa = round(sum(EPA, na.rm = T), digits = 2),
    snaps = n(),
    epa_per_play = round(epa/snaps, digits = 2),
    
    rush_attempt = sum(rush, na.rm = T),
    rush_yards = sum(yards_gained, na.rm = T),
    rush_touchdown = sum(rush_td, na.rm = T)
    ) %>% 
  mutate(ypa = round(rush_yards / rush_attempt, digits = 2)) %>% 
  drop_na(rusher_player_name) %>% 
  arrange(-rush_yards) %>% 
  filter(snaps > 100)


# 4.0 WR ------------------------------------------------------------------



# 5.0 TE ------------------------------------------------------------------



# 6.0 Teams ---------------------------------------------------------------


# team level plays
plays <- cfb_pbp %>% 
  group_by(pos_team) %>% 
  summarize(epa = round(mean(EPA, na.rm = T), digits = 3),
            n = n(), 
            game_count = n_distinct(game_id)) %>% 
  arrange(-epa) %>% 
  filter(game_count > max(game_count)*0.5) 