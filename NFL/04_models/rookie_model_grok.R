# nfl rookie model

# Load libraries
library(tidyverse)
library(nflverse)
library(tidymodels)

# Load play-by-play data (2020-2024)
pbp <- load_pbp(seasons = 2020:2024)

# Load roster data to identify rookies
rosters <- load_rosters(seasons = 2020:2024)

# Load draft data to confirm rookie status
draft_picks <- load_draft_picks(seasons = 2020:2024)

# Load combine data for additional features (e.g., 40-yard dash)
combine <- load_combine(seasons = 2020:2024)

# Filter rookies from rosters
rookies <- rosters %>%
  filter(game_type == "REG" & years_exp == 0) %>%
  select(gsis_id, full_name, position, team, season) %>%
  rename(player_id = gsis_id, player_name = full_name)

# Join with draft data for draft position
rookies <- rookies %>%
  left_join(draft_picks, by = c("player_id" = "pfr_player_id", "season")) %>%
  mutate(draft_round = ifelse(is.na(round), "Undrafted", round))

# Filter play-by-play data for relevant plays
pbp_clean <- pbp %>%
  filter(!is.na(posteam), play_type %in% c("pass", "run")) %>%
  select(season, week, posteam, 
         passer_player_id, rusher_player_id, receiver_player_id,
         passing_yards, pass_touchdown, interception, rushing_yards, rush_touchdown,
         receiving_yards, pass_touchdown, complete_pass, receptions = receiving_yards * 0 + 1)

# Aggregate QB stats
qb_stats <- pbp_clean %>%
  filter(!is.na(passer_player_id)) %>%
  group_by(season, passer_player_id) %>%
  summarize(
    games_played = n_distinct(week),
    passing_yards = sum(passing_yards, na.rm = TRUE),
    pass_tds = sum(pass_touchdown, na.rm = TRUE),
    interceptions = sum(interception, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(player_id = passer_player_id)

# Aggregate RB stats
rb_stats <- pbp_clean %>%
  filter(!is.na(rusher_player_id)) %>%
  group_by(season, rusher_player_id) %>%
  summarize(
    games_played = n_distinct(week),
    rushing_yards = sum(rushing_yards, na.rm = TRUE),
    rush_tds = sum(rush_touchdown, na.rm = TRUE),
    receptions = sum(complete_pass, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(player_id = rusher_player_id)

# Aggregate WR stats
wr_stats <- pbp_clean %>%
  filter(!is.na(receiver_player_id)) %>%
  group_by(season, receiver_player_id) %>%
  summarize(
    games_played = n_distinct(week),
    receiving_yards = sum(receiving_yards, na.rm = TRUE),
    receiving_tds = sum(pass_touchdown, na.rm = TRUE),
    receptions = sum(complete_pass, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(player_id = receiver_player_id)

# Combine stats and filter for rookies
rookie_stats <- rookies %>%
  left_join(qb_stats, by = c("season", "player_id")) %>%
  left_join(rb_stats, by = c("season", "player_id")) %>%
  left_join(wr_stats, by = c("season", "player_id")) %>%
  filter(pos %in% c("QB", "RB", "WR"))

