# group pff and pbp def data by week

library(tidyverse)

load("./01_data/training_data/position_groups/def.RData")

pff_def <- def %>% 
  group_by(team_name, year, week) %>% 
  summarise(def = round(weighted.mean(grades_defense.x, snap_counts_defense), digits = 1),
            rdef = round(weighted.mean(grades_run_defense.x, snap_counts_run_defense), digits = 1),
            tack = round(weighted.mean(grades_tackle.x, snap_counts_defense, na.rm = TRUE), digits = 1),
            prsh = round(weighted.mean(grades_pass_rush_defense.x, snap_counts_pass_rush.x), digits = 1),
            cov = round(weighted.mean(grades_coverage_defense.x, snap_counts_coverage.x), digits =1), 
            .groups = "drop") %>% 
  mutate(team_name = gsub('ARZ','ARI', team_name), 
         team_name = gsub('BLT','BAL', team_name), 
         team_name = gsub('CLV','CLE', team_name), 
         team_name = gsub('HST','HOU', team_name), 
         #team_name = gsub('JAX','JAC', team_name), 
         team_name = gsub('LA','LAR', team_name), 
         team_name = gsub('LARC','LAC', team_name), 
         join = paste0(team_name, year, week))

pbp <- load_pbp(2014:2022)

#def pass epa
pbp_def_pass <- pbp %>% 
  filter(pass == 1 &
           wp > 0.1 &
           wp < 0.9 &
           half_seconds_remaining > 120) %>% 
  group_by(defteam, game_date, week) %>% 
  summarize(def_pass_epa = round(mean(epa), digits = 3),
            n_plays_pass = n(), 
            .groups = "drop") %>% 
  mutate(def_pass_epa_ntile = ntile(def_pass_epa, 100),
         year = year(game_date), 
         join = paste0(defteam, year, week))
  

#def rush epa
pbp_def_rush <- pbp %>% 
  filter(rush == 1 &
           wp > 0.1 &
           wp < 0.9 &
           half_seconds_remaining > 120) %>% 
  group_by(defteam, game_date, week) %>% 
  summarize(def_rush_epa = round(mean(epa), digits = 3),
            n_plays_rush = n(), 
            .groups = "drop") %>% 
  mutate(def_rush_epa_ntile = ntile(def_rush_epa,100), 
         year = year(game_date), 
         join = paste0(defteam, year, week))

# join all data
defense <- pff_def %>% 
  left_join(pbp_def_pass %>% select(4,5,6,8), by=c("join")) %>% 
  left_join(pbp_def_rush %>% select(4,5,6,8), by=c("join")) %>% 
  drop_na()

# save defense object
save(defense, file = "./01_data/training_data/position_groups/defense.RData")
