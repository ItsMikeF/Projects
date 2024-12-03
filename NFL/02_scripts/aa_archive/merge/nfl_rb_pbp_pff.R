#load packages
suppressMessages({
  library(tidyverse) #metapackage
  library(nflverse) #functions to efficiently access NFL pbp data
})

#load play by play data
pbp <- load_pbp(seasons = 2014:2022)
pbp$year <- as.numeric(substr(pbp$game_date, 1, 4))

#rb historical stats and fpts
rb_pbp <- pbp %>% 
  group_by(rusher , posteam, game_date, defteam, week) %>% 
  summarize(
    
    rushing_yards = sum(rushing_yards, na.rm = T),
    rush_attempt = sum(rush_attempt, na.rm = T),
    rush_touchdown = sum(rush_touchdown, na.rm = T),
    fumble_lost = sum(fumble_lost, na.rm = T),
    
    receptions = sum(complete_pass, na.rm = T),
    receiving_yards = sum(receiving_yards, na.rm = T), 
    rec_touchdown = sum(pass_touchdown, na.rm = T),
    
    epa = round(mean(epa), digits = 2)) %>% 
  mutate(big_rush = ifelse(rushing_yards > 100, 1,0), 
         big_rec = ifelse(receiving_yards > 100, 1,0), 
         fpts = 
           rushing_yards * .1 +
           rush_touchdown * 6 +
           fumble_lost * -1 +
         
           receptions * 1 +
           rec_touchdown * 6 +
           receiving_yards * .1 +
           
           big_rush * 3 +
           big_rec * 3,
         year = as.numeric(substr(game_date, 1, 4)), 
         week_minus1 = week - 1, 
         merge = paste0(unlist(strsplit(rusher, "[.]"))[2], year, week_minus1)) %>% 
  drop_na()

#load pff merge files
def <- read_rds("./01_data/training_data/position_groups/def.Rdata")
names(def)
names(def)[which(grepl(".x", names(def)))]

#rushing defense summary
def_rush <- def %>% 
  group_by(team_name, year, week) %>% 
  summarise(
    grades_defense = round(weighted.mean(grades_defense.x, snap_counts_run_defense, na.rm = T), digits = 2),
    grades_run_defense = round(weighted.mean(grades_run_defense.x, snap_counts_run_defense, na.rm = T), digits = 2),
    avdt = round(weighted.mean(avg_depth_of_tackle, tackles.y, na.rm = T), digits = 2),
    stops = round(weighted.mean(stops.y / player_game_count, snap_counts_run, na.rm = T), digits = 2),
    missed_tackles = round(weighted.mean(missed_tackles.y / player_game_count, snap_counts_run, na.rm = T), digits = 4), 
    forced_fumbles = round(weighted.mean(forced_fumbles.y / player_game_count, snap_counts_run, na.rm = T), digits = 4)
    ) %>% 
  ungroup()

def_rush <- def_rush %>% 
  mutate(merge2 = paste0(def_rush$team_name, def_rush$year, def_rush$week))

#load pff rb merge file
rbs <- read_rds("./01_data/training_data/position_groups/rbs.RData") %>% 
  mutate(name = player) %>% 
  separate(name, into = c("first_name", "last_name"), sep=" ") %>% 
  mutate(designed_ypg = round(designed_yards/player_game_count, digits = 1),
         targets_pg = round(targets/player_game_count, digits = 1),
         merge = paste0(last_name, year, week)) 

rbs_full <- rbs %>% 
  left_join(rb_pbp, by = c("merge"))

rbs_full$merge2 <- paste0(rbs_full$defteam, rbs_full$year.x, rbs_full$week.x)

rbs_full_def <- rbs_full %>% 
  left_join(def_rush, by = c("merge2"))

rbs_full_def <- rbs_full_def %>% 
  drop_na() %>% 
  arrange(-fpts)

names(rbs_full_def)

rbs_select <- rbs_full_def %>% 
  select(player, team_name.x, year.x, week.x, designed_ypg, targets_pg, yprr, grades_run, grades_defense, grades_run_defense, yco_attempt, fpts)
