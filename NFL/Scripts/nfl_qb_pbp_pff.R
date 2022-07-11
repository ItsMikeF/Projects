#load packages
suppressMessages({
  library(tidyverse) #metapackage
  library(nflverse) #functions to efficiently access NFL pbp data
})

#load pbp
pbp <- load_pbp(seasons = 2014:2021)
pbp$year <- as.numeric(substr(pbp$game_date, 1, 4))

#sort for qb pbp
qb_pbp <- pbp %>% 
  group_by(passer ,posteam, game_date, defteam, week) %>% 
  summarize(
    pass_attempt = sum(pass_attempt, na.rm = T),
    passing_yards = sum(passing_yards, na.rm = T),
    pass_touchdown = sum(pass_touchdown, na.rm = T),
    interception = sum(interception, na.rm = T),
    
    rushing_yards = sum(rushing_yards, na.rm = T),
    rush_attempt = sum(rush_attempt, na.rm = T),
    rush_touchdown = sum(rush_touchdown, na.rm = T),
    fumble_lost = sum(fumble_lost, na.rm = T),
    
    epa = round(mean(qb_epa), digits = 2),
    cpoe = round(mean(cpoe, na.rm = T), digits = 2)) %>% 
  filter(pass_attempt > 5) %>% 
  mutate(big_py = ifelse(passing_yards > 300, 1,0), 
         fpts = 
           pass_touchdown * 4 +
           passing_yards * .04 +
           interception * -1 +
           rushing_yards * .1 +
           rush_touchdown * 6 +
           fumble_lost * -1 +
           big_py * 3, 
         year = as.numeric(substr(game_date, 1, 4)), 
         week_minus1 = week - 1, 
         merge = paste0(unlist(strsplit(passer, "[.]"))[2], year, week_minus1), 
         merge_def = paste0(defteam, year, week_minus1))

#load pff qb
qbs_pff <- read.csv("./Training_Data/position_groups/qbs.csv") 

qbs_pff_test <- qbs_pff %>%
  mutate(name = player) %>% 
  separate(name, into = c("first_name", "last_name"), sep=" ") %>% 
  mutate(merge = paste0(last_name, year, week))

#load pff def files
def <- read_csv("./Training_Data/position_groups/def.csv")

#sort pff def
def_pass <- def %>% 
  group_by(team_name, year, week) %>% 
  summarise(
    grades_defense = round(weighted.mean(grades_defense.x, snap_counts_run_defense, na.rm = T), digits = 2),
    grades_pass_rush_defense = round(weighted.mean(grades_pass_rush_defense.x, snap_counts_run_defense, na.rm = T), digits = 2),
    true_pass_set_grades_pass_rush_defense = round(weighted.mean(true_pass_set_grades_pass_rush_defense, true_pass_set_snap_counts_pass_rush, na.rm = T), digits = 2),
    grades_coverage_defense = round(weighted.mean(grades_coverage_defense, snap_counts_run_defense, na.rm = T), digits = 2),
    
    pass_rush_wins = round(weighted.mean(pass_rush_wins, snap_counts_pass_rush.y, na.rm = T), digits = 2),
    true_pass_set_prp = round(weighted.mean(true_pass_set_prp, true_pass_set_snap_counts_pass_rush, na.rm = T), digits = 2),
    true_pass_set_pass_rush_wins = round(weighted.mean(true_pass_set_pass_rush_wins, true_pass_set_snap_counts_pass_rush, na.rm = T), digits = 2),

    man_grades_coverage_defense = round(weighted.mean(man_grades_coverage_defense, man_snap_counts_coverage, na.rm = T), digits = 2),
    man_catch_rate = round(weighted.mean(man_catch_rate, man_snap_counts_coverage, na.rm = T), digits = 2),

    man_avg_depth_of_target = round(weighted.mean(man_avg_depth_of_target, man_snap_counts_coverage, na.rm = T), digits = 2),
    man_yards_after_catch = round(weighted.mean(man_yards_after_catch, man_snap_counts_coverage, na.rm = T), digits = 2),
    man_targets = round(weighted.mean(man_targets, man_snap_counts_coverage, na.rm = T), digits = 2),
    man_yards_per_coverage_snap = round(weighted.mean(man_yards_per_coverage_snap, man_snap_counts_coverage, na.rm = T), digits = 2),
    
    zone_grades_coverage_defense = round(weighted.mean(zone_grades_coverage_defense, zone_snap_counts_coverage, na.rm = T), digits = 2),
    zone_catch_rate = round(weighted.mean(zone_catch_rate, zone_snap_counts_coverage, na.rm = T), digits = 2),

    zone_avg_depth_of_target = round(weighted.mean(zone_avg_depth_of_target, zone_snap_counts_coverage, na.rm = T), digits = 2),
    zone_yards_after_catch = round(weighted.mean(zone_yards_after_catch, zone_snap_counts_coverage, na.rm = T), digits = 2),
    zone_targets = round(weighted.mean(zone_targets, zone_snap_counts_coverage, na.rm = T), digits = 2),
    zone_yards_per_coverage_snap = round(weighted.mean(zone_yards_per_coverage_snap, zone_snap_counts_coverage, na.rm = T), digits = 2),

    lhs_pressures = round(weighted.mean(lhs_prp, lhs_pass_rush_snaps, na.rm = T), digits = 2),
    lhs_prp = round(weighted.mean(lhs_prp, lhs_pass_rush_snaps, na.rm = T), digits = 2),
    
    pressures = round(weighted.mean(pressures, pass_rush_snaps, na.rm = T), digits = 2),
    prp = round(weighted.mean(prp.y, pass_rush_snaps, na.rm = T), digits = 2),
    
    rhs_pressures = round(weighted.mean(rhs_prp, rhs_pass_rush_snaps, na.rm = T), digits = 2),
    rhs_prp = round(weighted.mean(rhs_prp, rhs_pass_rush_snaps, na.rm = T), digits = 2)
  ) %>% 
  mutate(merge_def = paste0(team_name, year, week))

#merge all qb data
qbs <- qb_pbp %>% 
  left_join(qbs_pff_test, by = c("merge")) %>% 
  left_join(def_pass, by = c("merge_def"))
