#load packages
suppressMessages({
  library(tidyverse) #metapackage
  library(nflverse) #functions to efficiently access NFL pbp data
  library(reshape2) #flexibly Reshape Data
  library(ggrepel) #Automatically Position Non-Overlapping Text Labels with 'ggplot2'
  
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
         merge_off = paste0(posteam, year, week_minus1),
         merge_def = paste0(defteam, year, week_minus1))

#load pff qb
qbs_pff <- read.csv("./Training_Data/position_groups/qbs.csv") 

qbs_pff_test <- qbs_pff %>%
  mutate(name = player) %>% 
  separate(name, into = c("first_name", "last_name"), sep=" ") %>% 
  mutate(merge = paste0(last_name, year, week)) %>% 
  filter(attempts > 10)

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
    grades_tackle = round(weighted.mean(grades_tackle, snap_counts_defense, na.rm = T), digits = 2),
    
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

#blitz
def_blitz_pos <- def %>% 
  group_by(team_name, year, week, position) %>% 
  summarise(
    snaps = sum(snap_counts_pass_rush.x)
  )%>% 
  mutate(merge = paste0(team_name, year, week))

def_blitz <- def %>% 
  group_by(team_name, year, week) %>% 
  summarise(
    snaps = sum(snap_counts_pass_rush.x)
  ) %>% 
  mutate(merge = paste0(team_name, year, week)) %>% 
  ungroup() %>% 
  select(snaps, merge) %>% 
  rename("total_snaps"=snaps)

def_blitz_pos <- def_blitz_pos %>% 
  left_join(def_blitz, by=c("merge")) %>% 
  mutate(blitz = round(snaps/total_snaps, digits = 2)) %>% 
  filter(position == "LB" | position == "CB" | position == "S") %>% 
  group_by(team_name, year, week) %>% 
  summarise(
    blitz = sum(blitz)
  ) %>% 
  mutate(merge_def = paste0(team_name, year, week))

def_blitz_pos %>% 
  filter(week==17 & year==2021) %>% 
  arrange(-blitz) %>% 
  view()

#pbe
pbe <- read.csv("./Training_Data/position_groups/pbe.csv")

pbe <- pbe %>% 
  mutate(merge_off = paste0(team_name, year, week))

#merge all qb data
qbs <- qb_pbp %>% 
  left_join(qbs_pff_test, by = c("merge")) %>% 
  left_join(pbe, by = c("merge_off")) %>%
  left_join(def_pass, by = c("merge_def")) %>% 
  left_join(def_blitz_pos, by = c("merge_def"))

names <- tibble(names(qbs))
names

#write.csv(qbs, file = "pbp_pff_qbs.csv")

qbs_select <- qbs %>% 
  select(grades_pass.x, attempts.x, btt_rate, twp_rate, blitz_grades_pass, no_blitz_grades_pass, pressure_dropbacks_percent, pressure_grades_pass, deep_big_time_throws, pbe, 
         grades_defense, true_pass_set_grades_pass_rush_defense, grades_coverage_defense, man_grades_coverage_defense, man_catch_rate, 
         man_yards_per_coverage_snap, prp, blitz,
         fpts) %>%
  drop_na() %>% 
  mutate(across(where(is.character),as.numeric)) %>% 
  mutate(blitz_grades_pass_sq_blitz_rate = round((blitz_grades_pass^2)*blitz, digits = 0))

head(qbs_select)

qbs_select_cor <- qbs_select[,c(5:dim(qbs_select)[2])]

qbs_select_cor %>% 
  cor() %>%
  melt() %>%
  ggplot(aes(Var1, Var2, fill=value)) +
  geom_tile(color='white') +
  scale_fill_distiller(palette = 'GnBu', direction = 1) +
  geom_text(aes(label=paste(round(value,2)*100,'%')), size=2.5, color='black') +
  labs(x='',y='',fill='correlations', title='Relationship between QBs variables') +
  theme(axis.text.x = element_text(angle = 90, vjust = .5))

normalize <- function(x){
  return( round((x - min(x,na.rm = T))/( max(x, na.rm = T) - min(x, na.rm = T)), digits = 3 ))
}

qbs_norm <- qbs_select_cor

for(i in 1:length(qbs_norm)){
  qbs_norm[,i] = normalize(qbs_norm[,i])
}

#lets make a plot
qbs_select_colors <- qbs_select %>% 
  left_join(teams_colors_logos, by=c("posteam"="team_abbr"))

ggplot(qbs_select_colors, aes(x=blitz_grades_pass_sq_blitz_rate, y=fpts)) +
  geom_point(aes(color=qbs_select_colors$team_color)) +
  geom_text_repel(aes(label=paste0(passer,"\n", game_date,"\n", defteam))) +
  labs(
    title = "High Blitz Graded QBs vs High Blitz Defense Correlation?",
    caption = "Twitter: Its_MikeF \n data from pff and nflverse"
  )
