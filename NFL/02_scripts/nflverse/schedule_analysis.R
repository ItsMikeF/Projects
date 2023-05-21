# Lets mess around with the schedule

# Load packages
suppressMessages({
  library(nflverse)
  library(tidyr)
  library(stringr)
  library(purrr)
  library(dplyr)
  library(gt)
  library(gtExtras)
})
#options(digits = 3)

# 1.0 Schedule ----------------------------------------------------------------


# Load the schedule
schedules <- load_schedules(2023)

# Make a list of nfl schedules by team
nfl_schedules <- map(teams_colors_logos$team_abbr, 
                     function(x) schedules %>% 
                       filter(grepl(x, game_id) & game_type == "REG") %>% 
                       mutate(rest = round(difftime(gameday, lag(gameday)), digits = 1), 
                              rest_gained = rest-7, 
                              opponent = if_else(away_team==x, home_team, away_team))
                     )
nfl_schedules <- set_names(nfl_schedules, teams_colors_logos$team_abbr)

# lets look one schedule
nfl_schedules$ARI %>% mutate(opponent = if_else(away_team=="ARI", home_team, away_team)) %>% 
  select(game_id, week, opponent)


# 2.0 Rest Days ---------------------------------------------------------------


# Make a dataframe of the rest gained days
rest_days_gained <- data.frame()

for (i in seq_along(teams_colors_logos$team_abbr)) {
  rest_days_gained[i,1] <- teams_colors_logos$team_abbr[i]
  rest_days_gained[i,2] <- sum(nfl_schedules[[i]]$rest_gained, na.rm=T)
}

# investigaing why 11/13 produces 0.04 
test <- nfl_schedules[[11]] %>% select(gameday, rest, rest_gained)

difftime(test$gameday[9],test$gameday[8])
difftime("2022-11-13","2022-11-06")

# Inspect a few columns 
nfl_schedules[[4]]$gameday
nfl_schedules[[4]]$rest
nfl_schedules[[4]]$rest_gained


# 3.0 Bye Weeks -----------------------------------------------------------

# Find which gameweek is the bye week for each team
bye_weeks <- data.frame()

for (i in c(1:18,20:26,28:29,31:32,34:36)) {
  bye_weeks[i,1] <- teams_colors_logos$team_abbr[i]
  bye_weeks[i,2] <- which(nfl_schedules[[i]]$rest == max(nfl_schedules[[i]]$rest, na.rm = T))
}

bye_weeks <- bye_weeks %>% drop_na() %>% rename(team = V1, bye_week = V2)

bye_weeks %>% 
  group_by(bye_week) %>% 
  summarise(n=n())


# 4.0 Add run defense data ----------------------------------------------------
# must run the dfs_nfl_defense file

schedules_def_rush <- schedules %>% 
  left_join(pbp_def %>% select(defteam, def_rush_epa, def_rush_epa_rank), by=c("away_team"="defteam")) %>% 
  rename(def_rush_epa_away = def_rush_epa, def_rush_epa_rank_away = def_rush_epa_rank) %>% 
  
  left_join(pbp_def %>% select(defteam, def_rush_epa, def_rush_epa_rank), by=c("home_team"="defteam")) %>% 
  rename(def_rush_epa_home = def_rush_epa, def_rush_epa_rank_home = def_rush_epa_rank) %>% 
  
  select(game_id, week, away_team, def_rush_epa_away, def_rush_epa_rank_away, home_team, def_rush_epa_home, def_rush_epa_rank_home)

#make a list of each teams schedule with the defenses
schedules_def_list <- map(teams_colors_logos$team_abbr, 
                          function(x) schedules_def_rush %>% 
                            filter(grepl(x, game_id)) %>% 
                            dplyr::mutate(opponent = if_else(away_team==x, home_team, away_team), 
                                          opponent_rush_def = if_else(opponent == away_team, def_rush_epa_away, def_rush_epa_home), 
                                          opponent_rush_def_rank = if_else(opponent == away_team, def_rush_epa_rank_away, def_rush_epa_rank_home), 
                                          team = if_else(away_team==x, away_team, home_team))
                          )

#name the list elements
schedules_def_list <- set_names(schedules_def_list, teams_colors_logos$team_abbr)

#combine the list elements into a dataframe
schedules_def_df <- bind_rows(schedules_def_list) %>% drop_na()

#combine for sos def rush
sos_def_rush <- schedules_def_df %>% 
  group_by(team) %>% 
  summarise(sos_def_rush = round(mean(opponent_rush_def, na.rm = T), digits = 3),
            sos_def_rush_rank = round(mean(opponent_rush_def_rank, na.rm = T), digits = 1)) %>% 
  ungroup() %>% 
  arrange(sos_def_rush_rank)


# 5.0 Pass defense --------------------------------------------------------

schedules_def_pass <- schedules %>% 
  left_join(pbp_def %>% select(defteam, def_pass_epa, def_pass_epa_rank), by=c("away_team"="defteam")) %>% 
  rename(def_pass_epa_away = def_pass_epa, def_pass_epa_rank_away = def_pass_epa_rank) %>% 
  
  left_join(pbp_def %>% select(defteam, def_pass_epa, def_pass_epa_rank), by=c("home_team"="defteam")) %>% 
  rename(def_pass_epa_home = def_pass_epa, def_pass_epa_rank_home = def_pass_epa_rank) %>% 
  
  select(game_id, week, away_team, def_pass_epa_away, def_pass_epa_rank_away, home_team, def_pass_epa_home, def_pass_epa_rank_home)

#make a list of each teams schedule with the defenses
schedules_def_list_pass <- map(teams_colors_logos$team_abbr, 
                          function(x) schedules_def_pass %>% 
                            filter(grepl(x, game_id)) %>% 
                            dplyr::mutate(opponent = if_else(away_team==x, home_team, away_team), 
                                          opponent_pass_def = if_else(opponent == away_team, def_pass_epa_away, def_pass_epa_home), 
                                          opponent_pass_def_rank = if_else(opponent == away_team, def_pass_epa_rank_away, def_pass_epa_rank_home), 
                                          team = if_else(away_team==x, away_team, home_team))
)

#name the list elements
schedules_def_list_pass <- set_names(schedules_def_list_pass, teams_colors_logos$team_abbr)

#combine the list elements into a dataframe
schedules_def_df <- bind_rows(schedules_def_list_pass) %>% drop_na()

#combine for sos def pass
sos_def_pass <- schedules_def_df %>% 
  group_by(team) %>% 
  summarise(sos_def_pass = round(mean(opponent_pass_def, na.rm = T), digits = 3),
            sos_def_pass_rank = round(mean(opponent_pass_def_rank, na.rm = T), digits = 1)) %>% 
  ungroup() %>% 
  arrange(sos_def_pass_rank)


# 6.0 SOS -----------------------------------------------------------------

sos <- sos_def_pass %>% left_join(sos_def_rush, by =c("team")) %>% 
  mutate(def_rank = round((sos_def_pass_rank + sos_def_rush_rank)/2, digits = 1)) %>% 
  arrange(def_rank) %>% 
  left_join(teams_colors_logos %>% select(team_abbr, team_logo_espn), by=c("team"="team_abbr")) %>% 
  relocate(team_logo_espn, .after = team) %>% 
  rename(logo = team_logo_espn)

sos %>% 
  select(team, logo, sos_def_pass_rank, sos_def_rush_rank, def_rank) %>% 
  gt() %>% 
  gt_theme_dark() %>% 
  gt_img_rows(columns = logo, height = 50)


# 7.0 Split table into 2 tables side by side ------------------------------


#lets look at mean adp movement by team
tab1 <- sos %>%
  select(team, logo, sos_def_pass_rank, sos_def_rush_rank, def_rank) %>% 
  slice(1:16) %>% 
  gt() %>% 
  tab_header(title = "NFL Strength of Schedule") %>% 
  gt_img_rows(columns=logo) %>% 
  tab_footnote(footnote = "Data from nflfastr") %>% 
  data_color(columns = def_rank, colors = scales::col_numeric(
    palette = c("red", "green"),
    domain = c(min(sos$def_rank), max(sos$def_rank)))) %>% 
  gt_theme_dark()

#lets look at mean adp movement by team
tab2 <- sos %>%
  select(team, logo, sos_def_pass_rank, sos_def_rush_rank, def_rank) %>% 
  slice(17:32) %>% 
  gt() %>% 
  tab_header(title = "NFL Strength of Schedule") %>% 
  gt_img_rows(columns=logo) %>% 
  tab_footnote(footnote = "Data from nflfastr") %>% 
  data_color(columns = def_rank, colors = scales::col_numeric(
    palette = c("red", "green"),
    domain = c(min(sos$def_rank), max(sos$def_rank)))) %>% 
  gt_theme_dark()

listed_tables <- list(tab1, tab2)

#split the table into two tables side by side with 16 rows each
gt_two_column_layout(listed_tables,
                     output = "viewer")
