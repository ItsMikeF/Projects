#script was made via ChatGPT

# Load necessary packages
library(baseballr)
library(tidyverse)
library(boot)

# Step 1: Gather data -----------------------------------------------------

# Get team rosters from baseballr for the current season
teams <- (mlb_teams(season = 2023, sport_ids = c(1))) %>% 
  select(team_id, team_full_name, team_abbreviation, team_name) %>% 
  mutate(team_abbreviation = replace(team_abbreviation, team_abbreviation == "TB", "TBR"), 
         team_abbreviation = replace(team_abbreviation, team_abbreviation == "CWS", "CHW"),
         team_abbreviation = replace(team_abbreviation, team_abbreviation == "WSH", "WSN"),
         team_abbreviation = replace(team_abbreviation, team_abbreviation == "KC", "KCR"), 
         team_abbreviation = replace(team_abbreviation, team_abbreviation == "ARI", "AZ"), 
         team_abbreviation = replace(team_abbreviation, team_abbreviation == "SD", "SDP"), 
         team_abbreviation = replace(team_abbreviation, team_abbreviation == "SF", "SFG"), 
         team_name = replace(team_name, team_name == "D-backs", "Diamondbacks"))

team_rosters <- map(teams$team_id, ~ mlb_rosters(team_id = .x, season = 2023, roster_type = 'active')) %>% 
  set_names(teams$team_full_name)

rosters <- map_df(team_rosters, ~ data.frame(.x)) %>% 
  select(person_full_name, parent_team_id, position_abbreviation) %>% 
  rename(Name = person_full_name, 
         team_id = parent_team_id, 
         position = position_abbreviation) %>% 
  left_join(teams, by=c("team_id")) %>% 
  mutate(team_abbreviation = replace(team_abbreviation, team_abbreviation == "TB", "TBR"), 
         team_abbreviation = replace(team_abbreviation, team_abbreviation == "CWS", "CHW"),
         team_abbreviation = replace(team_abbreviation, team_abbreviation == "WSH", "WSN"),
         team_abbreviation = replace(team_abbreviation, team_abbreviation == "KC", "KCR"), 
         team_abbreviation = replace(team_abbreviation, team_abbreviation == "ARI", "AZ"))

# Load fg park factors for each stadium
park_factors <- fg_park(2022) %>% 
  select(home_team, basic_5yr) %>% 
  left_join(teams %>% select(team_name, team_abbreviation), by = c("home_team" = "team_name")) %>% 
  relocate(team_abbreviation) %>% 
  select(team_abbreviation, basic_5yr) %>% 
  rename(Team = team_abbreviation, 
         park_factor = basic_5yr)

# Scrape player stats from Fangraphs for the previous season
player_stats <- fg_batter_leaders(2022, 2022, league = "all", qual = 1, ind = 1, exc_p = T) 

# Merge player stats with team rosters to get position information
player_stats <- player_stats %>% 
  select(Name, Team, Age, G, PA, AB, H, R, RBI, BB, BB_pct, SO, K_pct, SB, CS, '1B', '2B', '3B', HR, AVG, OBP, SLG, OPS, ISO, BABIP, 
         HBP, wOBA, wRC_plus, WAR) %>% 
  rename(X1B = '1B', X2B = '2B', X3B = '3B') %>% 
  mutate(fpts = (X1B*3) + (X2B*6) + (X3B*8) + (HR*10) + (BB*3) + (HBP*3) + (RBI*3) + (R*2) + (SB*2)) %>% 
  left_join(rosters %>% select(Name, position), by = "Name") %>%
  left_join(teams %>% select(team_abbreviation, team_id), by=c("Team"="team_abbreviation")) %>%
  left_join(park_factors, by = "Team") %>% 
  relocate(c(position, team_id), .after = Name)

# Scrape game schedules from Baseball-Reference.com for the current season
game_schedules <- mlb_schedule(season = "2023")

game_schedules <- game_schedules %>%
  mutate(date = as.Date(date)) %>% 
  filter(game_type == "R") %>% 
  select(date, venue_name, game_pk, 
         away_team_id = teams_away_team_id, away_team = teams_away_team_name, 
         home_team_id = teams_home_team_id, home_team_ = teams_home_team_name) 

games <- rbind(
  game_schedules %>% select(date, team_id = away_team_id, team_name = away_team), 
  game_schedules %>% select(date, team_id = home_team_id, team_name = home_team_)
)

# Step 2: Clean and preprocess data ---------------------------------------

player_stats <- player_stats %>% filter(AB > 0.2*max(AB)) %>% drop_na()

# Step 3: Develop projection model
# Build a linear regression model to predict a player's wOBA based on past stats
model <- lm(fpts ~ Age + G + AB + AVG + OBP + SLG + OPS + ISO + BABIP + 
              H + HR + BB + BB_pct + SO + K_pct + SB + CS + HBP + wOBA + wRC_plus + WAR,
            data = player_stats)

# Step 4: Validate model
# Use cross-validation to evaluate the accuracy of the model
set.seed(123)
cv_results <- cv.glm(data = player_stats, glmfit = model, K = 10)

# Print the mean squared error and mean absolute error of the cross-validation results
cat("Mean squared error:", mean(cv_results$cvm), "\n")
cat("Mean absolute error:", mean(abs(cv_results$cvm)), "\n")

# Step 5: Apply model to current season
# Merge game schedules with player stats to get projected stats for each game
projected_stats <- games %>% 
  left_join(player_stats, by = c("team_id")) %>% 
  filter(!is.na(position)) %>% # Remove players without a defined position
  group_by(date, Name) %>% 
  summarize(fpts_proj = predict(model, newdata = data.frame(Age,G,AB,AVG,OBP,SLG,OPS,ISO,BABIP,
                                                              H,HR,BB,BB_pct,SO,K_pct,SB,CS,HBP,wOBA,wRC_plus,WAR)))
# Summarize projected stats by player and team for the entire season
player_projections <- projected_stats %>% 
  group_by(Name) %>% 
  summarize(fpts_proj = round(sum(fpts_proj)/100, digits = -1))
