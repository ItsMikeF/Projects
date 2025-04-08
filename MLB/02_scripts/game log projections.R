# gamelog projections 


# 1.0 load packages and data ----------------------------------------------

# load packages
library(baseballr)
library(mlbplotR)
library(tidyverse)
library(glue)

# load mlb team data
mlb_teams <- mlbplotR::load_mlb_teams()

#load fangraphs leaderboard data

season = year(Sys.Date())

# load pitcher ids
pitchers <- fg_pitcher_leaders(startseason = season, endseason = season)
pitcher_ids <- pitchers %>% select(PlayerName, playerid)

# Load opponent batter K rates 
team <- fg_team_batter(startseason = season, endseason = season)

team_k_fg <- team %>% select(team_name, K_pct, WAR) %>% 
  mutate(WAR = round(WAR, digits = 1),
         K_pct = round(K_pct, digits = 3))

team_k <- team %>% select(team_name, K_pct, WAR) %>% 
  mutate(WAR = round(WAR, digits = 1),
         K_pct = round(K_pct, digits = 3)) %>% 
  mutate(team_name = gsub("ARI", "AZ", team_name), 
         team_name = gsub("SDP", "SD", team_name), 
         team_name = gsub("KCR", "KC", team_name), 
         team_name = gsub("SFG", "SF", team_name), 
         team_name = gsub("TBR", "TB", team_name), 
         team_name = gsub("WSN", "WSH", team_name), 
         team_name = gsub("CHW", "CWS", team_name)) %>% 
  left_join(mlb_teams %>% select(team_abbr, team_id_num), by=c("team_name"="team_abbr"))


# load savant data
savant <- statcast_leaderboards(year = season, 
                                leaderboard = "expected_statistics",
                                player_type = "pitcher") %>% 
  separate(`last_name, first_name`, 
                    into = c("last_name","first_name"), 
                    sep = ", ") %>% 
  unite("fullName", first_name, last_name, sep = " ")

savant_select <- savant %>% select(fullName, est_woba)


# 2.0 aggregate slate data ------------------------------------------------


# define slate date probable pitchers
slate_date <- Sys.Date()

schedule <- mlb_schedule(season = 2025) %>% 
  filter(date == slate_date)

games <- schedule %>% 
  select(game_pk, 
         teams_away_team_name, teams_away_team_id, 
         teams_home_team_name, teams_home_team_id)

slate_probables <- map_df(games$game_pk, mlb_probables) %>% 
  left_join(pitcher_ids, by=c("fullName"="PlayerName")) %>% # join pitcher ids
  select(game_pk, game_date, fullName, playerid, team, team_id) %>% 
  drop_na() %>% 
  left_join(games, by=c("game_pk")) %>% # add opp team id for team k join 
  mutate(opp = if_else(team == teams_away_team_name, teams_home_team_name, teams_away_team_name), 
         opp_id = if_else(team == teams_away_team_name, teams_home_team_id, teams_away_team_id)) %>% 
  select(-c(teams_home_team_name, teams_away_team_name, teams_home_team_id, teams_away_team_id)) %>%   
  left_join(savant_select, by=c("fullName")) %>% 
  left_join(team_k %>% select(2:4), by=c("opp_id"="team_id_num")) %>% 
  select(-c(team_id, opp_id)) %>%
  relocate(opp, .after = est_woba) %>% 
  arrange(est_woba)


# 3.0 generate projections ------------------------------------------------

game_log_test <- fg_pitcher_game_logs(17677, 2024)


# Function to generate projection for each playerid
generate_projection <- function(playerid, team_k) {
  
  # Load game log with pitcher ID and year
  game_log <- fg_pitcher_game_logs(playerid, 2024) %>% 
    select(Date, Opp, HomeAway, IP, H, ER, SO) %>% 
    mutate(Opp = gsub("@", "", Opp)) %>% 
    arrange(Date) %>% 
    left_join(team_k_fg, by = c("Opp" = "team_name"))
  
  # Create linear model
  model <- lm(SO ~ K_pct + WAR, data = game_log)
  
  # Project with new data (example values for K_pct and WAR)
  new_data <- data.frame(K_pct = slate_probables$K_pct[which(slate_probables$playerid == playerid)], 
                         WAR = slate_probables$WAR[which(slate_probables$playerid == playerid)])
  projection <- predict(model, newdata = new_data)
  
  return(round(projection, 1))
}

# Apply the function to each playerid using purrr::map_dbl to return a numeric vector
slate_probables$projection <- map_dbl(slate_probables$playerid, 
                                      generate_projection, 
                                      team_k = team_k)
  
# add savant and fangraphs to projections
slate <- slate_probables %>% 
  relocate(projection, .after = playerid) %>% 
  arrange(-projection) %>% 
  view("slate")

# 3.0 game log check ------------------------------------------------------

game_log_display <- function(pitcher_id) {
  
  pitcher <- pitcher_ids$PlayerName[which(pitcher_ids$playerid == pitcher_id)]
  
  pitcher_game_log <- fg_pitcher_game_logs(pitcher_id, 2024) %>% 
    select(Date, Opp, HomeAway, IP, H, ER, SO) %>% 
    mutate(Opp = gsub("@", "", Opp)) %>% 
    left_join(team_k_fg, by = c("Opp" = "team_name")) %>% 
    arrange(Date) %>% 
    view(glue("{pitcher} game log"))
  
}
game_log_display(13164)

# 4.0 test code area ------------------------------------------------------

{# load game log with pitcher ID and year
  game_log <- fg_pitcher_game_logs(10603, 2024)
  game_log_filter <- game_log %>% 
    select(Date, Opp, HomeAway, IP, H, ER, SO) %>% 
    mutate(Opp = gsub("@", "", Opp)) %>% 
    arrange(Date)
  
  # load opponent batter k rates
  team <- fg_team_batter(startseason = 2024, endseason = 2024)
  team_k <- team %>% select(team_name, K_pct, WAR)
  
  ### test code block
  opening_day <- "2024-03-28"
  team_test <- fg_team_batter(startseason = 2024,
                              endseason = 2024, 
                              month = month(game_log_filter$Date[2]))
  
  
  # join the data
  game_log_filter <- game_log_filter %>% left_join(team_k, by=c("Opp"="team_name"))
  
  ###test code block### 
  # Use purrr::map2_dfr to iterate through each date and row number
  final_game_log <- game_log %>%
    mutate(
      K_pct = map2_dbl(Date, row_number(), ~ {
        team_data <- fg_team_batter(
          startseason = 2024,
          endseason = 2024,
          month = month(.x)
        )
        team_k <- team_data %>%
          select(team_name, K_pct) %>%
          filter(team_name == game_log$Opp[.y])
        if (nrow(team_k) > 0) team_k$K_pct else NA_real_
      }),
      WAR = map2_dbl(Date, row_number(), ~ {
        team_data <- fg_team_batter(
          startseason = 2024,
          endseason = 2024,
          month = month(.x)
        )
        team_k <- team_data %>%
          select(team_name, WAR) %>%
          filter(team_name == game_log$Opp[.y])
        if (nrow(team_k) > 0) team_k$WAR else NA_real_
      })
    )
  
  final_game_log$WAR
  final_game_log$`K%`
  
  # create linear model
  model <- lm(SO ~ K_pct + WAR, 
              data = game_log_filter)
  
  # project with new data
  new_data <- data.frame(K_pct = .225, 
                         WAR = 5.5)
  
  projection <- predict(model, newdata = new_data)
  projection}
