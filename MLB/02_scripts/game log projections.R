# gamelog projections 


# 1.0 load packages and data ----------------------------------------------


# load packages
library(baseballr)
library(mlbplotR)
library(tidyverse)

# load teams logos
mlb_teams <- load_mlb_teams()

# load fangraphs leaderboard data
pitchers <- fg_pitcher_leaders(startseason = 2024, endseason = 2024)
pitcher_ids <- pitchers %>% select(PlayerName, playerid)

# load savant data
savant <- statcast_leaderboards(year = 2024, 
                                leaderboard = "expected_statistics",
                                player_type = "pitcher") %>% 
  separate(`last_name, first_name`, 
                    into = c("last_name","first_name"), 
                    sep = ", ") %>% 
  unite("fullName", first_name, last_name, sep = " ")

savant_select <- savant %>% select(fullName, est_woba)

# define slate date probable pitchers
slate_date <- Sys.Date()+1
schedule <- mlb_schedule(season = 2024) %>% 
  filter(date == slate_date)

games <- schedule %>% select(game_pk, teams_away_team_name, teams_away_team_id, teams_home_team_name, teams_home_team_id)

probables_all <- map_df(games$game_pk, mlb_probables) %>% 
  left_join(pitcher_ids, by=c("fullName"="PlayerName")) %>% 
  select(game_pk, game_date, fullName, playerid, team, team_id) %>% 
  drop_na()

# Load opponent batter K rates outside the function
team <- fg_team_batter(startseason = 2024, endseason = 2024)
team_k <- team %>% select(team_name, K_pct, WAR)


# 2.0 generate projections ------------------------------------------------


# Function to generate projection for each playerid
generate_projection <- function(player_id, team_k) {
  # Load game log with pitcher ID and year
  game_log <- fg_pitcher_game_logs(player_id, 2024)
  
  # Filter and prepare the game log
  game_log_filter <- game_log %>% 
    select(Date, Opp, HomeAway, IP, H, ER, SO) %>% 
    mutate(Opp = gsub("@", "", Opp)) %>% 
    arrange(Date)
  
  # Join the data
  game_log_filter <- game_log_filter %>% 
    left_join(team_k, by = c("Opp" = "team_name"))
  
  # Create linear model
  model <- lm(SO ~ K_pct + WAR, data = game_log_filter)
  
  # Project with new data (example values for K_pct and WAR)
  new_data <- data.frame(K_pct = 0.225, WAR = 5.5)
  projection <- predict(model, newdata = new_data)
  
  return(round(projection, 1))
}

# Apply the function to each playerid using purrr::map_dbl to return a numeric vector
probables_all$projection <- map_dbl(probables_all$playerid, generate_projection, team_k = team_k)
  
# add savant data
probables_all <- probables_all %>% 
  left_join(savant_select, by=c("fullName")) %>% 
  arrange(-projection) %>% 
  view("probables_all")


# 3.0 test code area ------------------------------------------------------

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
