# create training data set for mlb model


# 1.0 load packages and data ----------------------------------------------


# load packages
suppressMessages({
  library(baseballr)
  library(mlbplotR)
  library(tidyverse)
  library(glue)
})

# define season
season = year(Sys.Date())

years <- c(2023:2025)


# 2.0 pitchers ------------------------------------------------------------


# load pitcher ids
pitchers <- fg_pitcher_leaders(startseason = season, endseason = season)
pitcher_ids <- pitchers %>% select(PlayerName, playerid, Throws)

# pitcher game logs
{
  pitcher_game_log <- fg_pitcher_game_logs(22182, 2024) %>% 
    select(Date, Team, PlayerName, playerid, Age, W, L,
           Balls, Strikes, Pitches, Events, Barrels, HardHit,
           Opp, HomeAway, IP, H, ER, SO) %>% 
    mutate(Opp = gsub("@", "", Opp)) %>% 
    arrange(Date)
}

# get game logs from years for 1 pitcher
game_logs <- map_df(years, function(year) {
  fg_pitcher_game_logs(22182, year) %>% 
    select(Date, Team, PlayerName, playerid, Age, W, L, QS, 
           Balls, Strikes, Pitches, Events, Barrels, HardHit,
           Opp, HomeAway, IP, H, ER, SO) %>% 
    mutate(Opp = gsub("@", "", Opp)) %>% 
    arrange(Date)
})

# grok code 
safe_pitcher_logs <- function(pitcher_id, years) {
  tryCatch({
    map_df(years, function(year) {
      fg_pitcher_game_logs(pitcher_id, year) %>%
        select(Date, Team, PlayerName, playerid, Age, W, L, QS, 
               Balls, Strikes, Pitches, Events, Barrels, HardHit,
               Opp, HomeAway, IP, H, ER, SO) %>%
        mutate(Opp = gsub("@", "", Opp)) %>%
        arrange(Date)
    })
  }, error = function(e) {
    message(glue("Error getting data for pitcher ID {pitcher_id}: {e$message}"))
    return(NULL)
  })
}

# Get game logs for all pitchers across specified years
all_pitcher_game_logs <- map_df(pitcher_ids$playerid, function(pitcher_id) {
  safe_pitcher_logs(pitcher_id, years)
})

# save all pitcher game logs 
save(all_pitcher_game_logs, file = "./01_data/training_data/all_pitcher_game_logs.Rdata")

# load pitcher logs
load("./01_data/training_data/all_pitcher_game_logs.Rdata")

all_pitcher_game_logs <- all_pitcher_game_logs %>% 
  mutate(fpts = W*5 + SO*3 + IP*3 + ER*-3) %>% 
  arrange(-fpts)


# 3.0 hitters -------------------------------------------------------------


# get hitter data
batters <- fg_batter_leaders(startseason = season, endseason = season)
batter_ids <- batters %>% select(PlayerName, playerid, Bats)

# hitter game logs, single batter, Corbin Carroll
{
batter_game_log <- fg_batter_game_logs(25878, 2024) %>% 
    
  select(PlayerName, playerid, Date, Team, Opp, season, Age, BatOrder, Pos, 
         G, AB, H, '1B', '2B', '3B', HR, R, RBI, 
         BB, SO, SB, HBP,
         Pitches, Balls, Strikes, 
         wOBA, 'wRC+', 
         Events, EV, maxEV, LA, Barrels, HardHit) %>% 
    
  mutate(home_away = as.integer(grepl("@", Opp)),
         home = if_else(home_away == 0, Team, Opp), 
         away = if_else(home_away == 1, Team, Opp), 
         home = gsub("@", "", home), 
         away = gsub("@", "", away), 
         game_id = paste(Date, away, home, sep = "_"),
         
         year = year(Date),
         month = month(Date), 
         
         wOBA = round(wOBA, digits = 2), 
         #"wRC+" = round('wRC+', digits = 2), 
         LA = round(LA, digits = 2), 
         EV = round(EV, digits = 2), 
         maxEV = round(maxEV, digits = 2), 
         
         fpts = H*3 + HR*10 + BB*3 + HBP*3 + RBI*2 + R*2 + SB*4
         ) %>% 
    
    left_join(batter_ids %>% select(playerid, Bats), 
              by = c("playerid")) %>% 
    relocate(Bats, .after = "PlayerName") %>% 
    relocate(fpts, .after = "Pos")
}

# grok code for all batter game logs
safe_batter_logs <- function(batter_id, years) {
  tryCatch({
    map_df(years, function(year) {
      fg_batter_game_logs(batter_id, year)%>% 
        
        select(PlayerName, playerid, Date, Team, Opp, season, Age, BatOrder, Pos, 
               G, AB, H, '1B', '2B', '3B', HR, R, RBI, 
               BB, SO, SB, HBP,
               Pitches, Balls, Strikes, 
               wOBA, 'wRC+', 
               Events, EV, maxEV, LA, Barrels, HardHit) %>% 
        
        mutate(home_away = as.integer(grepl("@", Opp)),
               home = if_else(home_away == 0, Team, Opp), 
               away = if_else(home_away == 1, Team, Opp), 
               home = gsub("@", "", home), 
               away = gsub("@", "", away), 
               game_id = paste(Date, away, home, sep = "_"),
               
               year = year(Date),
               month = month(Date), 
               
               wOBA = round(wOBA, digits = 2), 
               #"wRC+" = round('wRC+', digits = 2), 
               LA = round(LA, digits = 2), 
               EV = round(EV, digits = 2), 
               maxEV = round(maxEV, digits = 2), 
               
               fpts = H*3 + HR*10 + BB*3 + HBP*3 + RBI*2 + R*2 + SB*4
        ) %>% 
        
        left_join(batter_ids %>% select(playerid, Bats), 
                  by = c("playerid")) %>% 
        relocate(Bats, .after = "PlayerName") %>% 
        relocate(fpts, .after = "Pos") %>% 
        arrange(fpts)
    })
  }, error = function(e) {
    message(glue("Error getting data for batter ID {batter_id}: {e$message}"))
    return(NULL)
  })
}

# Get game logs for all batters across specified years
all_batter_game_logs <- map_df(batter_ids$playerid, function(batter_id) {
  safe_batter_logs(batter_id, years)
})

save(all_batter_game_logs, file = "./01_data/training_data/all_batter_game_logs.Rdata")

# load pitcher logs
load("./01_data/training_data/all_batter_game_logs.Rdata")

all_batter_game_logs <- all_batter_game_logs 



# 4.0 load savant ---------------------------------------------------------


baseballr::statcast_search_pitchers(start_date = "03-31-2025")

schedule <- baseballr::mlb_schedule(season = 2025)
game1 <- baseballr::mlb_pbp(778869)

pitch_data  <- game1 %>% 
  select(matchup.pitcher.fullName, matchup.pitcher.id, 
         matchup.batter.fullName, matchup.batter.id, 
         details.type.code, details.type.description, pitchNumber, 
         pitchData.startSpeed, # velocity in mph from the release
         pitchData.endSpeed, # velocity in mph at the plate
         pitchData.coordinates.pfxX, # horitzontal movmement (inches)
         pitchData.coordinates.pfxZ, # vertical movement (inches)
         pitchData.breaks.spinRate, #spin rate (RPM)
         pitchData.coordinates.pX, # horizontal plate location
         pitchData.coordinates.pZ, # vertical plate location
         details.call.description
         )
