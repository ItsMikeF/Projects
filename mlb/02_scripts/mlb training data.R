# create training data set for mlb model


# 1.0 load packages and data ----------------------------------------------


# load packages
suppressMessages({
  library(furrr) # built on future and purrr, integrates well with map_df
  library(purrr)
  library(baseballr)
  library(mlbplotR)
  library(tidyverse)
  library(glue)
  library(progress)
  library(progressr)
})

# define season
season = year(Sys.Date())

years <- c(2023:2025)


# 2.0 batters -------------------------------------------------------------


# 2.1 define all batter ids -----------------------------------------------


# Use lapply to get batter data for each year
batter_data_list <- lapply(years, function(year) {
  fg_batter_leaders(startseason = year, 
                    endseason = year, 
                    qual = "0")
})

# Combine the list of data frames into a single data frame
batter_data <- do.call(rbind, c(batter_data_list, fill = TRUE))

batter_ids <- batter_data %>% 
  select(PlayerName, playerid, Bats) %>% # include bats for game log data
  arrange(playerid) %>% 
  distinct(playerid, .keep_all = T)

batter_ids_vec <- as.vector(batter_ids$playerid)


# 2.2 game logs -----------------------------------------------------------


# batter game logs, single batter example, Corbin Carroll
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

# define cores
cores <- availableCores() - 2 # Use all cores except two

# Get game logs for all batters with parallel processing and progress bar
all_batter_game_logs <- function(batter_ids_vec, years, cores) {
  # Set up parallel processing
  plan(multisession, workers = cores) # Use multiple CPU cores
  
  # Set up progress bar with progressr
  handlers(global = TRUE) # Enable progress bar globally
  handlers("progress") # Use progress bar style
  
  # Initialize progress bar
  p <- progressor(along = batter_ids$playerid)
  
  # Parallel map with progress bar
  result <- future_map_dfr(batter_ids$playerid, function(batter_id) {
    p() # Update progress bar
    safe_batter_logs(batter_id, years)
  }, .options = furrr_options(seed = TRUE))
  
  # Clean up parallel plan
  plan(sequential)
  
  return(result)
}

# Call the function, save output, and time execution
system.time({
  batter_logs <- all_batter_game_logs(batter_ids_vec, years, cores)
})

# save Rdata file
save(batter_logs, file = "./01_data/training_data/batter_logs.Rdata")

# load batter logs
load("./01_data/training_data/batter_logs.Rdata")

batter_logs <- batter_logs %>% 
  mutate(Date = ymd(Date)) %>% 
  filter(Date < "2024-04-12")

test2 <- batter_logs %>% 
  filter(PlayerName == "Pete Alonso")

# 2.1 aggregate batter data -----------------------------------------------


test1 <- batter_logs %>% 
  filter(season == 2025) %>% 
  filter(Team == "MIL") %>% 
  group_by(PlayerName) %>% 
  summarise(total_ab = sum(AB), 
            total_hr = sum(HR))

test <- batter_logs %>% 
  group_by(Team, season) %>% 
  summarise(total_hr = sum(HR))

batter_team_rolling <- batter_logs %>%
  group_by(Team, season) %>%
  arrange(Date) %>%
  mutate(
    AB_cum = lag(cumsum(AB), default = 0),
    H_cum = lag(cumsum(H), default = 0),
    HR_cum = lag(cumsum(HR), default = 0),
    R_cum = lag(cumsum(R), default = 0),
    RBI_cum = lag(cumsum(RBI), default = 0),
    BB_cum = lag(cumsum(BB), default = 0),
    SO_cum = lag(cumsum(SO), default = 0),
    wOBA_avg = lag(cummean(wOBA), default = NA),
    wRCplus_avg = lag(cummean('wRC+'), default = NA)
  ) %>%
  ungroup()

team_stats_up_to_date <- batter_team_rolling %>%
  group_by(Team, Date) %>%
  summarise(
    AB = sum(AB_cum, na.rm = TRUE),
    H = sum(H_cum, na.rm = TRUE),
    HR = sum(HR_cum, na.rm = TRUE),
    R = sum(R_cum, na.rm = TRUE),
    RBI = sum(RBI_cum, na.rm = TRUE),
    BB = sum(BB_cum, na.rm = TRUE),
    SO = sum(SO_cum, na.rm = TRUE),
    wOBA = mean(wOBA_avg, na.rm = TRUE),
    wRCplus = mean(wRCplus_avg, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  ungroup() %>% 
  mutate(join = paste0(Date, Team)) %>% 
  mutate(bat_avg = round(H/AB, digits = 3),
         R_rate = round(R/AB, digits = 3),
         bb_rate = round(BB/AB, digits = 3),
         k_rate = round(SO/AB, digits = 3))

library(dplyr)
library(zoo)

library(dplyr)
library(zoo)

# Step 1: Aggregate raw batter stats to team-level stats per game/date
team_game_stats <- batter_logs %>%
  group_by(Team, season, Date) %>%
  summarise(
    AB = sum(AB, na.rm = TRUE),
    H = sum(H, na.rm = TRUE),
    HR = sum(HR, na.rm = TRUE),
    R = sum(R, na.rm = TRUE),
    RBI = sum(RBI, na.rm = TRUE),
    BB = sum(BB, na.rm = TRUE),
    SO = sum(SO, na.rm = TRUE),
    #wOBA = weighted.mean(wOBA, w = AB, na.rm = TRUE),  # Weighted by AB
    #wRCplus = weighted.mean(wRCplus, w = AB, na.rm = TRUE),  # Weighted by AB
    .groups = "drop"
  )

# Step 2: Calculate cumulative or rolling team stats
team_stats_rolling <- team_game_stats %>%
  group_by(Team, season) %>%
  arrange(Date) %>%
  mutate(
    AB_cum = lag(cumsum(AB), default = 0),
    H_cum = lag(cumsum(H), default = 0),
    HR_cum = lag(cumsum(HR), default = 0),
    R_cum = lag(cumsum(R), default = 0),
    RBI_cum = lag(cumsum(RBI), default = 0),
    BB_cum = lag(cumsum(BB), default = 0),
    SO_cum = lag(cumsum(SO), default = 0),
    #wOBA_avg = lag(rollmean(wOBA, k = 14, fill = NA, align = "right")),  # 14-day rolling avg
    #wRCplus_avg = lag(rollmean(wRCplus, k = 14, fill = NA, align = "right"))  # 14-day rolling avg
  ) %>%
  ungroup()

# Step 3: Calculate team-level derived metrics
team_stats_up_to_date <- team_stats_rolling %>%
  mutate(
    join = paste0(Date, Team),
    bat_avg = round(H_cum / AB_cum, digits = 3),
    R_rate = round(R_cum / AB_cum, digits = 3),
    bb_rate = round(BB_cum / AB_cum, digits = 3),
    k_rate = round(SO_cum / AB_cum, digits = 3)
  ) %>%
  filter(AB_cum > 0)  # Avoid division by zero

# Step 1: Aggregate raw batter stats to team-level stats per game/date
team_game_stats <- batter_logs %>%
  group_by(Team, season, Date) %>%
  summarise(
    AB = sum(AB, na.rm = TRUE),
    H = sum(H, na.rm = TRUE),
    HR = sum(HR, na.rm = TRUE),
    R = sum(R, na.rm = TRUE),
    RBI = sum(RBI, na.rm = TRUE),
    BB = sum(BB, na.rm = TRUE),
    SO = sum(SO, na.rm = TRUE),
    wOBA = weighted.mean(wOBA, w = AB, na.rm = TRUE),  # Weighted by AB
    wRCplus = weighted.mean(wRCplus, w = AB, na.rm = TRUE),  # Weighted by AB
    .groups = "drop"
  )

# Step 2: Calculate cumulative or rolling team stats
team_stats_rolling <- team_game_stats %>%
  group_by(Team, season) %>%
  arrange(Date) %>%
  mutate(
    AB_cum = lag(cumsum(AB), default = 0),
    H_cum = lag(cumsum(H), default = 0),
    HR_cum = lag(cumsum(HR), default = 0),
    R_cum = lag(cumsum(R), default = 0),
    RBI_cum = lag(cumsum(RBI), default = 0),
    BB_cum = lag(cumsum(BB), default = 0),
    SO_cum = lag(cumsum(SO), default = 0),
    wOBA_avg = lag(rollmean(wOBA, k = 14, fill = NA, align = "right")),  # 14-day rolling avg
    wRCplus_avg = lag(rollmean(wRCplus, k = 14, fill = NA, align = "right"))  # 14-day rolling avg
  ) %>%
  ungroup()

# Step 3: Calculate team-level derived metrics
team_stats_up_to_date <- team_stats_rolling %>%
  mutate(
    join = paste0(Date, Team),
    bat_avg = round(H_cum / AB_cum, digits = 3),
    R_rate = round(R_cum / AB_cum, digits = 3),
    bb_rate = round(BB_cum / AB_cum, digits = 3),
    k_rate = round(SO_cum / AB_cum, digits = 3)
  ) %>%
  filter(AB_cum > 0)  # Avoid division by zero


# Step 2: Aggregate each game at the team-date level (i.e., 1 row per Team-Date)
team_game_logs <- batter_logs %>%
  group_by(Team, Date, year) %>%
  summarise(
    AB = sum(AB, na.rm = TRUE),
    H = sum(H, na.rm = TRUE),
    HR = sum(HR, na.rm = TRUE),
    R = sum(R, na.rm = TRUE),
    RBI = sum(RBI, na.rm = TRUE),
    BB = sum(BB, na.rm = TRUE),
    SO = sum(SO, na.rm = TRUE),
    wOBA = mean(wOBA, na.rm = TRUE),
    wRCplus = mean(`wRC+`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Team, Date)

# Step 3: Create cumulative stats up to (but not including) each date
team_stats_up_to_date <- team_game_logs %>%
  group_by(Team, year) %>%
  arrange(Date) %>%
  mutate(
    cum_AB = lag(cumsum(AB), default = 0),
    cum_H = lag(cumsum(H), default = 0),
    cum_HR = lag(cumsum(HR), default = 0),
    cum_R = lag(cumsum(R), default = 0),
    cum_RBI = lag(cumsum(RBI), default = 0),
    cum_BB = lag(cumsum(BB), default = 0),
    cum_SO = lag(cumsum(SO), default = 0),
    wOBA_avg = lag(cummean(wOBA), default = NA),
    wRCplus_avg = lag(cummean(wRCplus), default = NA)
  ) %>%
  ungroup() %>%
  mutate(
    bat_avg = round(cum_H / cum_AB, 3),
    R_rate = round(cum_R / cum_AB, 3),
    bb_rate = round(cum_BB / cum_AB, 3),
    k_rate = round(cum_SO / cum_AB, 3),
    join = paste0(Date, Team)
  )

# 3.0 pitchers ------------------------------------------------------------


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
        
        mutate(Opp = gsub("@", "", Opp), 
               fpts = W*5 + SO*3 + IP*3 + ER*-3, 
               Date = ymd(Date), 
               season = year(Date)) %>%
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
save(pitcher_logs, file = "./01_data/training_data/pitcher_logs.Rdata")

# load pitcher logs
load("./01_data/training_data/pitcher_logs.Rdata")

pitcher_logs <- pitcher_logs %>% 
  mutate(fpts = W*5 + SO*3 + IP*3 + ER*-3) %>% 
  mutate(Date = ymd(Date)) %>% 
  arrange(-fpts)


# 3.0 pitchers ------------------------------------------------------------


# Load required packages
library(furrr)
library(progressr)
library(dplyr)
library(purrr)
library(lubridate)
library(glue)
library(parallel)

# Function to safely fetch pitcher game logs (unchanged)
safe_pitcher_logs <- function(pitcher_id, years) {
  tryCatch({
    map_df(years, function(year) {
      fg_pitcher_game_logs(pitcher_id, year) %>%
        select(Date, Team, PlayerName, playerid, Age, W, L, QS, 
               Balls, Strikes, Pitches, Events, Barrels, HardHit,
               Opp, HomeAway, IP, H, ER, SO) %>%
        mutate(Opp = gsub("@", "", Opp), 
               fpts = W*5 + SO*3 + IP*3 + ER*-3, 
               Date = ymd(Date), 
               season = year(Date)) %>%
        arrange(Date)
    })
  }, error = function(e) {
    message(glue("Error getting data for pitcher ID {pitcher_id}: {e$message}"))
    return(NULL)
  })
}

# Get game logs for all pitchers with parallel processing and progress bar
all_pitcher_game_logs <- function(pitcher_ids, years, cores = detectCores() - 1) {
  # Set up parallel processing
  plan(multisession, workers = cores) # Use multiple CPU cores
  
  # Set up progress bar with progressr
  handlers(global = TRUE) # Enable progress bar globally
  handlers(handler_progress(format = "[:bar] :percent :eta")) # Custom progress bar style
  
  # Initialize progress bar
  p <- progressor(along = pitcher_ids$playerid)
  
  # Parallel map with progress bar
  result <- future_map_dfr(pitcher_ids$playerid, function(pitcher_id) {
    p() # Update progress bar
    safe_pitcher_logs(pitcher_id, years)
  }, .options = furrr_options(seed = TRUE))
  
  # Clean up parallel plan
  plan(sequential)
  
  return(result)
}

# Call the function, save output, and time execution
cores <- detectCores() - 2 # Use all cores except two
system.time({
  pitcher_logs <- all_pitcher_game_logs(pitcher_ids, years, cores)
})

# Inspect or save the output
head(pitcher_logs) # View first few rows

# Optional: Save to CSV or RDS
write.csv(pitcher_logs, "pitcher_game_logs.csv", row.names = FALSE)
saveRDS(pitcher_logs, "pitcher_logs.rds") # Smaller, faster format


# 3.1 join team level batter data to pitcher game log ---------------------


test <- pitcher_logs %>% 
  mutate(join = paste0(Date, Opp)) %>% 
  left_join(team_stats_up_to_date, by=c("join"))


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
