# hockey dfs file

# 1.0 load packages and data ----------------------------------------------

# load packages
suppressMessages({
  library(tidyverse)
  library(lubridate)
  library(fastRhockey)
})

options(digits = 3)

# Specify date
date <- "2023-11-16"

# Define the recoding rules as a named vector
team_abbrev_recode <- c("ANH" = "ANA", "NJ" = "NJD", "SJ" = "SJS", "MON" = "MTL")

# Import CSVs
nhl_salaries <- read.csv(paste0("./01_data/contests/", date, "/DKSalaries.csv")) %>%
  separate(Game.Info, 
           c("Away", "String"), 
           sep = "@") %>%
  separate(String, 
           c("Home", "Date", "Time"), 
           sep = " ", 
           extra = "drop") %>% 
  mutate(across(c(TeamAbbrev, Home, Away), ~recode(., !!!team_abbrev_recode))) %>%
  mutate(Opponent = if_else(Home == TeamAbbrev, Away, Home))

# get games

games <- nhl_salaries %>% select(Away, Home) %>% distinct()

# load money puck data
mp_skaters <- read.csv(paste0("./01_data/contests/", date,"/skaters.csv"))
mp_lines <- read.csv(paste0("./01_data/contests/", date,"/lines.csv"))
mp_goalies <- read.csv(paste0("./01_data/contests/", date,"/goalies.csv"))
mp_teams <- read.csv(paste0("./01_data/contests/", date,"/teams.csv"))

nhl_team_table <- read.csv("./01_data/nhlteams.csv")


# process data ------------------------------------------------------------


#Skaters
skaters <- nhl_salaries %>% 
  left_join(mp_skaters, by = c('Name' = "name")) %>% 
  filter(situation == "all") 

# power play
pp_gf <- mp_teams %>% 
  filter(situation == "5on4") %>% 
  select(name, 
         iceTime,
         games_played,
         penalityMinutesFor,
         xGoalsFor,
         flurryScoreVenueAdjustedxGoalsFor) %>% 
  mutate(iceTime = iceTime/60, # convert seconds to minutes
         pp_per60 = (iceTime / (games_played*60))*60, 
         xGoalsFor_per60 = xGoalsFor / (iceTime/60), 
         flurryScoreVenueAdjustedxGoalsFor_per60 = flurryScoreVenueAdjustedxGoalsFor / (iceTime/60)) %>% 
  arrange(-xGoalsFor_per60)

pp_ga <- mp_teams %>% 
  filter(situation == "4on5") %>% 
  select(name, 
         iceTime, 
         games_played, 
         penalityMinutesAgainst,
         xGoalsAgainst, 
         flurryScoreVenueAdjustedxGoalsAgainst) %>% 
  mutate(iceTime = iceTime/60, # convert seconds to minutes
         pp_per60 = (iceTime / (games_played*60))*60, 
         xGoalsAgainst_per60 = xGoalsAgainst / (iceTime/60), 
         flurryScoreVenueAdjustedxGoalsAgainst_per60 = flurryScoreVenueAdjustedxGoalsAgainst / (iceTime/60)) %>% 
  arrange(-xGoalsAgainst_per60)

pp_slate_home <- games %>% select(Home) %>% rename(home = Home) %>% 
  left_join(
    pp_gf %>% 
      select(name, xGoalsFor_per60, penalityMinutesFor) %>% 
      rename(home_xGoalsFor_per60 = xGoalsFor_per60, home_penalityMinutesFor = penalityMinutesFor),
    by = c("home" = "name")
  ) %>%
  left_join(
    pp_ga %>% 
      select(name, xGoalsAgainst_per60, penalityMinutesAgainst) %>% 
      rename(home_xGoalsAgainst_per60 = xGoalsAgainst_per60, home_penalityMinutesAgainst = penalityMinutesAgainst),
    by = c("home" = "name")
  )

pp_slate_away <- games %>% select(Away) %>% rename(away = Away) %>% 
  left_join(pp_gf %>% 
              select(name, xGoalsFor_per60, penalityMinutesFor) %>% 
              rename(away_xGoalsFor_per60 = xGoalsFor_per60, away_penalityMinutesFor = penalityMinutesFor), 
            by=c("away" = "name")) %>% 
  left_join(pp_ga %>% 
              select(name, xGoalsAgainst_per60, penalityMinutesAgainst) %>% 
              rename(away_xGoalsAgainst_per60 = xGoalsAgainst_per60, away_penalityMinutesAgainst = penalityMinutesAgainst), 
            by=c("away" = "name"))

pp <- cbind(pp_slate_away, pp_slate_home)

# teams
teams <- mp_teams %>% 
  filter(situation == "all") %>% 
  select(name, 
         games_played,
         flurryScoreVenueAdjustedxGoalsFor,
         flurryScoreVenueAdjustedxGoalsAgainst) %>% 
  mutate(xg_per_game = round(flurryScoreVenueAdjustedxGoalsFor / games_played, digits = 2),
         xg_rank = round(rank(-xg_per_game), digits = 0),
         
         xga_per_game = round(flurryScoreVenueAdjustedxGoalsAgainst / games_played, digits = 2),
         xga_rank = round(rank(xga_per_game), digits = 0),
         
         xgd = round(flurryScoreVenueAdjustedxGoalsFor - flurryScoreVenueAdjustedxGoalsAgainst, digits = 2),
         xgd_rank = round(rank(-xgd), digits = 0),
         
         xg_ratio = round(xg_per_game / xga_per_game, digits = 2),
         
         name_xgd = paste(name, xgd),
         
         name_xg_ratio = paste(name, xg_ratio))

# lines
mp_team_lines <- function(){
  
}

team_lines <- list()

for (i in 1:32) {
  team_lines[[i]] <- mp_lines %>% 
    filter(team == nhl_team_table$TeamAbbrev[i] & position == "line") %>% 
    filter(xGoalsFor == max(xGoalsFor)) %>% 
    arrange(-xGoalsFor) %>% 
    select(name, team, games_played, xGoalsFor)
}

team_lines <- do.call("rbind", team_lines)
names(team_lines)[c(1,3)] <- c("line","line_games_played")

teams <- teams %>% 
  left_join(team_lines, by = c("name" = "team"))

teams %>% 
  arrange(-xgd) %>% 
  view(title = "Teams")

nhl_team_logos

teams <- teams %>% 
  left_join(nhl_team_table, by = c("name" = "TeamAbbrev"))

# plot xg vs xga
xg_plot <- function(){
  teams %>%
    ggplot(aes(x = xg_per_game , y = xga_per_game)) +
    geom_hline(yintercept = mean(teams$xga_per_game, na.rm = TRUE), color = "red", linetype = "dashed", alpha=0.5) +
    geom_vline(xintercept =  mean(teams$xg_per_game, na.rm = TRUE), color = "red", linetype = "dashed", alpha=0.5) +
    geom_point(aes(color = xg_ratio), alpha = 0.7, cex = 6) +
    scale_color_gradient(low = "red", high = "green", guide = "colourbar") +
    geom_text_repel(aes(label=name_xg_ratio)) +
    labs(x = "xg_per_game",
         y = "xga_per_game",
         title = paste("NHL Teams", today()),
         caption = "based on flurryScoreVenueAdjustedxGoals
       Twitter: Its_MikeF | Data: MoneyPuck") +
    theme_bw() +
    theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold")) +
    #scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
    scale_y_reverse(n.breaks = 10)
}

# centers
slate_centers <- function(){
  centers <- skaters %>% 
    filter(position == "C" &
             Salary > 2500) %>% 
    left_join(teams, by = c('Opponent' = 'name')) %>% 
    mutate(xg_min = round(I_F_xGoals_with_earned_rebounds / (icetime/60), digits = 4), 
           xg_min_rank = round(rank(-xg_min), digits = 0))
  
  centers <- centers %>% 
    select(Name,
           Salary,
           TeamAbbrev,
           xg_min,
           xg_min_rank,
           Opponent,
           xga_per_game,
           xga_rank,
           xgd,
           I_F_points,
           I_F_xGoals,
           I_F_xGoals_with_earned_rebounds,
           I_F_points) %>% 
    arrange(-I_F_xGoals) %>%
    view(title = "Centers")
}

# wingers
slate_wingers <- function(){
  wingers <- skaters %>% 
    filter(position == "L" | position == "R" &
             Salary > 2500) %>%
    select(Name,
           Salary,
           TeamAbbrev,
           Opponent,
           I_F_points,
           I_F_xGoals,
           I_F_xGoals_with_earned_rebounds,
           AvgPointsPerGame, 
           games_played,
           icetime,
           iceTimeRank,
           I_F_points) %>% 
    arrange(-I_F_xGoals) %>% 
    view(title = "Wingers")
}

# defensemen
slate_defenders <- function(){
  defenders <- skaters %>%
    filter(position == "D" &
             Salary > 2500) %>%
    select(Name,
           Salary,
           TeamAbbrev,
           Opponent,
           I_F_points,
           I_F_xGoals,
           I_F_xGoals_with_earned_rebounds,
           AvgPointsPerGame, 
           games_played,
           icetime,
           iceTimeRank,
           I_F_points) %>% 
    arrange(-I_F_xGoals) %>% 
    view(title = "Defenders")
}

# goalies
slate_goalies <- function(){
  goalies <- nhl_salaries %>% 
    left_join(mp_goalies, by = c('Name' = "name")) %>% 
    filter(situation == "all") %>% 
    mutate(gsae <- flurryAdjustedxGoals - goals,
           gsae_per_game <- round((flurryAdjustedxGoals - goals) / games_played, digits = 2),
           icetime <- round(icetime/60, digits = 0))
  
  goalies %>% 
    select(Name, 
           Salary,
           team,
           games_played,
           gsae,
           gsae_per_game) %>% 
    arrange(-gsae_per_game) %>% 
    view(title = "Goalies")
}

# arhive -----------------------------------------------------------------

#Rotowire NHL
rotowire <- function(){
  
  rotowire_nhl <- read.csv(paste0("./01_data/contests/", date,"/nhl-odds-rotowire.csv"))
  
  names(rotowire_nhl)[c(1,2,11)] <- c('Squad', 'DateTime', 'Opp.Score')
  
  rotowire_nhl <- rotowire_nhl[-c(1),-c(4,6,8)]
  
  rotowire_nhl[4:8] <- as.numeric(unlist(rotowire_nhl[4:8]))
  
  rotowire_nhl <- rotowire_nhl %>% 
    left_join(nhl_team_table, by = c("Squad" = "Mascot"))
  
  rotowire_nhl <- rotowire_nhl %>% 
    left_join(teams, by = c("TeamAbbrev" = "name"))
  
  rotowire_nhl_away <- rotowire_nhl[seq(1, nrow(rotowire_nhl), 2), ]
  rotowire_nhl_home <- rotowire_nhl[seq(2, nrow(rotowire_nhl), 2), ]
  
  rotowire_nhl_away$DateTimeScorePoints <- paste(rotowire_nhl_away$DateTime, rotowire_nhl_away$Opp.Score, rotowire_nhl_away$Total.Points)
  rotowire_nhl_home$DateTimeScorePoints <- paste(rotowire_nhl_home$DateTime, rotowire_nhl_home$Implied.Score, rotowire_nhl_home$Total.Points)
  
  rotowire_nhl_slate <- rotowire_nhl_away %>% 
    left_join(rotowire_nhl_home, by = c("DateTimeScorePoints" = "DateTimeScorePoints")) 
  
  rotowire_nhl_slate[, c(5:9)] <- sapply(rotowire_nhl_slate[, c(5:9)], as.numeric)
  
  rotowire_nhl_slate$xgd_home <- rotowire_nhl_slate$xgd.y - rotowire_nhl_slate$xgd.x
  
  rotowire_nhl_slate %>% 
    select(Squad.x,
           Win.x,
           Implied.Score.x,
           xgd.x,
           xgd_rank.x,
           Squad.y,
           Win.y,
           Implied.Score.y,
           xgd.y,
           xgd_rank.y,
           Total.Points.y, 
           xgd_home) %>% 
    arrange(-xgd_home) %>% 
    view(title = "NHL Slate")
}

#Combinations
combinations <- function(){
  possibilities_centers <- round(factorial(dim(centers)[1]) / (factorial(2)*factorial(dim(centers)[1] - 2)), digits = 0)
  possibilities_wingers <- round(factorial(dim(wingers)[1]) / (factorial(2)*factorial(dim(wingers)[1] - 2)), digits = 0)
  possibilities_defenders <- round(factorial(dim(defenders)[1]) / (factorial(2)*factorial(dim(defenders)[1] - 2)), digits = 0)
  
  possibilities_total <- format(possibilities_centers * possibilities_wingers * possibilities_defenders * dim(goalies)[1], scientific = T)
  
  combinations_centers <- combn(centers$Name, 2)
  combinations_wingers <- combn(wingers$Name, 3)
  combinations_defenders <- combn(defenders$Name, 2)
  
  #expand_grid()
}
