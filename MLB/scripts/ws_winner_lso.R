#get mlb world series odds via the Live Sports Odds API

library(httr)
library(tidyverse)
library(gt)
library(stats)
library(mlbplotR)

ws_winner <- function(variables) {
  teams_colors_logos <- mlbplotR::load_mlb_teams() %>% 
    filter(!team_abbr %in% c("AL", "NL", "MLB")) %>% 
    mutate(
      a = rep(1:6, 5), 
      b = sort(rep(1:5, 6), decreasing=T), 
      alpha = ifelse(grepl("A", team_abbr),1,0.75),
      color = ifelse(grepl("E", team_abbr), "b/w", NA)
    )
  
  sport_key <- "baseball_mlb_world_series_winner"
  url <- paste0("https://odds.p.rapidapi.com/v4/sports/",sport_key,"/odds")
  
  queryString <- list(
    regions = "us",
    oddsFormat = "american",
    dateFormat = "iso"
  )
  
  response <- VERB("GET", url, add_headers('X-RapidAPI-Host' = 'odds.p.rapidapi.com', 
                                           'X-RapidAPI-Key' = 'b34680a5eamsh5c39b8cf066dd0ap1e8d93jsnc9e5097b3c8c'), 
                   query = queryString, content_type("application/octet-stream"))
  
  content <- content(response, "parsed")

  ws_winner <- data.frame()
  
  for (i in 1:length(content[[1]][[8]][[2]][[4]][[1]][[3]])) {
    ws_winner[i,1] <- content[[1]][[8]][[2]][[4]][[1]][[3]][[i]]$name
    ws_winner[i,2] <- content[[1]][[8]][[2]][[4]][[1]][[3]][[i]]$price
  }
  
  #Add Functions
  convert_ML <- function(odds) {
    breakeven <- if_else(odds > 0, 100 / (100 + odds), abs(odds) / (abs(odds) + 100))
    return(round(breakeven, digits = 4))
  }
  
  ws_winner <<- ws_winner %>% 
    rename("team"=V1, 
           "american_odds"=V2) %>% 
    mutate(implied_odds = convert_ML(american_odds)) %>% 
    left_join(teams_colors_logos %>% select(team_name, team_abbr, team_logo_espn, team_color, team_color2), by=c('team'='team_name'))

}

ws_winner()

ws_winner %>% 
  ggplot(aes(x=team_abbr, y=implied_odds)) +
  geom_col(aes(color=team_abbr, fill=team_abbr)) +
  geom_mlb_logos(aes(team_abbr=team_abbr), width=.07, alpha=0.9) +
  scale_color_mlb(type="secondary") +
  scale_fill_mlb(alpha = 0.4)
