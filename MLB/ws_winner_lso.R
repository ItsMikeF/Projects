#get mlb world series odds via the Live Sports Odds API

library(httr)
library(tidyverse)
library(gt)
library(stats)

ws_winner <- function(variables) {
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
    mutate(implied_odds = convert_ML(american_odds))
    
}

ws_winner()
