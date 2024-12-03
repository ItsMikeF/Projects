# Get NFL odds via LSO api

# Load packages
suppressMessages({
  library(httr)
  library(tidyverse)
  library(glue)
  library(rlist)
  library(pipeR)
  library(nflverse)
})

# home and away are mixed up

# get spreads
spreads <- function(x) {
  # 1.0 Read LSO API --------------------------------------------------------
  
  
  # Write key and url
  key <- "americanfootball_nfl"
  url <- glue("https://odds.p.rapidapi.com/v4/sports/{key}/odds")
  
  # Define list of parameters for the API request
  queryString <- list(
    regions="us", 
    markets="spreads",
    oddsFormat="american", 
    dateFormat="iso"
  )
  
  # Perform get request with headers and content type
  response <- VERB("GET", 
                   url, 
                   add_headers('X-RapidAPI-Host' = 'odds.p.rapidapi.com', 
                               'X-RapidAPI-Key' = 'b34680a5eamsh5c39b8cf066dd0ap1e8d93jsnc9e5097b3c8c'),
                   query = queryString, content_type("application/octet-stream"))
  
  # Extract the content from the response and parse it into an R object
  content <- content(response, "parsed")
  
  
  # 2.0 Create odds table ---------------------------------------------------
  
  
  #Add Functions
  convert_ML <- function(odds) {
    breakeven <- if_else(odds > 0, 100 / (100 + odds), abs(odds) / (abs(odds) + 100))
    return(round(breakeven, digits = 4))
  }
  
  #gpt suggested code
  # convet nested list of api output to list of dataframes
  df_list <- lapply(content, function(x) {
    data.frame(
      book = sapply(x$bookmakers, function(y) y$title),
      away_team = sapply(x$bookmakers, function(y) y$markets[[1]]$outcomes[[1]]$name),
      away_spread = sapply(x$bookmakers, function(y) y$markets[[1]]$outcomes[[1]]$point),
      home_team = sapply(x$bookmakers, function(y) y$markets[[1]]$outcomes[[2]]$name),
      home_spread = sapply(x$bookmakers, function(y) y$markets[[1]]$outcomes[[2]]$point),
      last_update = sapply(x$bookmakers, function(y) y$last_update)
    )
  })
  
  # bind list to a dataframe
  df <- bind_rows(df_list[1:15]) # this 15 is the number of games remaining in the game week
  
  away <- df %>% 
    group_by(away_team) %>% 
    summarise(avg_spread = round(mean(away_spread), digits = 1),
              min = round(min(away_spread), digits = 1),
              max = round(max(away_spread), digits = 1)) %>% 
    rename(team = away_team) %>% 
    mutate(location = "away")
  
  home <- df %>% 
    group_by(home_team) %>% 
    summarise(avg_spread = round(mean(home_spread), digits = 1),
              min = round(min(home_spread), digits = 1), 
              max = round(max(home_spread), digits = 1)) %>% 
    rename(team = home_team) %>% 
    mutate(location = "home")
  
  # bind away and home teams
  slate_spreads <<- rbind(away, home) %>% 
    left_join(teams_colors_logos %>% select(team_abbr, team_name) %>% filter(team_abbr != "LAR"), 
              by = c("team" = "team_name")) %>% 
    relocate(team_abbr, .after = team)
}
spreads(x)

# get totals
totals <- function(x) {
  # 1.0 Read LSO API --------------------------------------------------------
  
  
  # Write key and url
  key <- "americanfootball_nfl"
  url <- glue("https://odds.p.rapidapi.com/v4/sports/{key}/odds")
  
  # Define list of parameters for the API request
  queryString <- list(
    regions="us", 
    markets="totals",
    oddsFormat="american", 
    dateFormat="iso"
  )
  
  # Perform get request with headers and content type
  response <- VERB("GET", 
                   url, 
                   add_headers('X-RapidAPI-Host' = 'odds.p.rapidapi.com', 
                               'X-RapidAPI-Key' = 'b34680a5eamsh5c39b8cf066dd0ap1e8d93jsnc9e5097b3c8c'),
                   query = queryString, content_type("application/octet-stream"))
  
  # Extract the content from the response and parse it into an R object
  content <- content(response, "parsed")
  
  
  # 2.0 Create odds table ---------------------------------------------------
  
  
  #Add Functions
  convert_ML <- function(odds) {
    breakeven <- if_else(odds > 0, 100 / (100 + odds), abs(odds) / (abs(odds) + 100))
    return(round(breakeven, digits = 4))
  }
  
  #gpt suggested code
  # convert nested list of api output to list of dataframes
  df_list <- lapply(content, function(x) {
    data.frame(
      book = sapply(x$bookmakers, function(y) y$title),
      away_team = x$away_team,
      away_total = sapply(x$bookmakers, function(y) y$markets[[1]]$outcomes[[1]]$point),
      home_team = x$home_team,
      home_total = sapply(x$bookmakers, function(y) y$markets[[1]]$outcomes[[2]]$point),
      last_update = sapply(x$bookmakers, function(y) y$last_update)
    )
  })
  
  # bind list to a dataframe
  df <- bind_rows(df_list[1:15]) # this 15 is the number of games remaining in the game week
  
  away <- df %>% 
    group_by(away_team) %>% 
    summarise(avg_total = round(mean(away_total), digits = 1),
              min = round(min(away_total), digits = 1),
              max = round(max(away_total), digits = 1)) %>% 
    rename(team = away_team) %>% 
    mutate(location = "away")
  
  home <- df %>% 
    group_by(home_team) %>% 
    summarise(avg_total = round(mean(home_total), digits = 1),
              min = round(min(home_total), digits = 1), 
              max = round(max(home_total), digits = 1)) %>% 
    rename(team = home_team) %>% 
    mutate(location = "home")
  
  # bind away and home teams
  slate_totals <<- rbind(away, home) %>% 
    left_join(teams_colors_logos %>% select(team_abbr, team_name) %>% filter(team_abbr != "LAR"), 
              by = c("team" = "team_name")) %>% 
    relocate(team_abbr, .after = team)
}
totals(x)

# calc team points
points <- slate_spreads %>% select(2,3,6) %>% 
  left_join(slate_totals %>% select(2,3), by=c("team_abbr")) %>% 
  relocate(location, .after = team_abbr) %>% 
  mutate(proj_points = round(avg_total/2 - (avg_spread/2), digits=1)) %>% 
  arrange(-proj_points)
