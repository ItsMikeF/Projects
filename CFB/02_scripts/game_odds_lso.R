# Load required packages
library(httr)      # For API requests
library(purrr)     # For functional programming
library(lubridate) # For date-time handling
library(tidyverse) # For tibble and data manipulation
library(cfbfastR)  # For teams data (assumed already loaded)

# load team names
teams <- load_cfb_teams() %>% 
  select(school, mascot) %>% 
  mutate(school_mascot = paste(school, mascot, sep = " "))

# Updated function to retrieve NCAAF odds with spreads and totals, replacing mascots with school names
get_ncaaf_odds <- function(api_key, teams, target_date = today(), 
                           regions = "us", odds_format = "american", 
                           markets = "spreads,totals", date_format = "iso") {
  # Compute the date string as ISO 8601 UTC timestamp (required for LSO date param)
  date_timestamp <- as.character(target_date)  # YYYY-MM-DD
  date_str <- paste0(date_timestamp, "T00:00:00Z")  # Append time for full ISO
  
  # Define API endpoint and query parameters (apiKey as query param for LSO)
  url <- "https://api.the-odds-api.com/v4/sports/americanfootball_ncaaf/odds"
  query <- list(
    apiKey = api_key,        # LSO auth: query param
    regions = regions,
    oddsFormat = odds_format,
    markets = markets,       # Request both spreads and totals
    dateFormat = date_format,
    date = date_str          # ISO timestamp for target date
  )
  
  # Make API request
  response <- tryCatch(
    {
      GET(url, query = query)
    },
    error = function(e) {
      stop("Failed to retrieve data from API: ", e$message)
    }
  )
  
  # Check response status
  if (http_status(response)$category != "Success") {
    stop("API request failed with status: ", http_status(response)$reason, 
         ". Check your apiKey and parameters. Full response: ", 
         content(response, "text"))
  }
  
  # Parse response content
  content <- content(response, "parsed", simplifyVector = FALSE)
  
  # Check if content is empty
  if (length(content) == 0) {
    warning("No data returned from the API for date: ", date_str)
    return(tibble())  # Return empty tibble instead of stopping
  }
  
  # Extract odds data into a data frame
  odds_df <- map_dfr(content, function(game) {
    # Safely extract bookmaker data (use first bookmaker if available)
    bookmaker <- game$bookmakers[[1]] %||% list()
    markets <- bookmaker$markets %||% list()
    
    # Find spreads market
    spread_market <- detect(markets, ~ .x$key == "spreads", .default = list())
    spread_outcomes <- spread_market$outcomes %||% list()
    
    # Find totals market
    total_market <- detect(markets, ~ .x$key == "totals", .default = list())
    total_outcomes <- total_market$outcomes %||% list()
    
    # Check if spread outcomes exist and have at least 2 entries
    if (length(spread_outcomes) < 2) {
      return(tibble(
        game_id = game$id %||% NA_character_,
        target_date = date_timestamp,
        home_team = NA_character_,
        home_spread = NA_real_,
        away_team = NA_character_,
        away_spread = NA_real_,
        game_total = NA_real_
      ))
    }
    
    # Extract game total from totals market (use first outcome, "over" or "under")
    game_total <- if (length(total_outcomes) >= 1) {
      total_outcomes[[1]]$point %||% NA_real_
    } else {
      NA_real_
    }
    
    # Extract home and away team names from game object
    home_team <- game$home_team %||% NA_character_
    away_team <- game$away_team %||% NA_character_
    
    # Match outcomes to home and away teams
    home_outcome <- detect(spread_outcomes, ~ .x$name == home_team, .default = spread_outcomes[[1]])
    away_outcome <- detect(spread_outcomes, ~ .x$name == away_team, .default = spread_outcomes[[2]])
    
    # If no match found (edge case), default to first/second outcome
    if (is.null(home_outcome$name) || is.null(away_outcome$name)) {
      home_outcome <- spread_outcomes[[1]]
      away_outcome <- spread_outcomes[[2]]
    }
    
    # Extract team names, spreads, and game total
    tibble(
      game_id = game$id %||% NA_character_,
      target_date = date_timestamp,
      home_team = home_outcome$name %||% NA_character_,
      home_spread = home_outcome$point %||% NA_real_,
      away_team = away_outcome$name %||% NA_character_,
      away_spread = away_outcome$point %||% NA_real_,
      game_total = game_total
    )
  })
  
  # Replace home_team and away_team with school names from teams data frame
  odds_df <- odds_df %>%
    left_join(select(teams, school_mascot, school), 
              by = c("home_team" = "school_mascot"), 
              suffix = c("", ".home")) %>%
    mutate(home_team = if_else(!is.na(school), school, home_team)) %>%
    select(-school) %>%
    left_join(select(teams, school_mascot, school), 
              by = c("away_team" = "school_mascot"), 
              suffix = c("", ".away")) %>%
    mutate(away_team = if_else(!is.na(school), school, away_team)) %>%
    select(-school)
  
  return(odds_df)
}

# Retrieve API key from environment variable
api_key <- Sys.getenv("LSO_KEY")

# Get tomorrow's date dynamically
tomorrow <- today() + days(1)  # 2025-09-13

# Call the function for tomorrow's games, passing the teams data frame
tomorrow_odds <- get_ncaaf_odds(api_key = api_key, teams = teams, target_date = tomorrow)

# Display the structure and preview
str(tomorrow_odds)
head(tomorrow_odds)

# save data to prevent unnecessary calls
saveRDS(tomorrow_odds, file = "./01_data/tomorrow_odds.RData")
#odds <- readRDS("./01_data/tomorrow_odds.RData")
