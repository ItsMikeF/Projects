# get cfb odds via LSO API

# Load required packages
library(tidyverse) # For data manipulation and piping
library(httr)      # For API requests
library(purrr)     # For functional programming
library(lubridate) # For date-time handling

# Updated function to retrieve NCAAF odds data for a specific date
get_ncaaf_odds <- function(api_key, target_date = today(), 
                           regions = "us", odds_format = "american", 
                           markets = "spreads", date_format = "iso") {
  # Compute the date string in YYYY-MM-DD format
  date_str <- as.character(target_date)
  
  # Define API endpoint and query parameters
  base_url <- "https://api.the-odds-api.com"
  url <- "https://api.the-odds-api.com/v4/sports/americanfootball_ncaaf/odds"
  query <- list(
    regions = regions,
    oddsFormat = odds_format,
    markets = markets,
    dateFormat = date_format,
    date = date_str  # Filter for the target date
  )
  
  # Make API request
  response <- tryCatch(
    {
      GET(url, 
          add_headers('X-RapidAPI-Key' = api_key, 
                      'X-RapidAPI-Host' = 'odds.p.rapidapi.com'), 
          query = query, 
          content_type("application/octet-stream"))
    },
    error = function(e) {
      stop("Failed to retrieve data from API: ", e$message)
    }
  )
  
  # Check response status
  if (http_status(response)$category != "Success") {
    stop("API request failed with status: ", http_status(response)$reason)
  }
  
  # Parse response content
  content <- content(response, "parsed", simplifyVector = FALSE)
  
  # Check if content is empty
  if (length(content) == 0) {
    warning("No data returned from the API for date: ", date_str)
    return(tibble())  # Return empty tibble instead of stopping
  }
  
  # Optional: Filter for future/ongoing games if needed (e.g., exclude completed ones)
  # current_time <- now("UTC")
  # content <- keep(content, ~ ymd_hms(.x$commence_time) >= current_time)
  # (Uncomment and adjust if you want to exclude past games within the date)
  
  # Extract odds data into a data frame
  odds_df <- map_dfr(content, function(game) {
    # Safely extract bookmaker data (use first bookmaker if available)
    bookmaker <- game$bookmakers[[1]] %||% list()
    market <- bookmaker$markets[[1]] %||% list()
    outcomes <- market$outcomes %||% list()
    
    # Check if outcomes exist and have at least 2 entries (home and away teams)
    if (length(outcomes) < 2) {
      return(tibble(
        game_id = game$id %||% NA_character_,
        target_date = date_str,
        home_team = NA_character_,
        home_spread = NA_real_,
        away_team = NA_character_,
        away_spread = NA_real_
      ))
    }
    
    # Extract team names and spreads
    tibble(
      game_id = game$id %||% NA_character_,
      target_date = date_str,
      home_team = outcomes[[1]]$name %||% NA_character_,
      home_spread = outcomes[[1]]$point %||% NA_real_,
      away_team = outcomes[[2]]$name %||% NA_character_,
      away_spread = outcomes[[2]]$point %||% NA_real_
    )
  })
  
  return(odds_df)
}

# Retrieve API key from environment variable
api_key <- Sys.getenv("LSO_KEY")
if (api_key == "") {
  stop("API key not found. Please set 'RAPIDAPI_KEY' in your .Renviron file.")
}

# Get tomorrow's date dynamically
tomorrow <- today() + days(1)

# Call the function for tomorrow's games
tomorrow_odds <- get_ncaaf_odds(api_key = api_key, target_date = tomorrow)

# Display the structure and preview
str(tomorrow_odds)
head(tomorrow_odds)
