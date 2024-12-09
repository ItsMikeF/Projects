# Get NFL odds via LSO api

# Load packages
suppressMessages({
  library(httr)
  library(tidyverse)
  library(glue)
  library(rlist)
  library(pipeR)
  library(nflverse)
  library(jsonlite)
})

# Load necessary libraries
library(httr)
library(jsonlite)
library(dplyr)

# get sports

# define headers
api_key <- "114ea856077620641393f6fcf173cf11"
regions = "us"
oddsFormat = "american"
markets = "player_rush_yds"
sport = "americanfootball_nfl"

base_url <- "https://api.the-odds-api.com/v4/sports"

# Define the API endpoint for fetching NFL odds
url <- glue("{base_url}/?apiKey={api_key}")

response <- GET(url)

content <- content(response, "parsed")

# Convert the list into a dataframe
sport_keys <- map_dfr(content, ~ as_tibble(.x))

# 2.1 get events ----------------------------------------------------------

# define url for event ids
url <- glue("{base_url}/{sport}/events?apiKey={api_key}")

response <- GET(url)

content <- content(response, "parsed")

event_ids <- map_dfr(content, ~as_tibble(.x))

# 2.2 get props -----------------------------------------------------------


markets <- "player_rush_yds"
event_id <- event_ids$id[1]

# Build the URL for the API call
url <- glue("{base_url}/{sport}/events/{event_id}/odds?apiKey={api_key}&regions=us&markets={markets}&oddsFormat={oddsFormat}")

# Make the API request
response <- GET(url)

# Parse the content of the response
content <- content(response, "parsed")

# Start with top-level data
top_level_data <- tibble(
  id = content$id,
  sport_key = content$sport_key,
  sport_title = content$sport_title,
  commence_time = content$commence_time,
  home_team = content$home_team,
  away_team = content$away_team
)

# Extract bookmakers
bookmakers_data <- list_rbind(map(content$bookmakers, ~ {
  tibble(
    id = content$id,  # Include the event ID to join later
    bookmaker_key = .x$key,
    bookmaker_title = .x$title,
    markets = list(.x$markets)  # Preserve markets for further flattening
  )
}))

# Extract markets
markets_data <- list_rbind(map(bookmakers_data$markets, ~ {
  list_rbind(map(.x, ~ {
    tibble(
      bookmaker_key = bookmakers_data$bookmaker_key,  # Include bookmaker key explicitly
      market_key = .x$key,
      last_update = .x$last_update,
      outcomes = list(.x$outcomes)  # Preserve outcomes for further flattening
    )
  }))
}))

# Extract outcomes
outcomes_data <- list_rbind(map(markets_data$outcomes, ~ {
  list_rbind(map(.x, ~ {
    tibble(
      market_key = markets_data$market_key,  # Include market key explicitly
      outcome_name = .x$name,
      description = .x$description,
      price = .x$price,
      point = .x$point
    )
  }))
}))

# Combine all levels using joins
final_data <- top_level_data %>%
  left_join(bookmakers_data, by = "id") %>%
  left_join(markets_data, by = "bookmaker_key") %>%
  left_join(outcomes_data, by = "market_key") %>% 
  mutate(join = paste0(id, bookmaker_key, market_key, outcome_name, description, price, point)) %>% 
  distinct(join, .keep_all = TRUE) %>% 
  select(id, bookmaker_key, market_key, outcome_name, description, price, point, last_update, join)


# Combine all dataframes in the list into a single dataframe (optional)
combined_data <- list_rbind(rushing_yard_list)

# View the result
print(combined_data)

# 2.3 all event rush props ----------------------------------------------------


rushing_yard_list <- lapply(event_ids$id, function(event_id){
  
  markets = "player_rush_yds"
  
  # Build the URL for the API call
  url <- glue("{base_url}/{sport}/events/{event_id}/odds?apiKey={api_key}&regions=us&markets={markets}&oddsFormat={oddsFormat}")
  
  # Make the API request
  response <- GET(url)
  
  # Parse the content of the response
  content <- content(response, "parsed")
  
  # Start with top-level data
  market <- map_df(content$bookmakers, ~as_tibble(.x))
  
  # Start with top-level data
  top_level_data <- tibble(
    id = content$id,
    sport_key = content$sport_key,
    sport_title = content$sport_title,
    commence_time = content$commence_time,
    home_team = content$home_team,
    away_team = content$away_team
  )
  
  # Extract bookmakers
  bookmakers_data <- map_dfr(content$bookmakers, ~ {
    tibble(
      id = content$id,  # Include the event ID to join later
      bookmaker_key = .x$key,
      bookmaker_title = .x$title,
      markets = list(.x$markets)  # Preserve markets for further flattening
    )
  })
  
  # Extract markets
  markets_data <- map_dfr(bookmakers_data$markets, ~ map_dfr(.x, ~ {
    tibble(
      bookmaker_key = bookmakers_data$bookmaker_key,  # Include bookmaker key to join later
      market_key = .x$key,
      last_update = .x$last_update,
      outcomes = list(.x$outcomes)  # Preserve outcomes for further flattening
    )
  }))
  
  # Extract outcomes
  outcomes_data <- map_dfr(markets_data$outcomes, ~ map_dfr(.x, ~ {
    tibble(
      market_key = markets_data$market_key,  # Include market key to join later
      outcome_name = .x$name,
      description = .x$description,
      price = .x$price,
      point = .x$point
    )
  }))
  
  # Combine all levels using joins
  final_data <- top_level_data %>%
    left_join(bookmakers_data, by = "id") %>%
    left_join(markets_data, by = "bookmaker_key") %>%
    left_join(outcomes_data, by = "market_key") %>% 
    mutate(join = paste0(id, bookmaker_key, market_key, outcome_name, description, price, point)) %>% 
    distinct(join, .keep_all = T) %>% 
    select(id, bookmaker_key, market_key, outcome_name, description, price, point, last_update, join)

  return(final_data)
  
  })

# Combine all dataframes in the list into a single dataframe (optional)
all_rush_yards <- bind_rows(rushing_yard_list)

library(purrr)
library(vctrs)

rushing_yard_list <- lapply(event_ids$id, function(event_id) {
  
  markets <- "player_rush_yds"
  
  # Build the URL for the API call
  url <- glue("{base_url}/{sport}/events/{event_id}/odds?apiKey={api_key}&regions=us&markets={markets}&oddsFormat={oddsFormat}")
  
  # Make the API request
  response <- GET(url)
  
  # Parse the content of the response
  content <- content(response, "parsed")
  
  # Start with top-level data
  top_level_data <- tibble(
    id = content$id,
    sport_key = content$sport_key,
    sport_title = content$sport_title,
    commence_time = content$commence_time,
    home_team = content$home_team,
    away_team = content$away_team
  )
  
  # Extract bookmakers
  bookmakers_data <- list_rbind(map(content$bookmakers, ~ {
    tibble(
      id = content$id,  # Include the event ID to join later
      bookmaker_key = .x$key,
      bookmaker_title = .x$title,
      markets = list(.x$markets)  # Preserve markets for further flattening
    )
  }))
  
  # Extract markets
  markets_data <- list_rbind(map(bookmakers_data$markets, ~ {
    list_rbind(map(.x, ~ {
      tibble(
        bookmaker_key = bookmakers_data$bookmaker_key,  # Include bookmaker key explicitly
        market_key = .x$key,
        last_update = .x$last_update,
        outcomes = list(.x$outcomes)  # Preserve outcomes for further flattening
      )
    }))
  }))
  
  # Extract outcomes
  outcomes_data <- list_rbind(map(markets_data$outcomes, ~ {
    list_rbind(map(.x, ~ {
      tibble(
        market_key = markets_data$market_key,  # Include market key explicitly
        outcome_name = .x$name,
        description = .x$description,
        price = .x$price,
        point = .x$point
      )
    }))
  }))
  
  # Combine all levels using joins
  final_data <- top_level_data %>%
    left_join(bookmakers_data, by = "id") %>%
    left_join(markets_data, by = "bookmaker_key") %>%
    left_join(outcomes_data, by = "market_key") %>% 
    mutate(join = paste0(id, bookmaker_key, market_key, outcome_name, description, price, point)) %>% 
    distinct(join, .keep_all = TRUE) %>% 
    select(id, bookmaker_key, market_key, outcome_name, description, price, point, last_update, join)
  
  return(final_data)
})

# Combine all dataframes in the list into a single dataframe (optional)
combined_data <- list_rbind(rushing_yard_list)

# View the result
print(combined_data)
