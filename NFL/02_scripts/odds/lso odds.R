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
sport = "american_football"

base_url <- "https://api.the-odds-api.com/"

# Define the API endpoint for fetching NFL odds
url <- glue("{base_url}/v4/sports/?apiKey={api_key}")

response <- GET(url)
response

content <- content(response, "parsed")
content


# 2.1 get events ----------------------------------------------------------

url <- glue("{base_url}/v4/sports/{sport}/events?apiKey={api_key}")

response <- GET(url)

content <- content(response, "parsed")
content

# 2.2 get props -----------------------------------------------------------

apiKey = api_key,
regions = "us"
markets = "player_rush_yds"
oddsFormat = "american"

url <- glue("https://api.the-odds-api.com/v4/sports/americanfootball_nfl/
            events/a512a48a58c4329048174217b2cc7ce0/
            odds?
            apiKey={api_key}&
            regions=us&
            markets={markets}&
            oddsFormat={oddsFormat}")


# Query parameters
params <- list(
  apiKey = api_key,
  regions = "us",          # Restrict to U.S. bookmakers
  markets = "player_rush_yds",  # Get player rushing yards props
  oddsFormat = "american"  # American odds format
)

url <- "https://api.the-odds-api.com/v4/sports/americanfootball_nfl/odds"

# Make the GET request
response <- GET(url, query = params)

# Check the response status
if (status_code(response) == 200) {
  # Parse the JSON response
  data <- fromJSON(content(response, "text", encoding = "UTF-8"), flatten = TRUE)
  
  # Filter for FanDuel Sportsbook
  fanduel_data <- data %>%
    filter(bookmakers.key == "fanduel") %>%
    select(bookmakers.markets.outcomes) %>%
    unnest_wider(bookmakers.markets.outcomes)
  
  # Clean and format the data
  fanduel_props <- fanduel_data %>%
    unnest(cols = c(name, price, point)) %>%
    select(player = name, odds = price, rushing_yards_line = point)
  
  # Display the data
  print(fanduel_props)
} else {
  # Print error message if the request fails
  cat("Error: Unable to fetch data. Status code:", status_code(response), "\n")
}
