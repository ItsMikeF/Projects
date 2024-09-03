# get sports from live sports odds api

# Load necessary libraries
library(httr)
library(jsonlite)
library(glue)
library(purrr)
library(dplyr)

# Your API key
api_key <- "114ea856077620641393f6fcf173cf11"

# Base URL for all calls
base_url <- "https://api.the-odds-api.com" 

# get sports
endpoint <- glue("/v4/sports/?apiKey={api_key}") # define endpoint
url <- paste0(base_url, endpoint) # define endpoint URL
request <- GET(url) # request the api endpoint
content <- content(request, "parsed") # parse response
df <- do.call(rbind, lapply(content, as.data.frame))

# get game odds
sport <- "baseball_mlb"
region <- "us"
markets <- "spreads"
url <- glue("{base_url}/v4/sports/{sport}/odds/?apiKey={api_key}&regions={region}&markets={markets}")
request <- GET(url)
content <- content(request, "parsed")
df1 <- do.call(rbind, lapply(content, as.data.frame))

# get events
sport <- "baseball_mlb"
region <- "us"

url2 <- glue("{base_url}/v4/sports/{sport}/events?apiKey={api_key}")
request2 <- GET(url2)
content2 <- content(request2, "parsed")
df2 <- do.call(rbind, lapply(content2, as.data.frame))
df2$id


# endpoint for prop odds
sport <- "baseball_mlb"
eventId <- "0fa4f95b55265402ee718dc613c1d684"
region <- "us"
markets <- "pitcher_strikeouts"
oddsFormat <- "american"
dateFormat <- "iso"

url3 <- glue("{base_url}/v4/sports/{sport}/events/{eventId}/odds?apiKey={api_key}&regions={region}&markets={markets}&dateFormat={dateFormat}&oddsFormat={oddsFormat}")
request3 <- GET(url3)
content3 <- content(request3, "parsed")
df3 <- do.call(rbind, lapply(content3, as.data.frame))

content3$bookmakers

# Assuming content3 is the main list, and we want to extract all nested information
bookmakers_list <- content3$bookmakers

fetch_event_data <- function(eventId) {
  # API parameters
  sport <- "baseball_mlb"
  region <- "us"
  markets <- "pitcher_strikeouts"
  oddsFormat <- "american"
  dateFormat <- "iso"
  
  # Construct the API URL
  url3 <- glue("{base_url}/v4/sports/{sport}/events/{eventId}/odds?apiKey={api_key}&regions={region}&markets={markets}&dateFormat={dateFormat}&oddsFormat={oddsFormat}")
  
  # Make the API request while ignoring SSL hostname mismatch
  request3 <- GET(url3, config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE))
  
  # Parse the content
  content3 <- content(request3, "parsed")
  
  # Check if content3 is NULL or empty to avoid errors
  if (is.null(content3) || length(content3) == 0) {
    return(NULL)
  }
  
  # Extract bookmakers list
  bookmakers_list <- content3$bookmakers
  
  # Flatten the list into a DataFrame
  flattened_df <- map_df(bookmakers_list, function(bookmaker) {
    bookmaker_key <- bookmaker$key
    bookmaker_title <- bookmaker$title
    
    # Flatten markets
    map_df(bookmaker$markets, function(market) {
      market_key <- market$key
      market_last_update <- market$last_update
      
      # Flatten outcomes
      map_df(market$outcomes, function(outcome) {
        tibble(
          event_id = eventId,
          bookmaker_key = bookmaker_key,
          bookmaker_title = bookmaker_title,
          market_key = market_key,
          market_last_update = market_last_update,
          outcome_name = outcome$name,
          outcome_description = outcome$description,
          outcome_price = outcome$price,
          outcome_point = outcome$point
        )
      })
    })
  })
  
  return(flattened_df)
}

# Retry to fetch data
combined_df <- map_df(df2$id, fetch_event_data)

unique(combined_df$bookmaker_key)

