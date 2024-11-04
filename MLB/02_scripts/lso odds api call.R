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