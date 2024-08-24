# get sports from live sports odds api

# Load necessary libraries
library(httr)
library(jsonlite)
library(glue)

# Your API key
api_key <- "114ea856077620641393f6fcf173cf11"

# Base URL 
base_url <- "https://api.the-odds-api.com" 

# Eefine endpoint
endpoint <- glue("/v4/sports/?apiKey={api_key}")

# Eefine endpoint URL
url <- paste0(base_url, endpoint)

# request the api endpoint
request <- GET(url)

content <- content(response, "parsed")

df <- do.call(rbind, lapply(content, as.data.frame))
