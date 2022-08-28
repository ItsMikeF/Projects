#lets read the coinbase api

#load packages
suppressMessages({
  library(httr)
  library(jsonlite)
})

api_key <- "4b9c53aa369df200e9952d0c006baf32"
url <- "https://api.pro.coinbase.com/"

query_string <- list(all=T)

response <- VERB("GET", url, add_headers(
  'CB-ACCESS-KEY' = api_key,
  'X-RapidAPI-Host' = 'odds.p.rapidapi.com'),
  query = query_string,
  content_type("application/json"))

#code snippet from rapid api
library(httr)

url <- "https://odds.p.rapidapi.com/v4/sports"

queryString <- list(all = "true")

response <- VERB("GET", url, add_headers(
  'X-RapidAPI-Key' = 'b34680a5eamsh5c39b8cf066dd0ap1e8d93jsnc9e5097b3c8c',
  'X-RapidAPI-Host' = 'odds.p.rapidapi.com'),
  query = queryString,
  content_type("application/octet-stream"))

content(response, "parse")
