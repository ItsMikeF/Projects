# Get nhl odds via lso api

# Load packages
suppressMessages({
  library(httr)
  library(tidyverse)
  library(glue)
  library(rlist)
  library(pipeR)
})


# Write key and url
key <- "icehockey_nhl"
url <- glue("https://odds.p.rapidapi.com/v4/sports/{key}/odds")

# Define list of parameters for the API request
queryString <- list(
  regions="us", 
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

#Add Functions
convert_ML <- function(odds) {
  breakeven <- if_else(odds > 0, 100 / (100 + odds), abs(odds) / (abs(odds) + 100))
  return(round(breakeven, digits = 4))
}

#gpt suggested code
df_list <- lapply(content, function(x) {
  data.frame(
    book = sapply(x$bookmakers, function(y) y$title),
    away_team = sapply(x$bookmakers, function(y) y$markets[[1]]$outcomes[[1]]$name),
    away_odds_ml = sapply(x$bookmakers, function(y) y$markets[[1]]$outcomes[[1]]$price),
    away_odds_imp = sapply(x$bookmakers, function(y) convert_ML(y$markets[[1]]$outcomes[[1]]$price)),
    home_team = sapply(x$bookmakers, function(y) y$markets[[1]]$outcomes[[2]]$name),
    home_odds_ml = sapply(x$bookmakers, function(y) y$markets[[1]]$outcomes[[2]]$price),
    home_odds_imp = sapply(x$bookmakers, function(y) convert_ML(y$markets[[1]]$outcomes[[2]]$price)),
    last_update = sapply(x$bookmakers, function(y) y$last_update)
  ) %>%
    mutate(hold = (away_odds_imp + home_odds_imp - 1) * 100)
})

df <- bind_rows(df_list)
view(df)

# Find average odds
away_avg_odds <- sapply(list, function(x) round(mean(x$away_odds_imp), digits = 3))
home_avg_odds <- sapply(list, function(x) round(mean(x$home_odds_imp), digits = 3))

for (i in 1:length(content)) {
  print(paste(content[[i]]$away_team[1], away_avg_odds[i]))
  print(paste(content[[i]]$home_team[1], home_avg_odds[i]))
}

