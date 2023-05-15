# Get nhl odds via lso api

# Load packages
library(httr)
library(tidyverse)
library(glue)
library(rlist)
library(pipeR)

# Write key and url
key <- "icehockey_nhl"
url <- glue("https://odds.p.rapidapi.com/v4/sports/{key}/odds")

# Read the API
queryString <- list(
  regions="us", oddsFormat="american", dateFormat="iso"
)

response <- VERB("GET", url, add_headers('X-RapidAPI-Host' = 'odds.p.rapidapi.com', 
                                         'X-RapidAPI-Key' = 'b34680a5eamsh5c39b8cf066dd0ap1e8d93jsnc9e5097b3c8c'),
                 query = queryString, content_type("application/octet-stream"))

content <- content(response, "parsed")

# Load in bookmakers
bookmakers <- as.vector(unlist(read.csv("./data/bookmakers.csv")))

#Add Functions
convert_ML <- function(odds) {
  breakeven <- if_else(odds > 0, 100 / (100 + odds), abs(odds) / (abs(odds) + 100))
  return(round(breakeven, digits = 4))
}

# Move data to a dataframe
list <- list()
for (j in 1:length(content)) {
  
  df <- data.frame()
  
  for (i in 1:13) {
    df[i,1] <- content[[j]]$bookmakers[[i]]$title
    df[i,2] <- content[[j]]$bookmakers[[i]]$markets[[1]]$outcomes[[1]]$name
    df[i,3] <- content[[j]]$bookmakers[[i]]$markets[[1]]$outcomes[[1]]$price
    df[i,4] <- convert_ML(df[i,3])
    
    df[i,5] <- content[[j]]$bookmakers[[i]]$markets[[1]]$outcomes[[2]]$name
    df[i,6] <- content[[j]]$bookmakers[[i]]$markets[[1]]$outcomes[[2]]$price
    df[i,7] <- convert_ML(df[i,6])
    
    df[i,8] <- content[[j]]$bookmakers[[i]]$last_update
    
    colnames(df) <- c("book", "away_team", "away_odds_ml", "away_odds_imp","home_team", "home_odds_ml", "home_odds_imp","last_update")
    
    df <- df %>% arrange(book)
    
    list[[j]] <- df
  }
  
}

#lets do some eda on the dataframes in the list
for (i in 1:length(content)) {
  print(paste(list[[i]]$away_team[1], round(mean(list[[i]]$away_odds_imp), digits = 3)))
  print(paste(list[[i]]$home_team[1], round(mean(list[[i]]$home_odds_imp), digits = 3)))
}

# Transpose testing
odds <- rbind(as.tibble(t(list[[1]] %>% select(book, away_odds_ml, home_odds_ml))), 
              as.tibble(t(list[[2]] %>% select(away_odds_ml, home_odds_ml))), 
              as.tibble(t(list[[3]] %>% select(away_odds_ml, home_odds_ml))), 
              as.tibble(t(list[[4]] %>% select(away_odds_ml, home_odds_ml)))
)

# Rename the columns as the bookmakers
colnames(odds) <- odds[1,]
odds <- odds %>% slice(-1)

# Create a vector of the away teams 
away <- map_chr(content, ~ .x$away_team[[1]])

# Create a vector of the home teams
home <- map_chr(content, ~ .x$home_team[[1]])

# Create a vector of the teams for the row names
teams <- flatten_chr(map2(away, home, c))

# Assign the row names
rownames(odds) <- teams
odds
