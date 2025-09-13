#lets read the lso api

#load packages
suppressMessages({
  library(tidyverse) #ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, forcats
  library(glue) #interpreted string literals
  library(rvest) #easily harvest (scrape) web pages
  library(httr) #tools for working with URLs and HTTP
  library(cfbfastR) #access cfb pbp data
  library(ggimage) #use image in ggplot2
})

#lets look at the live sports odds API
{
  url <- "https://odds.p.rapidapi.com/v4/sports/americanfootball_ncaaf/odds"
  
  queryString <- list(
    regions = "us",
    oddsFormat = "american",
    markets = "spreads",
    dateFormat = "iso"
  )
  
  response <- VERB("GET", url, add_headers('X-RapidAPI-Key' = 'b34680a5eamsh5c39b8cf066dd0ap1e8d93jsnc9e5097b3c8c', 'X-RapidAPI-Host' = 'odds.p.rapidapi.com'), query = queryString, content_type("application/octet-stream"))
  
  content <- content(response, "parsed")
  
  #filter out future games
  content <- content[-c(47:95)]
  
  #create a df of the nested data
  df <- data.frame()
  
  for (i in 1:length(content)) {
    df[i,1] = content[[i]]$bookmakers[[1]]$markets[[1]]$outcomes[[1]]$name
    df[i,2] = content[[i]]$bookmakers[[1]]$markets[[1]]$outcomes[[1]]$point
    
    df[i,3] = content[[i]]$bookmakers[[1]]$markets[[1]]$outcomes[[2]]$name
    df[i,4] = content[[i]]$bookmakers[[1]]$markets[[1]]$outcomes[[2]]$point
  }
  
  a <- df[,c(1:2)]
  b <- df[,c(3:4)]
  names(b) <- names(a)
  
  dg <- rbind(a,b)
  names(dg) <- c("school_mascot", "spread")
}
