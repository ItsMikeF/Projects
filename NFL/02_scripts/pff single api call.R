library(tidyverse)
library(httr)
library(jsonlite)
library(glue)

fetch_pff_data <- function(team_id, week, year, cookies) {
  url <- glue("https://premium.pff.com/api/v1/facet/receiving/summary?league=ncaa&season={year}&franchise_id={team_id}&week={week}&division=fbs")
  response <- GET(url, add_headers(Cookie = cookies))
  
  if (status_code(response) == 200 && length(content(response)$receiving_summary) > 0) {
    df <- as.data.frame(fromJSON(content(response, "text", encoding = "UTF-8"))$receiving_summary)
    df$week <- week
    df$year <- year
    df$team_id <- team_id
    return(df)
  } else {
    return(data.frame())
  }
}

# Example usage:
cookies <- "your_cookie_string_here"
df <- fetch_pff_data(team_id = 101, week = 6, year = 2024, cookies = cookies_pff)
