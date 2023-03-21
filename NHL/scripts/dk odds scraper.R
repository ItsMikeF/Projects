#lets scrape draftkings for nhl odds

library(rvest)
library(RCurl)
library(dplyr)
library(data.table)

url <- "https://sportsbook.draftkings.com/leagues/hockey/nhl?category=team-futures"
page <- read_html(url)

teams <- page %>% html_nodes(".sportsbook-outcome-cell__label") %>% html_text()
odds <- page %>% html_nodes(".default-color") %>% html_text()

df <- tibble(teams, odds)
