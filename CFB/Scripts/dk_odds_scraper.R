#lets scrape the odds from draftkings

#load packages
suppressMessages({
  library(tidyverse) #ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, forcats
  library(glue) #interpreted string literals
  library(rvest) #easily harvest (scrape) web pages
  library(httr) #tools for working with URLs and HTTP
  library(cfbfastR) #access cfb pbp data
  library(ggimage) #use image in ggplot2
})

url <- "https://sportsbook.draftkings.com/leagues/football/ncaaf"
webpage <- read_html(url)

css1 <- ".event-cell__name-text" #teams
css2 <- ".no-label .sportsbook-outcome-cell__line" #lines
css3 <- "span+ .sportsbook-outcome-cell__line" #totals

teams <- html_text(html_elements(webpage, css1))
lines <- html_text(html_elements(webpage, css2)) %>% as.numeric()
totals<- html_text(html_elements(webpage, css3)) %>% as.numeric()

dk_odds <- as.data.frame(cbind(teams, lines, totals)) %>% 
  mutate(lines = as.numeric(lines),
         totals = as.numeric(totals))

#view the odds
dk_odds_slate <- dk_odds %>% 
  slice_head(n=(games*2)) %>% 
  view(title = "Slate Odds")
