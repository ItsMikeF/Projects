#lets compare player prop odds from dk and fd

# 0.0 Load required packages ----------------------------------------------

#load packages
suppressMessages({
  library(tidyverse) #ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, forcats
  library(nflverse) #nflfastr nflseedr nfl4th, nflreadr, nflplotr
  library(glue) #interpreted string literals
  library(rvest) #easily harvest (scrape) web pages
  library(httr) #tools for working with URLs and HTTP
  library(ggimage) #use image in ggplot2
  library(gt) #easiyl create presentation ready display tables
})


# 1.0 scrape dk odds ------------------------------------------------------

dk_scraper <- function(url) {
  
  #toggle this on and off for testing
  url <- "https://sportsbook.draftkings.com/leagues/football/nfl?category=rush/rec-props&subcategory=rec-yds"
  webpage <- read_html(url)
  
  css1 <- ".sportsbook-event-accordion__title-wrapper" #player
  css2 <- "th+ .sportsbook-table__column-row .sportsbook-outcome-cell__line" #lines
  css3 <- "th+ .sportsbook-table__column-row .default-color" #odds
  
  teams <- html_elements(webpage, css1)
  html_table(webpage)
  html_text(teams)
  
  lines <- html_text(html_elements(webpage, css2)) %>% as.numeric()
  totals<- html_text(html_elements(webpage, css3)) %>% as.numeric()
  
  dk_odds <<- as.data.frame(cbind(teams, lines, totals)) %>% 
    mutate(lines = as.numeric(lines),
           totals = as.numeric(totals))
}

dk_scraper("https://sportsbook.draftkings.com/leagues/football/nfl?category=rush/rec-props&subcategory=rec-yds")
