#lets evolve nfl dfs

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

week <- 2

dk_scraper <- function(url) {
  webpage <- read_html(url)
  
  css1 <- ".event-cell__name-text" #teams
  css2 <- ".no-label .sportsbook-outcome-cell__line" #lines
  css3 <- "span+ .sportsbook-outcome-cell__line" #totals
  
  teams <- html_text(html_elements(webpage, css1))
  lines <- html_text(html_elements(webpage, css2)) %>% as.numeric()
  totals<- html_text(html_elements(webpage, css3)) %>% as.numeric()
  
  dk_odds <<- as.data.frame(cbind(teams, lines, totals)) %>% 
    mutate(lines = as.numeric(lines),
           totals = as.numeric(totals))
  
  time = Sys.time() %>% as.character() %>% str_replace_all(.,":","") %>% substr(1,15)
  write.csv(dk_odds, file = glue("./contests/2022_w{week}/odds/{time}_nfl_dk_odds.csv"))
}

dk_scraper("https://sportsbook.draftkings.com/leagues/football/nfl")

games <- 13
dk_odds_slate <- dk_odds[1:(games*2),]

# 2.0 scrape the injury report --------------------------------------------


# 3.0 scrape depth charts -------------------------------------------------


# 4.0 load projections ----------------------------------------------------

