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
  
  games <- 12
  dk_odds_slate <- dk_odds[1:(games*2),]
  
  time = Sys.time() %>% as.character() %>% str_replace_all(.,":","") %>% substr(1,15)
  write.csv(dk_odds_slate, file = glue("./contests/2022_w{week}/odds/{time}_nfl_dk_odds.csv"))
}

dk_scraper("https://sportsbook.draftkings.com/leagues/football/nfl")

games <- 12
dk_odds_slate <- dk_odds[1:(games*2),]

dk_odds_delta <- read.csv(paste0("./contests/2022_w2/odds/",list.files(path = "./contests/2022_w2/odds/")[1])) %>% 
  left_join(read.csv(paste0("./contests/2022_w2/odds/",list.files(path = "./contests/2022_w2/odds/")[
    length(list.files(path = "./contests/2022_w2/odds/"))])), by=c("teams")) %>% 
  mutate(delta_spread = lines.y-lines.x, 
         delta_total = totals.y-totals.x)

# 2.0 scrape the injury report --------------------------------------------

injury_report <- function(url) {
  #scrape player injuries tables from espn
  injuries <- html_table(html_elements(read_html(url),".Table__league-injuries"))
  
  #get team names
  teams <- html_text(html_elements(read_html(url),".ml2"))
  teams <- as.data.frame(teams[-1], col.names="team_name") %>% 
    rename("team_name"='teams[-1]')
  
  
  #join team abbreviations to scraped team name dataframe
  teams <- teams %>% 
    left_join(teams_colors_logos %>% select(team_abbr, team_name), 
              by=("team_name"))
  
  #remove LA and add WSH 
  teams <- teams[-c(19),]
  teams[32,2] <- "WSH"
  
  #set names of the list elements as the teams
  injuries <- setNames(injuries, teams$team_abbr)
  
  #add team names to each dataframe
  for (i in 1:32) {
    injuries[[i]]$team <- teams$team_abbr[i]
  }
  
  #combine all dataframes to one dataframe
  injuries <<- as.data.frame(do.call(rbind, injuries)) %>% 
    rename_with(tolower) %>% 
    select(name, pos, team, status, comment)
}

injury_report("https://www.espn.com/nfl/injuries")

# 3.0 scrape depth charts -------------------------------------------------


# 4.0 load projections ----------------------------------------------------

