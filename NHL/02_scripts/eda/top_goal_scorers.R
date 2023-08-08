#lets play nhl best ball

#load packages
suppressMessages({
  library(tidyverse) #ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, forcats
  library(glue) #interpreted string literals
  library(rvest) #easily harvest (scrape) web pages
  library(httr) #tools for working with URLs and HTTP
  library(ggimage) #use image in ggplot2
  library(ggrepel) #auto position non-overlapping text labels with ggplot2
  library(ggtext) #improved text rendering support for ggplot2
  library(fastRhockey)
  library(hockeyR)
})


# 1.0 load goalscorer odds ------------------------------------------------

goal_scorerer_odds <- function(url) {
  #read the webpage
  webpage <- read_html(url)
  
  #define the css selectors
  css1 <- ".sportsbook-outcome-cell__label"
  css2 <- ".default-color"
  
  #write selectors to objects
  goal_scorers <- html_elements(webpage, css1) %>% html_text()
  odds <- html_elements(webpage, css2) %>% html_text() %>% as.numeric()
  
  #create df of the teams and odds
  top_goalscorer_odds <<- as.data.frame(cbind(goal_scorers, odds)) %>% 
    mutate_at(c('odds'), as.numeric) %>% 
    mutate(implied = round(100/(100+odds), digits = 3))
  
  time = Sys.time() %>% as.character() %>% str_replace_all(.,":","") %>% substr(1,15)
  write.csv(top_goalscorer_odds, file = glue("./odds/goal_scorer/{time}_goal_scorer_dk_odds.csv"))
  
}

goal_scorerer_odds("https://sportsbook.draftkings.com/leagues/hockey/nhl?category=awards&subcategory=top-goalscorer")

# 2.0 load prior year xg --------------------------------------------------

#hockeyR
pbp <- load_pbp(2022:2023)
names(pbp)

xg_leaders <- pbp %>% 
  filter(event_type %in% c("SHOT", "MISSED_SHOT", "GOAL")) %>% 
  group_by(player = event_player_1_name, id= event_player_1_id) %>% 
  summarize(
    team = last(event_team_abbr), 
    goals = sum(event_type == "GOAL"), 
    xg = round(sum(xg, na.rm = T),1), 
    .groups = "drop"
  ) %>% 
  arrange(-xg) #%>% slice_head(n=10) 

xg_leaders$player <- str_replace(xg_leaders$player, "[.]"," ")

# 3.0 add xg to goal scorer odds ------------------------------------------

top_goalscorer_odds <- top_goalscorer_odds %>% 
  left_join(xg_leaders, by=c("goal_scorers"="player"))

#project the xg for each player
#project ice time for each player
#include team playoff odds
#include goal scorer odds
#create ice time adjusted epa per team based on roster data
#aggregate that over season based on the schedule
