#scrape our lads depth charts

#load packages
suppressMessages({
  library(tidyverse) #ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, forcats
  library(glue) #interpreted string literals
  library(rvest) #easily harvest (scrape) web pages
  library(httr) #tools for working with URLs and HTTP
  library(cfbfastR) #access cfb pbp data
  library(ggimage) #use image in ggplot2
  library(tictoc) #functions for timing r scripts
})


# 1.0 scrape ourlads ------------------------------------------------------

url <- "https://www.ourlads.com/ncaa-football-depth-charts/depth-chart/texas/92016"
url <- "https://www.ourlads.com/ncaa-football-depth-charts/depth-chart/alabama/89923"
webpage <- GET(url)
parsed_content <- content(webpage, as = "parsed")

td_data <- parsed_content %>% 
  html_nodes("td") %>% 
  html_text()
td_data

ncol <- 11
data_frame <- data.frame(matrix(td_data, ncol = ncol, byrow = TRUE))


# 2.0 separate offense / defense ------------------------------------------

# get offense
offense <- data_frame[2:12,] %>% select(2:4) %>% 
  rename(player1 = X4) %>% 
  separate(player1, into = c("last_name", "first_name"), sep = ", ") %>% 
  separate(first_name, into = c("first_name", "classmen1"), sep = " ", extra = "merge") %>% 
  mutate(player1 = paste(first_name, last_name)) %>% 
  select(-c(2,3)) %>% 
  select(1,3,4)

# get defense
defense <- data_frame[14:25,] %>% select(3:5) %>% 
  rename(player1 = X5) %>% 
  separate(player1, into = c("last_name", "first_name"), sep = ", ") %>% 
  separate(first_name, into = c("first_name", "classmen1"), sep = " ", extra = "merge") %>% 
  mutate(player1 = paste(first_name, last_name)) %>% 
  select(-c(2,3)) %>% 
  select(1,3,4)


# 3.0 load pff data -------------------------------------------------------


# load pff data
blocking <- read.csv("./01_data/contests/2023_w1/offense_blocking.csv") %>% 
  select(player, grades_pass_block, grades_run_block, snap_counts_offense)

def <- read.csv("./01_data/contests/2023_w1/defense_summary.csv") %>% 
  select(player, grades_defense, grades_pass_rush_defense, grades_run_defense, grades_coverage_defense)

qb <- read.csv("./01_data/contests/2023_w1/passing_summary.csv") %>% 
  select(player, grades_pass)

rb <- read.csv("./01_data/contests/2023_w1/rushing_summary.csv") %>% 
  select(player, grades_run)

wr <- read.csv("./01_data/contests/2023_w1/receiving_summary.csv") %>% 
  select(player, grades_pass_route)


# 4.0 join pff data -------------------------------------------------------


defense <- defense %>% 
  left_join(def, by = c("player1"="player"))
view(defense)

offense <- offense %>% 
  left_join(qb, by=c("player1"="player")) %>% 
  left_join(rb, by=c("player1"="player")) %>%
  left_join(wr, by=c("player1"="player")) %>%
  left_join(blocking, by=c("player1"="player"))
view(offense)
