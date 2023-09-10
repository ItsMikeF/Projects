# add grades to depth charts

# load packages
library(tidyverse)
library(glue)

# depth charts
load("./01_data/cfb_depth_charts.RData")

# define teams
away_team <- "texas"
home_team <- "alabama"

# away starters
away <- as_tibble(depth_charts[[away_team]]) %>% 
  separate(player1, into = c("last_name", "first_name"), sep = ", ") %>% 
  separate(first_name, into = c("first_name", "classmen1"), sep = " ", extra = "merge") %>% 
  mutate(player1 = paste(first_name, last_name)) %>% 
  select(-c(2,3)) %>% 
  separate(player2, into = c("last_name", "first_name"), sep = ", ") %>% 
  separate(first_name, into = c("first_name", "classmen2"), sep = " ", extra = "merge") %>% 
  mutate(player2 = paste(first_name, last_name)) %>% 
  select(-c(3,4)) %>% 
  select(1,4,2,5,3) %>% 
  select(1:3) # only grab first strings

# home starters
home <- as_tibble(depth_charts[[home_team]]) %>% 
  separate(player1, into = c("last_name", "first_name"), sep = ", ") %>% 
  separate(first_name, into = c("first_name", "classmen1"), sep = " ", extra = "merge") %>% 
  mutate(player1 = paste(first_name, last_name)) %>% 
  select(-c(2,3)) %>% 
  separate(player2, into = c("last_name", "first_name"), sep = ", ") %>% 
  separate(first_name, into = c("first_name", "classmen2"), sep = " ", extra = "merge") %>% 
  mutate(player2 = paste(first_name, last_name)) %>% 
  select(-c(3,4)) %>% 
  select(1,4,2,5,3) %>% 
  select(1:3) # only grab first strings

# load pff data
blocking <- read.csv("./01_data/contests/2023_w1/offense_blocking.csv") %>% 
  select(player, grades_pass_block, grades_run_block, snap_counts_offense)

def <- read.csv("./01_data/contests/2023_w1/offense_blocking.csv")

qb <- read.csv("./01_data/contests/2023_w1/passing_summary.csv") %>% 
  select(player, grades_pass)

rb <- read.csv("./01_data/contests/2023_w1/rushing_summary.csv") %>% 
  select(player, grades_run)

wr <- read.csv("./01_data/contests/2023_w1/receiving_summary.csv") %>% 
  select(player, grades_pass_route)

# join pff data
away %>% 
  left_join(qb, by=c("player1"="player")) %>% 
  left_join(rb, by=c("player1"="player")) %>%
  left_join(wr, by=c("player1"="player")) %>%
  left_join(blocking, by=c("player1"="player"))

home %>% 
  left_join(qb, by=c("player1"="player")) %>% 
  left_join(rb, by=c("player1"="player")) %>%
  left_join(wr, by=c("player1"="player")) %>%
  left_join(blocking, by=c("player1"="player"))
