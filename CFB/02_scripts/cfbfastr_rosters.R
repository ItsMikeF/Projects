#lets scrape the depth charts 

#load packages
suppressMessages({
  library(tidyverse) #ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, forcats
  library(cfbfastR) #access cfb pbp data
})

test <- read_rds("./01_data/pbp_players_pos_2023.rds")

teams <- sort(unique(test$pos_team))
teams

test %>% filter(pos_team == "USC")
