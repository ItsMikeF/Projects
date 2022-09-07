#lets play look the cfb talent

#load packages
suppressMessages({
  library(tidyverse) #ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, forcats
  library(glue) #interpreted string literals
  library(rvest) #easily harvest (scrape) web pages
  library(httr) #tools for working with URLs and HTTP
  library(cfbfastR) #access cfb pbp data
  library(ggimage) #use image in ggplot2
})

#talent table
talent <- cfbd_team_talent() %>% 
  left_join(cfb_teams, by=c("school"))
  
  talent %>% 
  slice_head(n=10) %>% 
  ggplot(aes(x=school,y=talent)) +
  geom_point() +
  geom_image(aes(image = logo)) +
  labs(
    title = "2021 School Talent"
  )