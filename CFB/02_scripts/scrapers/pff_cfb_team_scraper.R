#lets scrape the pff cfb team data table

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

url <- "https://premium.pff.com/ncaa/teams/2021/REGPO"
css1 <- ".kyber-grade-badge__info-text"

webpage <- read_html(url)

test <- html_elements(webpage, css1)
