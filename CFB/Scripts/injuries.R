#get cfb injuries from covers.com

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

injury_report <- function(url) {
  css1 <- ".covers-CoversMatchups-Table a" #players
  webpage <- read_html(url)
  
  injuries <- html_text(html_elements(webpage, css1)) %>% as.character() %>% 
    str_replace_all("[\r\n]" , "")
  injuries <- gsub(" ","",injuries)
  injuries <<- injuries[-c(which(injuries == ""))]
}

injury_report("https://www.covers.com/sport/football/ncaaf/injuries")
