#lets scrape the nfl injury report

#load packages
suppressMessages({
  library(tidyverse) #ggplot2 dplyr tidyr readr stringr forcats purrr tibble
  library(nflverse) #nflfastr nflseedr nfl4th nflreadr nflplotr
  library(rvest) #easily harvest (scrape) web pages
  library(janitor) #simple little tools for examining and cleaning dirty data
  library(ggrepel) #automatically position non-overlapping text labels with ggplot2
  library(tictoc) #Functions for Timing R Scripts, as Well as Implementations of Stack and List Structures
  library(gt) #Easily Create Presentation-Ready Display Tables
})

#define url
url <- "https://www.espn.com/nfl/injuries"

#scrape player injuries
injuries <- html_table(html_elements(read_html(url),".Table__league-injuries"))

#scrape team names
teams <- html_text(html_elements(read_html(url),".ml2")) 
teams <- teams[-1]

#create df of teams and # of players on the injury report
observations <- function(a){
  dim(a)[1]
}

inj_counts <- unlist(lapply(injuries, observations))

#create df 
inj <- as.tibble(cbind(teams, inj_counts)) %>% 
  mutate(inj_counts = as.numeric(inj_counts)) %>% 
  arrange(-inj_counts)

inj %>% 
  slice_head(n=10) %>% 
  gt() %>% 
  tab_header(title = "NFL Injuries") %>% 
  tab_source_note(source_note = "https://www.espn.com/nfl/injuries")
