# dk nfl prop scraper

#load packages
suppressMessages({
  library(dplyr) #ggplot2 dplyr tidyr readr stringr forcats purrr tibble
  library(purrr)
  library(stringr)
  library(rvest) #easily harvest (scrape) web pages
  library(glue)
})


# 1.0 passing props -------------------------------------------------------


# define prop categories
pass_props <- c("pass-tds", "pass-yds", "alt-pass-yds", "pass-+-rush-yds", 
                "pass-completions", "pass-attempts", "interceptions")

url <- glue("https://sportsbook.draftkings.com/leagues/football/nfl?category=passing-props-&subcategory={pass_props[1]}")

# define webpage
webpage <- read_html(url)

css <- ".sportsbook-outcome-cell__line , .sportsbook-outcome-cell__label , .default-color , .sportsbook-row-name"

# find nodes
html <- html_nodes(webpage, css)
text <- trimws(html_text(html)) %>% as_tibble()


# 2.0 rush/rec props ------------------------------------------------------

rush_rec_props <- c("rush-tds", "rec-tds", "alt-rush-yds", "alt-rec-yds",
                    "receptions", "rush-+-rec-yds", "alt-rush-+-rec-yds", 
                    "rush-attempts", "longest-rush", "longest-reception")

# 3.0 d/st props ----------------------------------------------------------


dst_props <- c("sacks", "tackles-+-ast", "fg-made", "kicking-pts", "solo-tackles")