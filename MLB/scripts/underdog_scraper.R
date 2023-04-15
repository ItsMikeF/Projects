#scrape underdog for opponent draft data

# Load required packages
library(RSelenium)
library(rvest)
library(webdriver)
library(xml2)


# example code ------------------------------------------------------------


url <- "https://rvest.tidyverse.org/articles/starwars.html"
css <- ".styles__draftTitle__lEz9A"

webpage <- read_html(url)

print(xml_find_all(webpage, "//*"), max=200)

films <- webpage %>% html_elements("section")
films

title <- films %>% html_element("h2") %>% html_text2()
title


# test code ---------------------------------------------------------------


url <- "https://underdogfantasy.com/live/best-ball/mlb/1a908de1-0275-4345-b78b-76b63be5be00"
css <- ".styles__draftTitle__lEz9A"

webpage <- read_html(url)

print(xml_find_all(webpage, "//*"), max=200)

films <- webpage %>% html_elements("path")
films

title <- films %>% html_element("d")
title
