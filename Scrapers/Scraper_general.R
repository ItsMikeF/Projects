#Import packages
library(tidyverse)
library(rvest)
library(XML)
library(httr)
library(RCurl)
library(data.table)

#Define inputs
url <- "https://sportsbook.draftkings.com/leagues/baseball/mlb?category=home-run-derby&subcategory=highest-exit-velocity"
css1 <- ".sportsbook-outcome-cell__label"
css2 <- ".default-color"

#Read html page
webpage <- read_html(url)

getHTMLLinks(webpage)

#Extract paragraphs
webpage %>%
  # extract paragraphs
  html_nodes("p") %>%

#Read text nodes
words <- html_text(html_nodes(webpage,css1))

#Read numeric nodes
numbers <- as.numeric(html_text(html_nodes(webpage,css2)))

#Read HTML table
df <- as.data.frame(readHTMLTable(getURL(url)))

#Create data table
table <- data.frame(cbind(words, numbers))

#Option to Write csv
#write.csv(table, file = "table.csv")