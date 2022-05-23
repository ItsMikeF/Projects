library(tidyverse)
library(rvest)
library(XML)
library(httr)

#Define inputs
url <- "https://sportsbook.draftkings.com/leagues/golf/88671886"
css1 <- ".side-rail-name"
css2 <- ".component-18-side-rail+ .component-18 .default-color"

#Read html page
webpage_dk <- read_html(url)

#Read text nodes
words <- html_text(html_nodes(webpage_dk,css1))

#Read numeric nodes
numbers <- as.numeric(html_text(html_nodes(webpage_dk,css2)))

#Create data table
table <- data.frame(cbind(words, numbers))

#Option to Write csv
#write.csv(table, file = "table.csv")
