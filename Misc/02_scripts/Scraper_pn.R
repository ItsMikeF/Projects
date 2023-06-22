library(XML)
library(httr)
library(jsonlite)

library(rvest)     # HTML Hacking & Web Scraping
library(jsonlite)  # JSON manipulation
library(tidyverse) # Data Manipulation
library(tidyquant) # ggplot2 theme
library(xopen)     # Opens URL in Browser
library(knitr)     # Pretty HTML Tables

url <- 'https://www.pinnacle.com/en/golf/pga-tour-att-byron-nelson/matchups#futures'
css1 <- ".style_label__2KJur"
css2 <- ".style_price__15SlF"

#Read html page
webpage_pn <- read_html(url)

#Read text nodes
outrights_pn <- html_text(html_nodes(webpage_pn, css1))

#Read numeric nodes
odds_pn <- as.numeric(html_text(html_nodes(webpage_pn, css2)))

#Create data table
outrights_odds_pn <- data.frame(cbind(outrights_pn, odds_pn))

#Code testing area
xopen(url)
?xopen
