#Import packages
library(tidyverse)
library(rvest)
library(XML)
library(httr)
library(RCurl)
library(data.table)

#Define inputs
url <- "https://datagolf.com/course-table?sort_cat=scoring&sort=adj_par_4_score&diff=hardest"
url <- "https://www.theguardian.com/world/2017/jun/26/angela-merkel-and-donald-trump-head-for-clash-at-g20-summit"
css1 <- "div.data.course-col.selectorgadget_selected"
css2 <- ".parent-expanded"

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