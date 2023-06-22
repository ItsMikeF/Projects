#Import packages
library(tidyverse)
library(rvest)
library(XML)
library(httr)
library(RCurl)


sharknado <- html("[https://www.imdb.com/title/tt8031422/](https://www.imdb.com/title/tt8031422/)")

sharknado %>%
  
  html_nodes("table") %>%
  
  .[[1]] %>%
  
  html_table()
