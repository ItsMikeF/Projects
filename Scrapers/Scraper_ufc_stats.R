library(tidyverse)
library(rvest)
library(XML)
library(lubridate)
library(stats)
library(tictoc)

webpage_ufc <- read_html('http://ufcstats.com/fight-details/7d4e49d8a6678157')

fighters_dk <- html_nodes(webpage_dk,'.b-fight-details__table-text')
