library(tidyverse)
library(rvest)
library(XML)
library(httr)

webpage_pff <- read_html('https://premium.pff.com/nfl/teams/2021/REGPO')

outrights_pff <- html_text(html_nodes(webpage_pff,'.justify-content-start'))

stats_pff <- as.numeric(html_text(html_nodes(webpage_pff,'.kyber-grade-badge__info-text')))

outrights_odds_pff <- data.frame(cbind(outrights_pff, stats_pff))
#rm(outrights_pff, odds_pff)
#write.csv(outrights_odds_pff, file = "outrights_odds_pff.csv")