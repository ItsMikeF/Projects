library(XML)
library(RCurl)
library(dplyr)
library(data.table)
library(tictoc)
library(tidyverse)
library(lubridate)
library(stats)

user <- unlist(strsplit(getwd(), "/"))
user <- user[3]

setwd(paste0("C://Users//",user,"//Documents//Github//DFS_Data//Data_CBB//2022MM"))

### Point System

point_system <- tibble(10,20,40,80,160,320)
names(point_system) <- c("R32", "S16", "E8", "F4", "S2", "C")

### MM Data ###

historical_mm <- read.csv("NCAA Mens March Madness Historical Results.csv")

historical_mm$Date <- year(format(as.Date(historical_mm$Date, "%m/%d/%Y")))
#historical_mm$Date <- if_else(historical_mm$Date >80, historical_mm$Date + 1900, historical_mm$Date + 2000)
historical_mm <- historical_mm %>% 
  filter(Date > 2001) %>% 
  filter(Round != "Opening Round")

### Kenpom Data ###

kenpom <- read.csv("Kenpom.csv")
kenpom$metric <- round((kenpom$adj_em^2) * kenpom$sos_adj_em, digits = 1)

### Back Testing Metric ###

kenpom_metric_table <- tibble(c(2002:2019, 2021))
names(kenpom_metric_table) <- "year"

years <- c(2002:2019, 2021)

for(i in years){
  cat("Getting", i, "\n")
  kenpom_metric <- kenpom %>% 
    filter(year == i) %>% 
    arrange(-metric)
  kenpom_metric_table[if_else(i<2020,i-2001,i-2002),2] <- max(kenpom_metric$metric)
  kenpom_metric_table[if_else(i<2020,i-2001,i-2002),3] <- kenpom_metric[1,24]
  historical_mm_metric <- historical_mm %>% 
    filter(Date == i & Round == "National Championship") 
  kenpom_metric_table[if_else(i<2020,i-2001,i-2002),4] <- historical_mm_metric[1,5]
}

names(kenpom_metric_table)[2:4] <- c("metric", "team", "winner")
kenpom_metric_table$correct <- if_else(kenpom_metric_table$team == kenpom_metric_table$winner,1,0)

cat("Metric:", "kenpom$adj_em^2 * kenpom$sos_adj_em\n", sum(kenpom_metric_table$correct), "predicted champions on", 
    dim(kenpom_metric_table)[1], "tournaments,",
    round((sum(kenpom_metric_table$correct / dim(kenpom_metric_table)[1]*100)), digits = 1), "% winning")