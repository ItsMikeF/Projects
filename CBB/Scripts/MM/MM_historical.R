#this is the backtesting file 

# 0.0 load packages -------------------------------------------------------

library(XML)
library(RCurl)
library(dplyr)
library(data.table)
library(tictoc)
library(tidyverse)
library(lubridate)
library(stats)
library(glue)


# 1.0 Establish the year --------------------------------------------------


year <- year(today())

# 1.1 Establish Point System ----------------------------------------------


point_system <- tibble(10,20,40,80,160,320)
names(point_system) <- c("R32", "S16", "E8", "F4", "S2", "C")


# 2.0 Load Data -----------------------------------------------------------


historical_mm <- read.csv("./Training_data/ncaa_mm_historical_results.csv") %>% 
  mutate(Date = year(format(as.Date(Date, "%m/%d/%Y")))) %>% 
  filter(Date > 2001) %>% 
  filter(Round != "Opening Round")

kenpom <- read.csv(glue("./Training_data/{year}MM/Kenpom.csv"))

# 2.1 Define the metric ---------------------------------------------------


kenpom$metric <- round((kenpom$adj_em^2) * kenpom$sos_adj_em, digits = 1)

# 3.0 create the metric table ---------------------------------------------


kenpom_metric_table <- tibble(c(2002:2019, 2021:2023))
names(kenpom_metric_table) <- "year"

years <- c(2002:2019, 2021:2023)

for(i in years){
  cat("Getting", i, "\n")
  kenpom_metric <- kenpom %>% 
    filter(year == i) %>% 
    arrange(-metric)
  kenpom_metric_table[if_else(i<2020,i-2001,i-2002),2] <- max(kenpom_metric$metric)
  kenpom_metric_table[if_else(i<2020,i-2001,i-2002),3] <- kenpom_metric[1,3]
  historical_mm_metric <- historical_mm %>% 
    filter(Date == i & Round == "National Championship") 
  kenpom_metric_table[if_else(i<2020,i-2001,i-2002),4] <- historical_mm_metric[1,5]
}

names(kenpom_metric_table)[2:4] <- c("metric", "pick", "winner")

#bring in the odds
kenpom_metric_table$join <- paste0(kenpom_metric_table$year, kenpom_metric_table$winner)

hist_odds <- read.csv("./Training_data/historical_mm_odds.csv") %>% 
  select(Year, Team, `Round.1`) %>% 
  rename(odds = `Round.1`) %>% 
  mutate(join = paste0(Year, Team))

kenpom_metric_table <- kenpom_metric_table %>% 
  left_join(hist_odds %>% select(odds, join), by=c("join")) %>% select(-join) %>% 
  arrange(-year)

kenpom_metric_table$correct <- if_else(kenpom_metric_table$pick == kenpom_metric_table$winner,1,0)

kenpom_metric_table$odds[1] <- 800

# 3.1 Results Print Out ---------------------------------------------------


cat("Metric:", "kenpom$adj_em^2 * kenpom$sos_adj_em\n", sum(kenpom_metric_table$correct, na.rm = T), "predicted champions on", 
    dim(kenpom_metric_table)[1]-1, "tournaments,",
    round((sum(kenpom_metric_table$correct, na.rm = T) / (dim(kenpom_metric_table)[1]-1)*100), digits = 1), "% winning")


kenpom_metric_table %>% drop_na() %>% group_by(correct) %>% summarize(odds = mean(odds))
