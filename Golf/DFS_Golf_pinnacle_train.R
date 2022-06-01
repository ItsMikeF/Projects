#Load packages
library(tidyverse, warn.conflicts = F) #metapackage
library(lubridate, warn.conflicts = F)
library(utils)
library(stats)

#Inputs
tournament <- "Charles Schwab"
date <- c("2022-05-29")

tournaments <- c("2022-04-10	The Masters", 
                 "2022-04-17	RBC Heritage", 
                 "2022-05-01	Mexico Open",
                 "2022-05-08	Wells Fargo", 
                 "2022-05-15 AT&T Byron Nelson", 
                 "2022-05-22 PGA Championship", 
                 "2022-05-29 Charles Schwab")

#Set Working Directory
dt <- paste(date, tournament)

setwd(paste0("C://Users//",unlist(strsplit(getwd(), "/"))[3],"//Documents//GitHub//Projects//Golf//Training_data//", dt))

#Import CSVs
golf_salaries <- read.csv("DKSalaries.csv")
rg <- read.csv(list.files(pattern = "projections_draftkings_golf"))
odds_pn <- read.csv("pga_historical_outrights.csv", header = T)
cam <- read.csv(list.files(pattern = "-model"))

#Add Functions
convert_ML <- function(odds) {
  breakeven <- if_else(odds > 0, 100 / (100 + odds), abs(odds) / (abs(odds) + 100))
  return(round(breakeven, digits = 4))
}

#PN Odds Adjustment
odds_pn <- odds_pn %>%
  select(player_name, open_odds, close_odds) %>%
  separate(player_name, into = c("last", "first"), sep = ",")

odds_pn$Golfer <- trimws(paste(odds_pn$first, odds_pn$last))
odds_pn <- odds_pn %>% select(Golfer, open_odds, close_odds)

odds_pn[,2:3] <- sapply(odds_pn[,2:3], convert_ML)
names(odds_pn) <- c("Golfer", "odds_open", "odds_close")

odds_pn$odds_delta <- odds_pn$odds_close - odds_pn$odds_open
odds_pn$odds_delta_per <- round((odds_pn$odds_close - odds_pn$odds_open)/odds_pn$odds_open, digits = 4)

#Correct golfer names in course adj model
cam <- cam %>%
  select(player_name, make_cut, first_round_lead, top_30, top_20, top_10, top_5, top_3, win, observed_finish) %>%
  separate(player_name, into = c("last", "first"), sep = ",")
cam$Golfer <- trimws(paste(cam$first, cam$last))
cam$observed_finish <- gsub('T', '', cam$observed_finish)
cam$observed_finish <- gsub('CU', '100', cam$observed_finish)
cam <- cam %>% 
  select(Golfer, make_cut, first_round_lead, top_30, top_20, top_10, top_5, top_3, win, observed_finish)
cam[,2:9] <- round(cam[,2:9], digits = 4)
cam[,10] <- as.numeric(cam[,10])

#Create golfer tibble
golfers <- golf_salaries %>% 
  left_join(odds_pn, by = c("Name" = "Golfer"))

#Add Odds Rank
golfers$odds_rank <- round(rank(-golfers$odds_close), digits =0)

### Create golfer table ###
golfers <- golfers %>% select(Name, ID, Salary, AvgPointsPerGame, odds_open, odds_close, odds_rank, odds_delta, odds_delta_per) %>% drop_na(odds_close)
rg <- rg %>% select(name, fpts, proj_own, ceil, floor)

golfers <- golfers %>%
  left_join(rg, by=c("Name" = "name"))

golfers <- golfers %>% 
  left_join(cam, by=c("Name" = "Golfer"))

golfers$odds_per_dollar <- round(golfers$odds_close / golfers$Salary * 10^6, digits = 2)

golfers$residuals <- round(residuals(loess(odds_per_dollar ~ Salary, golfers)), digits = 2)

### Ownership Change formula
own_multiplier <- 100/50

golfers$own_change <- round(
  (own_multiplier * 0.15 * golfers$residuals) +
  (own_multiplier * 2) +
  (2 * (1000 * golfers$odds_delta)) +
  (own_multiplier * 2 * if_else(golfers$odds_delta_per > 1, 1, golfers$odds_delta_per)^2), digits = 2)

golfers$adj_own <- case_when(golfers$proj_own + golfers$own_change <= 0 ~ 0,
                             golfers$proj_own + golfers$own_change < 100 ~ round((golfers$proj_own + golfers$own_change)/own_multiplier)*own_multiplier, 
                             golfers$proj_own + golfers$own_change >= 100 ~ 100)

### Modify Results
results <- read.csv(list.files(pattern = "draftkings_pga_"))
results <- results %>%
  select(player_name, ownership, total_pts) %>%
  separate(player_name, into = c("last", "first"), sep = ",")

results$Player <- trimws(paste(results$first, results$last))
results <- results %>% 
  select(Player, ownership, total_pts)

#Add Results to Golfers
golfers <- golfers %>%
  left_join(results, by = c("Name" = "Player"))

golfers$date <- date
golfers$tournament <- tournament
write.csv(golfers, file = "golfers_results.csv")
