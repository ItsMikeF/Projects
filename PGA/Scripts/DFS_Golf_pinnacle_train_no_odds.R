#load packages
suppressMessages({
  library(tidyverse, warn.conflicts = F) #metapackage
  library(stringr) #simple consistent wrappers for common string operations
})

#List tournaments in golfer results
tournaments <- {c("2022-04-10 The Masters", 
                 "2022-04-17 RBC Heritage", 
                 "2022-05-01 Mexico Open",
                 "2022-05-08 Wells Fargo", 
                 "2022-05-15 AT&T Byron Nelson", 
                 "2022-05-22 PGA Championship", 
                 "2022-05-29 Charles Schwab", 
                 "2022-06-05 Memorial Tournament", 
                 "2022-06-12 RBC Canadian Open", 
                 "2022-06-19 US Open", 
                 "2022-06-26 Travelers Championship", 
                 "2022-07-03 John Deere Classic")}

#find latest training data folder
folder <- list.dirs()[length(list.dirs())]
file_list <- list.files(path = folder, pattern = "*.csv")

#automated inputs
date <- str_sub(folder, nchar("./Training_data/")+1, nchar("./Training_data/")+10)
tournament <- str_sub(folder, nchar("./Training_data/")+12, nchar(folder))

#import salaries
golf_salaries <- read.csv(paste0(folder, "/DKSalaries.csv")) %>% 
  select(Name, ID, Salary, AvgPointsPerGame)

#import rotogrinders
rg <- read.csv(paste0(folder, "/", list.files(path = folder, pattern = "projections_draftkings_golf"))) %>% 
  select(name, fpts, proj_own, ceil, floor)

#import data golf model predictions
cam <- read.csv(paste0(folder, "/", list.files(path = folder, pattern = "-model"))) %>%
  select(player_name, make_cut, first_round_lead, top_30, top_20, top_10, top_5, top_3, win, observed_finish) %>%
  separate(player_name, into = c("last", "first"), sep = ",") %>% 
  unite("Golfer", first:last) %>% 
  mutate(Golfer = trimws(Golfer), 
         observed_finish = gsub('T', '', observed_finish), 
         observed_finish = gsub('CU', '100', observed_finish), 
         observed_finish = gsub('WD', '', observed_finish)) %>% 
  select(Golfer, make_cut, first_round_lead, top_30, top_20, top_10, top_5, top_3, win, observed_finish) %>% 
  mutate(across(2:9, round, 4),
         observed_finish = as.numeric(observed_finish))

#import dfs points, ownership, and salaries
results <- read.csv(paste0(folder, "/", list.files(path = folder, pattern = "draftkings_pga_"))) %>%
  select(player_name, ownership, total_pts) %>%
  separate(player_name, into = c("last", "first"), sep = ",") %>% 
  unite("Player", first:last) %>% 
  mutate(Player = trimws(Player)) %>% 
  select(Player, ownership, total_pts)

#Create golfer tibble
golfers <- golf_salaries

golfers <- golf_salaries %>% 
  left_join(rg, by=c("Name" = "name")) %>% 
  left_join(cam, by=c("Name" = "Golfer")) %>% 
  left_join(results, by = c("Name" = "Player")) %>% 
  mutate(odds_per_dollar = round(golfers$win / golfers$Salary * 10^6, digits = 2), 
         residuals = round(residuals(loess(odds_per_dollar ~ Salary, golfers)), digits = 2))

golfers$odds_per_dollar <- round(golfers$odds_close / golfers$Salary * 10^6, digits = 2)

golfers$residuals <- round(residuals(loess(odds_per_dollar ~ Salary, golfers)), digits = 2)

### Ownership Change formula
own_multiplier <- 100/100

golfers$own_change <- round(
  (own_multiplier * 0.15 * golfers$residuals) +
    (own_multiplier * 2) +
    (2 * (1000 * golfers$odds_delta)) +
    (own_multiplier * 2 * if_else(golfers$odds_delta_per > 1, 1, golfers$odds_delta_per)^2), digits = 2)

golfers$adj_own <- case_when(golfers$proj_own + golfers$own_change <= 0 ~ 0,
                             golfers$proj_own + golfers$own_change < 40 ~ round((golfers$proj_own + golfers$own_change)/own_multiplier)*own_multiplier, 
                             golfers$proj_own + golfers$own_change >= 40 ~ 40)

golfers$date <- date
golfers$tournament <- tournament
write.csv(golfers, file = paste0(folder,"/golfers_results_no_odds.csv"))
