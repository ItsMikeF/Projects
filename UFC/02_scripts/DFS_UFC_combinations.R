library(tidyverse)
library(ggrepel)
library(lubridate)
library(stats)

user <- unlist(strsplit(getwd(), "/"))
user <- user[3]

setwd(paste("C://Users//", user, "//Documents//Github//DFS_Data//Data_UFC//2022-03-12", sep = ""))

###UFC Imports###

ufc_salaries <- read.csv("DKsalaries.csv")
ufc_odds <- read.csv("mma-odds-rotowire.csv", header = T)

###Rotowire Adjustments###

ufc_odds <- replace(ufc_odds, ufc_odds == '', '')

###UFC Odds###

ufc_odds <- ufc_odds[-c(1),-c(4,6,8,10,12,14,16,18,20,22,24)]
names(ufc_odds)[c(1,2,8)] <- c("Fighter", "Date Time","Fight.Goes.Distance.No")

ufc_odds[,c(3:13)] <- sapply(ufc_odds[,c(3:13)], as.numeric)

convert_ML <- function(odds) {
  breakeven <- if_else(odds > 0, 100 / (100 + odds), abs(odds) / (abs(odds) + 100))
  return(round(breakeven, digits = 4))
}

ufc_odds[,c(3:13)] <- sapply(ufc_odds[,c(3:13)], convert_ML)

###UFC Salaries###

ufc_salaries <- ufc_salaries %>% 
  separate(Game.Info, into = c("Matchup"), sep = " ")

ufc_salaries <- ufc_salaries %>% 
  separate(Matchup, into = c("Fighter1", "Fighter2"), sep = "@")

ufc_salaries$TeamOpp <- if_else(ufc_salaries$Fighter1 == ufc_salaries$TeamAbbrev, ufc_salaries$Fighter2, ufc_salaries$Fighter1)

opponents <- ufc_salaries %>% 
  select(TeamAbbrev, Name)
names(opponents)[2] <- "Opponent"

ufc_salaries <- ufc_salaries %>% 
  left_join(opponents, by = c("TeamOpp" = "TeamAbbrev"))

ufc_salaries <- ufc_salaries %>% 
  select(Name, ID, Salary, Opponent)

###Fighters###

fighters <- ufc_odds %>% 
  left_join(ufc_salaries, by = c("Fighter" = "Name"))

###Create random lineup###

lineup <- tibble(sample(fighters$Fighter, 6, replace = F, prob = fighters$Win))
names(lineup)[1] <- "Fighter"

lineup <- lineup %>% 
  left_join(fighters, by = c("Fighter")) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(where(is.character), ~"")))