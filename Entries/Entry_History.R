library(tidyverse)
library(ggrepel)
library(lubridate)
library(utils)
library(xlsx)
library(scales)

#Set Working Directory
setwd(paste0("C://Users//",unlist(strsplit(getwd(), "/"))[3],"//Documents//GitHub//DFS//Entries"))

#Load file
entry_history <- read.csv("draftkings-contest-entry-history.csv")

#Convert date time
entry_history$Contest_Date_EST <- as.Date(entry_history$Contest_Date_EST)
entry_history$Contest_Date_EST <- year(entry_history$Contest_Date_EST)
entry_history$Contest_Date_EST <- ymd_hms(entry_history$Contest_Date_EST)

entry_history$Contest_Date_EST <- as.POSIXct(entry_history$Contest_Date_EST, tz=Sys.timezone())

#entry_history$Contest_Date_EST <- as.Date(entry_history$Contest_Date_EST, tryFormats = "character", format = "%y/%m/%/d")
#entry_history$Contest_Date_EST <- as.Date(entry_history$Contest_Date_EST)

#Parse Numbers
entry_history$Winnings_Non_Ticket <- parse_number(entry_history$Winnings_Non_Ticket)
entry_history$Entry_Fee <- parse_number(entry_history$Entry_Fee)
entry_history$Prize_Pool <- parse_number(entry_history$Prize_Pool)

entry_history_table <- entry_history %>% 
  filter(Contest_Entries > 100) %>% 
  group_by(Sport, Contest_Date_EST) %>% 
  summarize(
    Winnings = sum(Winnings_Non_Ticket), 
    Fees = sum(Entry_Fee), 
    ROI = percent(round(sum(Winnings_Non_Ticket) / sum(Entry_Fee), digits =2))) %>% 
  arrange(-Fees) %>% 
  view(title = "Sport Date History")

entry_history_table <- entry_history %>% 
  filter(Contest_Entries > 100) %>% 
  group_by(Sport) %>% 
  summarize(
    Winnings = sum(Winnings_Non_Ticket), 
    Fees = sum(Entry_Fee), 
    ROI = percent(round(sum(Winnings_Non_Ticket) / sum(Entry_Fee), digits =2))) %>% 
  arrange(-Fees) %>% 
  view(title = "Sport Entry History")
