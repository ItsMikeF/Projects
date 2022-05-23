#Load packages
library(tidyverse, warn.conflicts = F)
library(ggrepel)
library(lubridate, warn.conflicts = F)
library(utils)
library(filesstrings, warn.conflicts = F)
library(xtable)
library(tictoc)
library(lpSolve)
library(stats)
library(XML)
library(binr)
library(httr)

#setwd
setwd("C:/Users/mikef/Documents/GitHub/DFS_Data/Data_Golf/2022-05-05 Wells Fargo")

#import csv
standings <- read.csv(list.files(pattern = "contest-standings-"))
golfers <- read.csv("golfers.csv")

#left join
golfers <- golfers %>% 
  left_join(standings, by = c("Name" = "Player"))

fpts <- golfers %>% 
  select(Name, FPTS.x, FPTS.y)

names(fpts)[2:3] <- c("FPTS_pred", "FPTS_actual")

mean((fpts$FPTS_actual - fpts$FPTS_pred)^2) #mse
caret::MAE(fpts$FPTS_actual, fpts$FPTS_pred) #mae
caret::RMSE(fpts$FPTS_actual, fpts$FPTS_pred) #rmse