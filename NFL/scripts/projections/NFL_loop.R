library(tidyverse)
library(ggrepel)
library(lubridate)
library(utils)

#############
###QB Data###
#############

setwd("C://Users//Mike Francis//Documents//Projects//DFS_Data//Data_NFL//2021_QB_Data")

qb_weeks <- list()

for(i in 1:17) {
  qb_weeks[[i]] <- left_join(read.csv(paste("passing_summary (", i,").csv", sep = "")), 
                             read.csv(paste("passing_pressure (", i,").csv", sep = "")), 
                             by = c("player_id" = "player_id"))
  qb_weeks[[i]]$week <- i
}

write.csv(qb_weeks[[1]], file = "2021_qb_passing_summary.csv")

for(i in 2:length(qb_weeks)){
  write.table(tibble(qb_weeks[[i]]), file = "2021_qb_passing_summary.csv", sep = ",", col.names = !file.exists("2021_qb_passing_summary.csv"), append = T)
}

"2021_qb_passing_summary" <- read.csv("2021_qb_passing_summary.csv")

#############
###RB Data###
#############

setwd("C://Users//Mike Francis//Documents//Projects//DFS_Data//Data_NFL//2021_RB_Data")

rb_weeks <- list()

for(i in 1:17) {
  rb_weeks[[i]] <- read.csv(paste("rushing_summary (", i,").csv", sep = ""))
  rb_weeks[[i]]$week <- i
}

write.csv(rb_weeks[[1]], file = "2021_rb_rushing_summary.csv")

for(i in 2:length(rb_weeks)){
  write.table(tibble(rb_weeks[[i]]), file = "2021_rb_rushing_summary.csv", sep = ",", col.names = !file.exists("2021_rb_rushing_summary.csv"), append = T)
}

"2021_rb_rushing_summary" <- read.csv("2021_rb_rushing_summary.csv")

#############
###WR Data###
#############

setwd("C://Users//Mike Francis//Documents//Projects//DFS_Data//Data_NFL//2021_WR_Data")

wr_weeks <- list()

for(i in 1:17) {
  wr_weeks[[i]] <- left_join(read.csv(paste("receiving_summary (", i,").csv", sep = "")), 
                             read.csv(paste("receiving_scheme (", i,").csv", sep = "")), 
                             by = c("player_id" = "player_id"))
  wr_weeks[[i]]$week <- i
}

write.csv(wr_weeks[[1]], file = "2021_wr_receiving_summary.csv")

for(i in 2:length(wr_weeks)){
  write.table(tibble(wr_weeks[[i]]), file = "2021_wr_receiving_summary.csv", sep = ",", col.names = !file.exists("2021_wr_receiving_summary.csv"), append = T)
}

"2021_wr_receiving_summary" <- read.csv("2021_wr_receiving_summary.csv")
