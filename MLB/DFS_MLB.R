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

user <- unlist(strsplit(getwd(), "/"))
user <- user[3]

date <- c("2022-04-08")

setwd(paste0("C://Users//",user,"//Documents//GitHub//DFS_Data//Data_MLB//", date))

date <- format(if_else(format(Sys.time(), format = "%H:%M:%S") > 13, today()+1, today()), format = "%Y-%m-%d")

if(!exists(paste("C://Users//",user,"//Documents//GitHub//DFS_Data//Data_CBB//",date, sep = "")))
{
  dir.create(paste("C://Users//",user,"//Documents//GitHub//DFS_Data//Data_CBB//",date, sep = ""))
  setwd(paste("C://Users//",user,"//Downloads", sep = ""))
  file.move(c("summary22.csv", "cbb-odds-rotowire.csv"), paste("C://Users//",user,"//Documents//GitHub//DFS_Data//Data_CBB//",date, sep = ""), 
            overwrite = T)
}

setwd(paste("C://Users//",user,"//Documents//GitHub//DFS_Data//Data_CBB//",date, sep = ""))

### Import CSVs ###

mlb_salaries <- read.csv("DKSalaries.csv")