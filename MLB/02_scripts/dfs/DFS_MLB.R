#Load packages
library(tidyverse, warn.conflicts = F) #metapackage
library(ggrepel, warn.conflicts = F) #automatically position non-overlapping text labels
library(lubridate, warn.conflicts = F) #make dealing with dates easier
library(utils, warn.conflicts = F) #various programming utilities
library(stats, warn.conflicts = F) #R statistical functions
library(binr, warn.conflicts = F) #cut numerical values into evenly distributed groups

#Inputs
date <- c("2022-04-08")

#Import CSVs
mlb_salaries <- read.csv(paste0("./",date,"/DKSalaries.csv"))