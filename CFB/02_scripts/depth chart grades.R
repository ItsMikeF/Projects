# add grades to depth charts

# load packages
library(tidyverse)
library(glue)

# depth charts
load("./01_data/cfb_depth_charts.RData")

# define teams
away_team <- "illinois"
home_team <- "kansas"

# display starters
depth_charts[[away_team]]
depth_charts[[home_team]]
