#lets look at the fantasy pros data

#libraries
library(tidyverse)

#load data
fp <- read.csv("./projections_season/FantasyPros_2023_Projections_H.csv")

fp %>% arrange(-HR) %>% slice_head(n=10)

hist(fp$HR)

fp %>% filter(HR>0)

numeric_fp <- fp[,unlist(lapply(fp, is.numeric))]

