#gather nfl defense data

#load packages
suppressMessages({
  library(nflfastR) #nflfastr nflseedr nflplotr
  library(tidyverse) #ggplot2 dplyr tibble tidyr purrr forecats 
  library(ggrepel) #automatically position non-overlapping text labels
  library(glue) #interpreted literal strings
})


pbp <- load_pbp(2022)

pbp_qb <- pbp %>% 
  filter(pass==1) %>% 
  group_by(passer, passer_id, posteam) %>% 
  summarise(n=n()) 

pbp_qb <- rename(pbp_qb, "v1"=passer, "v2"=passer_id)

pbp_wr <- pbp %>% 
  filter(pass==1) %>% 
  group_by(receiver, receiver_id, posteam) %>% 
  summarise(n=n())
pbp_wr <- rename(pbp_wr, "v1"=receiver, "v2"=receiver_id)

pbp_rb <- pbp %>% 
  filter(rush==1) %>% 
  group_by(rusher, rusher_id, posteam) %>% 
  summarise(n=n())
pbp_rb <- rename(pbp_rb, "v1"=rusher, "v2"=rusher_id)

players <- rbind(pbp_qb, pbp_wr, pbp_rb) %>% unique()

r3 <- read.csv("gauntlet_r3_adv.csv")
