#gather nfl defense data

#load packages
suppressMessages({
  library(nflfastR) #nflfastr nflseedr nflplotr
  library(tidyverse) #ggplot2 dplyr tibble tidyr purrr forecats 
  library(ggrepel) #automatically position non-overlapping text labels
  library(glue) #interpreted literal strings
})


# create df of player names and ids-------------------------------------------------------------------------

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

#download player names
r3 <- read.csv("gauntlet_r3_adv.csv")%>%
  mutate(pbp_name = Name) %>% 
  separate(pbp_name, into = c("first_name", "last_name"), sep=" ") %>% 
  mutate(first = substr(first_name, 1, 1), 
         name = paste(first, last_name, sep = "."))
r3 <- rename(r3, 
       team_count="Remaining.Teams.with.this.Player", 
       own ="Percent.of.R3.Teams.with.Them")
r3 <- r3 %>% select(Name, own, name)

r3$own <- as.data.frame(gsub("%", "",r3$own))
colnames(r3)[2] <- c("own")

r3$own <- as.numeric(r3)
