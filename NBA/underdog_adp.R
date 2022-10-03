#lets look at player projections

#load packages
suppressMessages({
  library(tidyverse) #metapackage
  library(ggrepel) #Automatically Position Non-Overlapping Text Labels with 'ggplot2'
  library(ggimage) #use image in ggplot2
  library(ggtext) #improve text rendering support for ggplot2
})

#load the first projections
projections_udd <- read.csv("./season/2022/projections_underdog_0924.csv") %>% 
  mutate(name = paste(firstName, lastName)) %>% 
  select(name, adp, projectedPoints, positionRank, slotName, teamName)

#load the latest projections
projections_udd_1 <- read.csv("./season/2022/projections_underdog_1001.csv") %>% 
  mutate(name = paste(firstName, lastName)) %>% 
  select(name, adp, projectedPoints, positionRank, slotName, teamName)

#check adp movement
projections_udd_delta <- projections_udd %>% 
  left_join(projections_udd_1 %>% select(name, adp), by=c("name"))

projections_udd_delta <- projections_udd_delta %>% 
  select(name, adp.x, adp.y, slotName, teamName, projectedPoints) %>% 
  rename( adp.x = first) %>% 
  mutate(adp.x = as.numeric(adp.x), 
         adp.y = as.numeric(adp.y)) 

projections_udd_delta <- projections_udd_delta %>% 
  mutate(adp_delta = adp.y-adp.x)

adp_movement <- projections_udd_delta %>% 
  group_by(teamName) %>% 
  summarize(
    adp_movement = sum(adp_delta, na.rm = T)
  ) %>% 
  filter(teamName != "")

ggplot(adp_movement, aes(x=teamName, y=adp_movement)) +
  geom_point() +
  scale_x_discrete(name=NULL, labels = adp_movement) +
  theme(
    axis.text.x = element_markdown(color = "black", size = 11)
  )
