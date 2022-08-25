#lets take a look at pff season long sos metrics

#load packages
suppressMessages({
  library(tidyverse) #metapackage
  library(nflverse) #functions to efficiently access NFL pbp data
  library(fs) #Cross-Platform File System Operations Based on 'libuv'
  library(ggrepel) #Automatically Position Non-Overlapping Text Labels with 'ggplot2'
})

#load the projections
pff_projections <- read.csv("./season_projections/projections.csv")

#filter for what we want
projections <- pff_projections %>% 
  select(c(1:8))

#find unique positions
unique(projections$position)
length(unique(projections$position))

#filter by the 6 positions
qbs <- projections %>% filter(position == "qb") %>% 
  mutate(residuals = round(residuals(loess(fantasyPoints ~ fantasyPointsRank)), digits = 2))

rbs <- projections %>% filter(position == "rb") %>% 
  mutate(residuals = round(residuals(loess(fantasyPoints ~ fantasyPointsRank)), digits = 2))

wrs <- projections %>% filter(position == "wr") %>% 
  mutate(residuals = round(residuals(loess(fantasyPoints ~ fantasyPointsRank)), digits = 2))

tes <- projections %>% filter(position == "te") %>% 
  mutate(residuals = round(residuals(loess(fantasyPoints ~ fantasyPointsRank)), digits = 2))

ks <- projections %>% filter(position == "k") %>% 
  mutate(residuals = round(residuals(loess(fantasyPoints ~ fantasyPointsRank)), digits = 2))

dst <- projections %>% filter(position == "dst") %>% 
  mutate(residuals = round(residuals(loess(fantasyPoints ~ fantasyPointsRank)), digits = 2))

#plot it
ggplot(qbs, aes(x=fantasyPointsRank, y=fantasyPoints), size=residuals) +
  geom_point() + 
  geom_text_repel(aes(label=playerName)) +
  geom_smooth(method = "loess", se=F) + 
  theme_dark() +
  labs(
    title = "2022/2023 Fantasy Draft Value", 
    caption = "Twitter: Its_MikeF \n Data from Pro Football Focus"
  )


#facet warp plot
ggplot(projections, aes(x=fantasyPointsRank, y=fantasyPoints), size=residuals) +
  geom_point() + 
  geom_text_repel(aes(label=playerName)) +
  facet_wrap(vars(position)) +
  geom_smooth(method = "loess", se=F) + 
  theme_dark() +
  labs(
    title = "2022/2023 Fantasy Draft Value", 
    caption = "Twitter: Its_MikeF \n Data from Pro Football Focus"
  )
