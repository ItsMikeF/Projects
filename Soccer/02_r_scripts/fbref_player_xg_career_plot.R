#use to plot a players career

#load packages
suppressMessages({
  library(tidyverse) #metapackage
  library(gt) #Easily Create Presentation-Ready Display Tables
  library(ggsci) #Scientific Journal and Sci-Fi Themed Color Palettes for 'ggplot2'
  library(reshape2) #flexibly Reshape Data
  library(ggrepel) #Automatically Position Non-Overlapping Text Labels with 'ggplot2'
  library(webshot)
  library(DescTools) # Tools for Descriptive Statistics
  
  library(worldfootballR) #extract and clean world soccer data
  library(engsoccerdata) #English and European Soccer Results
  library(socceR) #Evaluating Sport Tournament Predictions
  library(ggsoccer) #Plot Soccer Event Data
  library(footBayes) #Fitting Bayesian and MLE Football Models
  library(itscalledsoccer) #American Soccer Analysis API Client
  library(FPLdata) #Read in Fantasy Premier League Data
  library(EUfootball) #Football Match Data of European Leagues
})

player_url <- "https://fbref.com/en/players/c0c7ff58/Fabian-Ruiz-Pena"

player <- fb_player_season_stats(player_url, stat_type = "standard", time_pause = 3)

player <- player %>% 
  mutate(npxG_plus_xA_per_min = xG_Per_Minutes + xA_Per_Minutes)

ggplot(data = player , aes(x=Age, y=npxG_plus_xA_per_min)) +
  geom_bar(stat = "identity", aes(x= Age, fill=Comp)) +
  geom_text_repel(aes(label = paste(Min_Time, "minutes"))) +
  labs(
    title = paste(player[1,1] ,"Career"),
    subtitle = "Team: Napoli / League: Serie A",
    caption = "Data from FBRef"
  ) +
  scale_x_continuous()

#group by
player_group <- player %>% 
  group_by(Age) %>% 
  summarise(matches = sum(MP, na.rm = T), 
            minutes = sum(Min_Time, na.rm = T),
            goals = sum(Gls, na.rm = T),
            npxG_plus_xA_per_min = sum(npxG_plus_xA_per_min, na.rm = T))
player_group

ggplot(data = player_group, aes(x=Age, y=npxG_plus_xA_per_min)) +
  geom_bar(stat = "identity", aes(fill=npxG_plus_xA_per_min)) +
  geom_text_repel(aes(label = paste(minutes, "minutes"))) +
  labs(
    title = paste(player[1,1] ,"Career"),
    subtitle = "Team: Napoli / League: Serie A",
    caption = "Data from FBRef"
  ) 
