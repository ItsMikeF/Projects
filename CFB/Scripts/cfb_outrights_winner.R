#lets scrape cfb outrights

#load packages
suppressMessages({
  library(tidyverse) #ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, forcats
  library(glue) #interpreted string literals
  library(rvest) #easily harvest (scrape) web pages
  library(httr) #tools for working with URLs and HTTP
  library(cfbfastR) #access cfb pbp data
  library(ggimage) #use image in ggplot2
  library(ggrepel) #auto position non-overlapping text labels with ggplot2
  library(ggtext) #improved text rendering support for ggplot2
})

#read the webpage
url <- "https://sportsbook.draftkings.com/leagues/football/ncaaf?category=team-futures&subcategory=winner"
webpage <- read_html(url)

#define the css selectors
css1 <- ".sportsbook-outcome-cell__label"
css2 <- ".default-color"

#write selectors to objects
teams <- html_elements(webpage, css1) %>% html_text()
odds <- html_elements(webpage, css2) %>% html_text() %>% as.numeric()

#create df of the teams and odds
cfb_odds <- as.data.frame(cbind(teams, odds)) %>% 
  mutate_at(c('odds'), as.numeric) %>% 
  mutate(implied = round(100/(100+odds), digits = 3))

#add team logos
cfb_team <- cfbd_team_info() %>% 
  select(school, color, alt_color,logo)

#adjust cfbfastr team names to dk names
cfb_team$school[59] <- "Miami FL"
cfb_team$school[68] <- "North Carolina State"

#join logos to odds df
cfb_odds_logos <- cfb_odds %>% 
  left_join(cfb_team, by=c("teams"="school"))

#plot 25 teams
cfb_odds_logos %>% 
  slice_head(n=25) %>% 
  ggplot(aes(x=reorder(teams,-implied), y=implied)) +
  geom_col(aes(color=teams, fill=teams)) +
  geom_text(aes(label=implied), position = position_dodge(width = 0.9)) +
  #scale_x_discrete(aes(labels=logo)) +
  #geom_text_repel(aes(label=implied)) +
  #geom_image(aes(image=logo)) +
  labs(
    title = "Top 25 2022/23 CFB Winner Odds",
    y = "Implied Odds",
    caption = "Odds from DraftKings Sportsbook"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(face="bold"),
        legend.position = "none", 
        axis.title.x = element_blank(), 
        axis.text.x = element_text(face="bold"))

#need to figure out how to get the team logos on the x axis
#plot top 10 teams
cfb_odds_logos %>% 
  slice_head(n=10) %>% 
  ggplot(aes(x=reorder(teams,-implied), y=implied)) +
  geom_col(aes(color=color, fill=alt_color)) +
  geom_text(aes(label=implied), position = position_dodge(width = 0.9)) +
  scale_x_discrete(name = NULL,
                   labels = labels) +
  geom_text_repel(aes(label=implied)) +
  geom_image(aes(image=logo)) +
  labs(
    title = "Top 25 2022/23 CFB Winner Odds",
    y = "Implied Odds",
    caption = "Odds from DraftKings Sportsbook"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(face="bold"),
        legend.position = "none", 
        axis.title.x = element_blank(), 
        axis.text.x = element_markdown())
