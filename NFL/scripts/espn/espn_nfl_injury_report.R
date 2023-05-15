#lets scrape the nfl injury report from espn
#run this script 1st

#load packages
suppressMessages({
  library(tidyverse) #ggplot2 dplyr tidyr readr stringr forcats purrr tibble
  library(nflverse) #nflfastr nflseedr nfl4th nflreadr nflplotr
  library(rvest) #easily harvest (scrape) web pages
  library(janitor) #simplea little tools for examining and cleaning dirty data
  library(gt) #Easily Create Presentation-Ready Display Tables
})

# 1.0 scrape espn injury report ------------------------------------------------

#define url
url <- "https://www.espn.com/nfl/injuries"

#scrape player injuries tables from espn
injuries <- html_table(html_elements(read_html(url),".Table__league-injuries"))

#get team names
teams <- html_text(html_elements(read_html(url),".ml2"))
teams <- as.data.frame(teams[-1], col.names="team_name") %>% 
  rename("team_name"='teams[-1]')


# 2.0 create single dataframe -------------------------------------------------

#join team abbreviations to scraped team name dataframe
teams <- teams %>% 
  left_join(teams_colors_logos %>% select(team_abbr, team_name), 
            by=("team_name"))

#remove LA and add WSH 
teams <- teams[-c(19),]
teams[32,2] <- "WSH"

#set names of the list elements as the teams
injuries <- setNames(injuries, teams$team_abbr)

#add team names to each dataframe
for (i in 1:32) {
  injuries[[i]]$team <- teams$team_abbr[i]
}

#combine all dataframes to one dataframe
injuries <- as.data.frame(do.call(rbind, injuries)) %>% 
  rename_with(tolower) %>% 
  select(name, pos, team, status, comment)


# 3.0 gt tables ---------------------------------------------------------------

#create gt tables for each position, status, and team
injuries %>% 
  count(pos) %>% 
  arrange(-n) %>% 
  gt() %>% 
  tab_header(title = "NFL Injuries by Position") %>% 
  tab_source_note(source_note = "https://www.espn.com/nfl/injuries")

injuries %>% 
  count(status) %>% 
  arrange(-n) %>% 
  gt() %>% 
  tab_header(title = "NFL Injuries by Status") %>% 
  tab_source_note(source_note = "https://www.espn.com/nfl/injuries")

injuries %>% 
  count(team) %>% 
  arrange(-n) %>% 
  gt() %>% 
  tab_header(title = "NFL Injuries by Team") %>% 
  tab_source_note(source_note = "https://www.espn.com/nfl/injuries")
