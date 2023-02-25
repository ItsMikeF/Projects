#playoff best ball analysis
#source for the gt table
#https://jthomasmock.github.io/gtExtras/reference/gt_img_rows.html

#load packages
suppressMessages({
  library(tidyverse) #metapackage
  library(nflverse) #functions to efficiently access NFL pbp data
  library(fs) #Cross-Platform File System Operations Based on 'libuv'
  library(gt)
  library(gtExtras)
  library(ggrepel) #Automatically Position Non-Overlapping Text Labels with 'ggplot2'
  library(glue)
})

# 1.0 rankings --------------------------------------------------------------

date1 = "dec28"
date2 = "jan14"

#load the rankings
rankings_udd_1 <- read.csv(glue("./projections_playoffs/underdog_playoff_projections_{date1}.csv")) %>% 
  mutate(name = paste(firstName, lastName),
         adp = as.numeric(adp)) %>% 
  select(name, adp, projectedPoints, positionRank, slotName, teamName)

#load the rankings
rankings_udd_2 <- read.csv(glue("./projections_playoffs/underdog_playoff_projections_{date2}.csv")) %>% 
  mutate(name = paste(firstName, lastName), 
         adp = as.numeric(adp)) %>% 
  select(name, adp, projectedPoints, positionRank, slotName, teamName)

#combine rankings in 1 dataframe
rankings <- rankings_udd_1 %>% 
  left_join(rankings_udd_2 %>% select(name, adp), by=c("name")) %>% 
  mutate(delta = adp.x-adp.y, 
         percent_change = round(delta/adp.x,digits = 2)) %>% 
  select(name, adp.x, adp.y, delta, percent_change, projectedPoints, teamName) %>% 
  rename(.,date1=adp.x) %>% 
  rename(.,date2=adp.y) %>% 
  arrange(-percent_change) #%>% gt() %>% tab_header(title = "Playoff Best Ball Risers", subtitle = "Dec28 to Jan 07") 

team <- rankings %>% 
  drop_na() %>% 
  filter(date1 < 59) %>% 
  group_by(teamName) %>% 
  summarise(adp_mean = round(mean(date1, na.rm = T),digits = 1),
            adp_delta = round(mean(delta, na.rm = T),digits = 1), 
            percent_change= round(mean(percent_change, na.rm = T),digits = 2)) %>% 
  arrange(-percent_change) %>% 
  left_join(teams_colors_logos %>% select(team_name, team_logo_espn),by=c('teamName'='team_name'))

team %>%
  select(teamName, team_logo_espn, adp_mean, adp_delta, percent_change) %>% 
  gt() %>% 
  tab_header(title = "Playoff Best Ball - Mean Team ADP Movement", 
             subtitle = "Period: Jan07 to Jan11") %>% 
  gt_img_rows(columns=team_logo_espn) %>% 
  tab_footnote(footnote = "Data from Underdog NFL Rankings, players ADP > 59 filtered out")
