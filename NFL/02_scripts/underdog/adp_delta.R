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

# 1.0 Player Rankings --------------------------------------------------------------

# Filter the teams colors logos
teams_colors_logos <- teams_colors_logos %>% 
  filter(!team_name %in% c("St. Louis Rams", "San Diego Chargers", "Oakland Raiders")) %>% 
  filter(!team_abbr %in% c("LA")) %>% 
  select(team_name, team_abbr, team_logo_espn)

# Load the opening rankings
rankings_udd_1 <- read.csv(glue("./projections_season/2023/rankings_apr30.csv")) %>% 
  mutate(name = paste(firstName, lastName),
         adp = as.numeric(adp)) %>% 
  select(name, adp, projectedPoints, positionRank, slotName, teamName)

# Load the most current rankings by reading the last file in the directory
rankings_udd_2 <- read.csv(paste0("./projections_season/2023/",list.files(path = "./projections_season/2023/")[length(list.files(path = "./projections_season/2023/"))])) %>% 
  mutate(name = paste(firstName, lastName), 
         adp = as.numeric(adp)) %>% 
  select(name, adp, projectedPoints, positionRank, slotName, teamName)

# extract the date from the file name
date <- str_extract(list.files(path = "./projections_season/2023/")[length(list.files(path = "./projections_season/2023/"))], 
                    "(?<=_)[a-z]+[0-9]+")

# Combine rankings in a single dataframe
rankings <- rankings_udd_1 %>% 
  left_join(rankings_udd_2 %>% select(name, adp), by=c("name")) %>% 
  mutate(delta = adp.x-adp.y, 
         percent_change = round(delta/adp.x,digits = 2)) %>% 
  select(name, slotName, adp.x, adp.y, delta, percent_change, projectedPoints, teamName) %>% 
  rename(.,date1=adp.y) %>% 
  rename(.,date2=adp.x) %>% 
  arrange(date1) %>% 
  mutate(teamName = case_when(
    teamName == "NY Giants" ~ "New York Giants", 
    teamName == "NY Jets" ~ "New York Jets", 
    T ~ teamName
  )) %>% 
  left_join(teams_colors_logos, by=c('teamName'='team_name')) %>% 
  rename(team = team_logo_espn)

# create a GT table with the rankings
rankings %>% 
  select(name, team, date1, date2, delta, percent_change, projectedPoints) %>% 
  rename(apr30 = date2) %>% 
  rename_with(~ date, "date1") %>% 
  drop_na() %>% 
  gt() %>% 
  gt_img_rows(columns = team, height = 50) %>% 
  data_color(columns = percent_change, colors = scales::col_numeric(
    palette = c("red", "green"), 
    domain = c(min(rankings$percent_change, na.rm = T), max(rankings$percent_change, na.rm = T))
  )) %>% 
  gtsave(filename = glue("./projections_season/2023/NFL_rankings_{date}.html"))

# Top 25 Risers
rankings %>% 
  select(name, team, date1, date2, delta, percent_change, projectedPoints) %>% 
  drop_na() %>% 
  arrange(-percent_change) %>% 
  slice_head(n=25) %>% 
  gt() %>% 
  gt_img_rows(columns = team, height = 50) %>% 
  data_color(columns = percent_change, colors = scales::col_numeric(
    palette = c("red", "green"), 
    domain = c(min(rankings$percent_change, na.rm = T), max(rankings$percent_change, na.rm = T))
  )) 

# Top 25 Fallers
rankings %>% 
  select(name, team, date1, date2, delta, percent_change, projectedPoints) %>% 
  drop_na() %>% 
  arrange(percent_change) %>% 
  slice_head(n=25) %>% 
  gt() %>% 
  gt_img_rows(columns = team, height = 50) %>% 
  data_color(columns = percent_change, colors = scales::col_numeric(
    palette = c("red", "green"), 
    domain = c(min(rankings$percent_change, na.rm = T), max(rankings$percent_change, na.rm = T))
  ))


# 2.0 Team Rankings -------------------------------------------------------

team <- rankings %>% 
  drop_na() %>% 
  filter(date1 < 216) %>% 
  group_by(teamName) %>% 
  summarise(adp_mean = round(mean(date1, na.rm = T),digits = 1),
            adp_delta = round(mean(delta, na.rm = T),digits = 1), 
            percent_change= round(mean(percent_change, na.rm = T),digits = 2)) %>% 
  arrange(-percent_change) %>% 
  left_join(teams_colors_logos %>% select(team_name, team_logo_espn),by=c('teamName'='team_name'))

team %>%
  select(teamName, team_logo_espn, adp_mean, adp_delta, percent_change) %>% 
  arrange(adp_mean) %>% 
  gt() %>% 
  tab_header(title = "2023 Best Ball - Mean Team ADP Movement", 
             subtitle = "Period: Jan07 to May13") %>% 
  gt_img_rows(columns=team_logo_espn, height = 50) %>% 
  tab_footnote(footnote = "Data from Underdog NFL Rankings, players ADP > 59 filtered out")

