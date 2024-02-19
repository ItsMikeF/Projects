#mlb season 
#source for the gt table
#https://jthomasmock.github.io/gtExtras/reference/gt_img_rows.html

#load packages
suppressMessages({
  library(tidyverse) #metapackage
  library(gt)
  library(gtExtras)
  library(glue)
  library(openxlsx)
})

# 1.0 rankings --------------------------------------------------------------

date1 = "mar13"
date2 = "apr16"

#load the rankings
rankings_1 <- read.csv(glue("./01_data/projections/playoffs/rankings_{date1}.csv")) %>% 
  mutate(name = paste(firstName, lastName),
         adp = as.numeric(adp)) %>% 
  select(name, slotName, adp, projectedPoints, positionRank, slotName, teamName)

rankings_2 <- read.csv(glue("./01_data/projections/playoffs/rankings_{date2}.csv")) %>% 
  mutate(name = paste(firstName, lastName),
         adp = as.numeric(adp)) %>% 
  select(name, slotName, adp, projectedPoints, positionRank, slotName, teamName)

#combine rankings in 1 dataframe
rankings <- rankings_2 %>% 
  left_join(rankings_1 %>% select(name, adp, projectedPoints), by=c("name")) %>% 
  mutate(delta = adp.y-adp.x, 
         percent_change = round(delta/adp.x,digits = 2), 
         projectedPoints_delta = projectedPoints.x - projectedPoints.y) %>% 
  rename(projectedPoints = projectedPoints.x, 
         projectedPoints_mar13 = projectedPoints.y) %>% 
  select(name, slotName, adp.x, adp.y, delta, percent_change, projectedPoints, projectedPoints_mar13, projectedPoints_delta, positionRank, teamName) %>% 
  rename(.,"mar13"=adp.y) %>% 
  rename(.,"apr16"=adp.x) %>% 
  arrange(apr16) %>% 
  #left_join(teams_colors_logos %>% select(team_name, team_abbr, team_logo_espn),by=c('teamName'='team_name')) %>% 
  #rename(., team = team_logo_espn) %>% 
  distinct()

# 2.0 team data -----------------------------------------------------------

rankings_2 %>% 
  drop_na(adp) %>% 
  filter(adp < 230) %>% 
  group_by(teamName) %>% 
  summarise(adp_mean = round(mean(adp, na.rm = T),digits = 1), 
            proj = sum(projectedPoints))

rankings %>% 
  drop_na() %>% 
  filter(apr16 < 239) %>% 
  group_by(teamName) %>% 
  summarise(adp_mean = round(mean(apr16, na.rm = T),digits = 1),
            adp_delta = round(mean(delta, na.rm = T),digits = 1), 
            percent_change= round(mean(percent_change, na.rm = T),digits = 2)) %>% 
  arrange(adp_mean) %>% 
  gt() %>% 
  data_color(columns = percent_change, colors = scales::col_numeric(
    palette = c("red", "green"),
    domain = c(-.45,2))) %>% 
  gt_theme_dark()

# write rankings xlsx file
write.xlsx(rankings, file = "./03_outputs/2023_zamboni_rankings.xlsx")

# 3.0 Gtsave the draft guide ----------------------------------------

rankings %>% 
  arrange(apr16) %>% 
  select(teamName, name, positionRank,apr16, mar13, delta, percent_change, projectedPoints) %>% 
  distinct() %>% 
  #slice_head(n=10) %>% 
  gt() %>% #gt_img_rows(columns = team, height = 50) %>% gt_img_rows(columns = espn_headshot, height = 50) %>% 
  tab_header(title = "2023 Zamboni", subtitle = "Period: mar13 - apr16") %>% 
  tab_footnote(footnote = "Data from Underdog Fantasy") %>% 
  data_color(projectedPoints, colors = scales::col_numeric(
    palette = c("red", "green"),
    domain = c(0, 150))) %>% 
  data_color(apr16, colors = scales::col_numeric(
    palette = c("green", "red"),
    domain = c(1,240)
  )) %>% 
  data_color(percent_change, colors = scales::col_numeric(
    palette = c("red", "green"), 
    domain = c(-4,4)
  )) %>% 
  gt_theme_dark() %>% 
  gtsave(filename = "2023 UD Zamboni Board.html")
