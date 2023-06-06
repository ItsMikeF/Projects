# best ball ranking analysis

#load packages
suppressMessages({
  library(dplyr)
  library(tidyr)
  library(nflfastR)
  library(nflreadr)
  library(nflplotR)
  library(fs) #Cross-Platform File System Operations Based on 'libuv'
  library(gt)
  library(gtExtras)
  library(ggrepel) #Automatically Position Non-Overlapping Text Labels with 'ggplot2'
  library(glue)
  library(lubridate)
})

# 1.0 Define team logos --------------------------------------------------------------

# Filter the teams colors logos
teams_colors_logos <- teams_colors_logos %>% 
  filter(!team_abbr %in% c("LA","OAK","SD","STL")) %>% 
  select(team_name, team_abbr, team_logo_espn)

#create vector of modern team abbreviations
teams <- pull(teams_colors_logos %>% select(team_abbr)) 
teams <- teams[! teams %in% c("LA","OAK","SD","STL")]

# load rosters
rosters <- load_rosters(2022)


# 2.0 Load rankings -------------------------------------------------------

first_date <- "may05"

# Load the opening rankings
rankings_udd_1 <- read.csv(glue("./01_data/projections/season/2023/rankings_{first_date}.csv")) %>% 
  mutate(name = paste(firstName, lastName),
         adp = as.numeric(adp)) %>% 
  select(name, adp, projectedPoints, positionRank, slotName, teamName)

# Load the most current rankings by reading the last file in the directory
rankings_udd_2 <- read.csv(paste0("./01_data/projections/season/2023/", 
                                  list.files(path = "./01_data/projections/season/2023/")
                                  [max(which(grepl("jun",list.files(path = "./01_data/projections/season/2023/"))))]
                                  )
                           ) %>% 
  mutate(name = paste(firstName, lastName), 
         adp = as.numeric(adp)) %>% 
  select(name, adp, projectedPoints, positionRank, slotName, teamName)

# extract the date from the file name
second_date <- str_extract(list.files(path = "./01_data/projections/season/2023/")
                    [max(which(grepl("jun",list.files(path = "./01_data/projections/season/2023/"))))], 
                    "(?<=_)[a-z]+[0-9]+")


# 3.0 Merge rankings to one dataframe -------------------------------------


# Combine rankings in a single dataframe
rankings <- rankings_udd_1 %>% 
  left_join(rankings_udd_2 %>% select(name, adp), by=c("name")) %>% 
  mutate(delta = adp.x-adp.y, 
         percent_change = round(delta/adp.x,digits = 2)) %>% 
  select(name, slotName, adp.y, adp.x, delta, percent_change, projectedPoints, teamName) %>% 
  rename_with(~ first_date, adp.x) %>% 
  rename_with(~ second_date, adp.y) %>% 
  arrange(first_date) %>% 
  mutate(teamName = case_when(
    teamName == "NY Giants" ~ "New York Giants", 
    teamName == "NY Jets" ~ "New York Jets", 
    T ~ teamName
  )) %>% 
  left_join(teams_colors_logos, by=c('teamName'='team_name')) 

# create a GT table with the rankings
rankings %>% 
  relocate(team, .after = name) %>% 
  select(1:8, 10, 12) %>% 
  drop_na() %>% 
  gt() %>% 
  gt_img_rows(columns = team, height = 50) %>% 
  gt_img_rows(columns = id)
  data_color(columns = percent_change, colors = scales::col_numeric(
    palette = c("red", "green"), 
    domain = c(min(rankings$percent_change, na.rm = T), max(rankings$percent_change, na.rm = T))
  )) %>% 
  gt_theme_dark() %>% 
  tab_header(
    title = glue("{toupper(second_date)} NFL Best Ball Rankings")
  ) 


# 4.0 Risers and Fallers --------------------------------------------------

# Top 25 Risers
rankings %>% 
  relocate(team, .after = name) %>% 
  select(1:8) %>% 
  drop_na() %>% 
  arrange(-percent_change) %>% 
  slice_head(n=25) %>% 
  gt() %>% 
  tab_header(title = "2023 NFL Top 25 Risers") %>% 
  gt_img_rows(columns = team, height = 50) %>% 
  data_color(columns = percent_change, colors = scales::col_numeric(
    palette = c("red", "green"), 
    domain = c(min(rankings$percent_change, na.rm = T), max(rankings$percent_change, na.rm = T))
  )) %>% 
  gt_theme_dark()

# top 25 late round risers
rankings %>% 
  filter(.[[3]] > 180) %>% 
  relocate(team, .after = name) %>% 
  select(1:8) %>% 
  drop_na() %>% 
  arrange(-percent_change) %>% 
  slice_head(n=25) %>% 
  gt() %>% 
  tab_header(title = "2023 NFL Late Round Risers") %>% 
  gt_img_rows(columns = team, height = 50) %>% 
  data_color(columns = percent_change, colors = scales::col_numeric(
    palette = c("red", "green"), 
    domain = c(min(rankings$percent_change, na.rm = T), max(rankings$percent_change, na.rm = T))
  )) %>% 
  gt_theme_dark()


# Top 25 Fallers
rankings %>% 
  relocate(team, .after = name) %>% 
  select(1:8) %>% 
  drop_na() %>% 
  arrange(percent_change) %>% 
  slice_head(n=25) %>% 
  gt() %>% 
  tab_header(title = "2023 NFL Top 25 Fallers") %>% 
  gt_img_rows(columns = team, height = 50) %>% 
  data_color(columns = percent_change, colors = scales::col_numeric(
    palette = c("red", "green"), 
    domain = c(min(rankings$percent_change, na.rm = T), max(rankings$percent_change, na.rm = T))
  )) %>% 
  gt_theme_dark()


# 5.0 Team Rankings -------------------------------------------------------

team <- rankings %>% 
  drop_na() %>% 
  filter(.[[3]] < 216) %>% 
  group_by(teamName) %>% 
  summarise(adp_mean = round(mean(jun04, na.rm = T),digits = 1),
            adp_delta = round(mean(delta, na.rm = T),digits = 1), 
            percent_change= round(mean(percent_change, na.rm = T),digits = 2)) %>% 
  arrange(-percent_change) %>% 
  left_join(teams_colors_logos %>% select(team_name, team_logo_espn),by=c('teamName'='team_name'))

team %>%
  select(teamName, team_logo_espn, adp_mean, adp_delta, percent_change) %>% 
  arrange(adp_mean) %>% 
  gt() %>% 
  tab_header(title = "2023 Best Ball - Mean Team ADP Movement", 
             subtitle = glue("Period: {first_date} to {second_date}")) %>% 
  gt_img_rows(columns=team_logo_espn, height = 50) %>% 
  gt_theme_dark() %>% 
  tab_footnote(footnote = "Data from Underdog NFL Rankings, players ADP > 215 filtered out")

