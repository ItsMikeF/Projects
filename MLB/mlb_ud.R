#mlb season 
#source for the gt table
#https://jthomasmock.github.io/gtExtras/reference/gt_img_rows.html

#load packages
suppressMessages({
  library(tidyverse) #metapackage
  library(fs) #Cross-Platform File System Operations Based on 'libuv'
  library(gt)
  library(gtExtras)
  library(ggrepel) #Automatically Position Non-Overlapping Text Labels with 'ggplot2'
  library(glue)
  library(mlbplotR)
  library(baseballr) 
})


# 0.0 load teams colors ---------------------------------------------------

teams_colors_logos <- mlbplotR::load_mlb_teams() %>% 
  filter(!team_abbr %in% c("AL", "NL", "MLB")) %>% 
  mutate(
    a = rep(1:6, 5), 
    b = sort(rep(1:5, 6), decreasing=T), 
    alpha = ifelse(grepl("A", team_abbr),1,0.75),
    color = ifelse(grepl("E", team_abbr), "b/w", NA)
  )


# 1.0 rankings --------------------------------------------------------------

date1 = "jan23"
date2 = "feb25"

#load the rankings
rankings_1 <- read.csv(glue("./projections_season/rankings_{date1}.csv")) %>% 
  mutate(name = paste(firstName, lastName),
         adp = as.numeric(adp)) %>% 
  select(name, slotName, adp, projectedPoints, positionRank, slotName, teamName)

rankings_2 <- read.csv(glue("./projections_season/rankings_{date2}.csv")) %>% 
  mutate(name = paste(firstName, lastName),
         adp = as.numeric(adp)) %>% 
  select(name, slotName, adp, projectedPoints, positionRank, slotName, teamName)

#combine rankings in 1 dataframe
rankings <- rankings_1 %>% 
  left_join(rankings_2 %>% select(name, adp), by=c("name")) %>% 
  mutate(delta = adp.x-adp.y, 
         percent_change = round(delta/adp.x,digits = 2)) %>% 
  select(name, slotName, adp.x, adp.y, delta, percent_change, projectedPoints, teamName) %>% 
  rename(.,"jan23"=adp.x) %>% 
  rename(.,"feb25"=adp.y) %>% 
  arrange(-percent_change) #%>% gt() %>% tab_header(title = "Playoff Best Ball Risers", subtitle = "Dec28 to Jan 07") 


# 2.0 team data -----------------------------------------------------------

team_1 <- rankings_2 %>% 
  drop_na(adp) %>% 
  filter(adp < 230) %>% 
  group_by(teamName) %>% 
  summarise(adp_mean = round(mean(adp, na.rm = T),digits = 1), 
            proj = sum(projectedPoints))

team <- rankings %>% 
  drop_na() %>% 
  filter(feb25 < 239) %>% 
  group_by(teamName) %>% 
  summarise(adp_mean = round(mean(feb25, na.rm = T),digits = 1),
            adp_delta = round(mean(delta, na.rm = T),digits = 1), 
            percent_change= round(mean(percent_change, na.rm = T),digits = 2)) %>% 
  arrange(-percent_change) %>% 
  left_join(teams_colors_logos %>% select(team_name, team_logo_espn),by=c('teamName'='team_name'))

team %>%
  select(teamName, team_logo_espn, adp_mean, adp_delta, percent_change) %>% 
  gt() %>% 
  tab_header(title = "Playoff Best Ball - Mean Team ADP Movement", 
             subtitle = "Period: Jan23 to feb25") %>% 
  gt_img_rows(columns=team_logo_espn) %>% 
  tab_footnote(footnote = "Data from Underdog MLB Rankings, players ADP > 239 filtered out")


# 3.0 Pitchers ------------------------------------------------------------


sp <- rankings %>% filter(slotName == "P") %>%  arrange(feb25) %>%  drop_na() %>% left_join(teams_colors_logos %>% select(team_name, team_logo_espn), by=c('teamName'='team_name'))

sp %>% slice_head(n=10) %>% gt() %>% gt_img_rows(columns = team_logo_espn)

df <- baseballr::fg_pitcher_leaders(x=2022, y=2022, q = 100, pitcher_type = "sta")

filtered_df <- df %>% 
  mutate(Team = clean_team_abbrs(Team))

filtered_df %>% 
  mutate(playerid = as.double(playerid)) %>% 
  left_join(mlbplotR::load_headshots(), by=c("playerid"="fangraphs_id")) %>% 
  slice_min(ERA, n=12) %>% 
  ggplot(aes(x=ERA, y=FIP)) +
  geom_mlb_headshots(aes(player_id=savant_id), height=0.15)

test <- load_headshots() 

sp_test <- sp %>% 
  dplyr::left_join(df %>% select(Name, playerid), by=c('name'='Name')) %>% 
  dplyr::mutate(playerid = as.double(playerid)) %>% 
  dplyr::left_join(mlbplotR::load_headshots() %>% select(fangraphs_id, espn_headshot), by=c("playerid"="fangraphs_id")) %>% 
  drop_na()

sp_test %>% 
  select(name, espn_headshot, team_logo_espn, jan23, feb25, delta, percent_change, projectedPoints) %>% 
  arrange(-percent_change) %>% 
  slice_head(n=10) %>% 
  gt() %>% 
  gt_img_rows(columns= "espn_headshot", height = 50) %>% 
  gt_img_rows(columns = "team_logo_espn", height = 50) %>% 
  tab_header(title = "MLB The Dinger - Top 10 Starting Pitcher Risers", 
            subtitle = "Period: Jan23 to feb25") %>% 
  tab_footnote(footnote = "Data from Underdog MLB Rankings, players ADP > 239 filtered out") #%>% gtsave(filename = "tab1.html")


# 4.0 IF ------------------------------------------------------------------

infielders <- rankings %>% filter(slotName == "IF") %>%  arrange(feb25) %>%  drop_na() %>% left_join(teams_colors_logos %>% select(team_name, team_logo_espn), by=c('teamName'='team_name'))

infielders %>% slice_head(n=10) %>% gt() %>% gt_img_rows(columns = team_logo_espn)

df <- baseballr::fg_batter_leaders(x=2022, y=2022, q = 100, pitcher_type = "sta")


# 5.0 OF ------------------------------------------------------------------

outfielders <- rankings %>% filter(slotName == "OF") %>%  arrange(feb25) %>%  drop_na() %>% left_join(teams_colors_logos %>% select(team_name, team_logo_espn), by=c('teamName'='team_name'))

outfielders %>% slice_head(n=10) %>% gt() %>% gt_img_rows(columns = team_logo_espn)


