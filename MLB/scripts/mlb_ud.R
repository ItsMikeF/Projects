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
date2 = "feb26"

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
  select(name, slotName, adp.x, adp.y, delta, percent_change, projectedPoints, positionRank, teamName) %>% 
  rename(.,"jan23"=adp.x) %>% 
  rename(.,"feb26"=adp.y) %>% 
  arrange(feb26) %>% 
  left_join(teams_colors_logos %>% select(team_name, team_abbr, team_logo_espn),by=c('teamName'='team_name')) %>% 
  rename(., team = team_logo_espn)

#get all player ids
{
  #pitchers <- baseballr::fg_pitcher_leaders(x=2022, y=2022, q = 100, pitcher_type = "sta")
  #batters <- baseballr::fg_bat_leaders(x=2022, y=2022, qual = 0)
  
  #abc <- batters %>% select(playerid, Name)
  #def <- pitchers %>% select(playerid, Name)
  
  #playerids <- rbind(abc, def)
  
  }

rankings$name <- iconv(rankings$name, to='ASCII//TRANSLIT')

rankings <- rankings %>% 
  drop_na(feb26) %>% 
  left_join(playerids, by=c('name'='Name')) %>% 
  mutate(playerid = as.double(playerid)) %>% 
  left_join(mlbplotR::load_headshots() %>% select(fangraphs_id, espn_headshot) %>% drop_na(fangraphs_id), by=c("playerid"="fangraphs_id"))

rankings %>% 
  arrange(feb26) %>% 
  select(team, espn_headshot, name, feb26, delta, percent_change, projectedPoints) %>% 
  unique() %>% 
  gt() %>% 
  gt_img_rows(columns = team, height = 50) %>% 
  gt_img_rows(columns = espn_headshot, height = 50)

# 2.0 team data -----------------------------------------------------------

team_1 <- rankings_2 %>% 
  drop_na(adp) %>% 
  filter(adp < 230) %>% 
  group_by(teamName) %>% 
  summarise(adp_mean = round(mean(adp, na.rm = T),digits = 1), 
            proj = sum(projectedPoints))

team <- rankings %>% 
  drop_na() %>% 
  filter(feb26 < 239) %>% 
  group_by(teamName) %>% 
  summarise(adp_mean = round(mean(feb26, na.rm = T),digits = 1),
            adp_delta = round(mean(delta, na.rm = T),digits = 1), 
            percent_change= round(mean(percent_change, na.rm = T),digits = 2)) %>% 
  arrange(-adp_delta) %>% 
  left_join(teams_colors_logos %>% select(team_name, team_logo_espn),by=c('teamName'='team_name'))


team %>%
  select(teamName, team_logo_espn, adp_mean, adp_delta, percent_change) %>% 
  gt() %>% 
  tab_header(title = "Playoff Best Ball - Mean Team ADP Movement", 
             subtitle = "Period: Jan23 to feb26") %>% 
  gt_img_rows(columns=team_logo_espn) %>% 
  tab_footnote(footnote = "Data from Underdog MLB Rankings, players ADP > 239 filtered out")


# 3.0 Pitchers ------------------------------------------------------------

sp <- rankings %>% filter(slotName == "P") %>%  arrange(feb26) %>%  drop_na()

sp %>% slice_head(n=10) %>% gt() %>% gt_img_rows(columns = team) %>% gt_img_rows(columns = espn_headshot)

df <- baseballr::fg_pitcher_leaders(x=2022, y=2022, q = 100, pitcher_type = "sta") %>% 
  mutate(Team = clean_team_abbrs(Team))

eno_pitchers <- readxl::read_excel(path = "./data/Cheat-Sheet-Generator-0221.xlsx", sheet = "SP") %>% 
  select(Player, FP, FPRank, SD) %>% 
  mutate(across(where(is.numeric),round,1))

pitching_ranks <- read.csv("./data/2023_sarris_pitching_ranks.csv") 
pitching_ranks[,3:10] <- as.numeric(unlist(pitching_ranks[,3:10]))

sp %>% 
  left_join(eno_pitchers, by=c('name'='Player')) %>% 
  left_join(pitching_ranks, by=c('name'='Player')) %>% 
  select(team, espn_headshot, name, jan23, feb26, delta, percent_change, projectedPoints, positionRank, FP, FPRank, SD, ENO, IP, PPERA, PPK, PPSTUFF., INJURY.PCT, NFC.ADP) %>% 
  arrange(feb26) %>% 
  #slice_head(n=10) %>% 
  gt() %>% 
  gt_img_rows(columns= espn_headshot, height = 50) %>% 
  gt_img_rows(columns = team, height = 50) %>% 
  tab_header(title = "MLB The Dinger - Starting Pitchers",
             subtitle = "Period: Jan23 to feb26") %>% 
  tab_spanner(label = "Underdog", columns = c(jan23, feb26, delta, percent_change, projectedPoints, positionRank)) %>% 
  tab_spanner(label = "Eno Fantasy Ranks", columns = c(FP, FPRank, SD)) %>% 
  tab_spanner(label = "Eno Pitching Ranks", columns = c(ENO, IP, PPERA, PPK, PPSTUFF., INJURY.PCT, NFC.ADP)) %>% 
  tab_footnote(footnote = "Data from Underdog MLB Rankings, players ADP > 240 filtered out") %>% 
  opt_stylize(style = 6, color = "gray", add_row_striping = T) %>% 
  data_color(columns = projectedPoints, colors = scales::col_numeric(
    palette = c("red", "green"),
    domain = c(400, 1300))) %>% 
  data_color(columns = percent_change, colors = scales::col_numeric(
    palette = c("red", "green"),
    domain = c(-.1,0.3))) %>% 
  data_color(columns = FP, colors = scales::col_numeric(
    palette = c("red", "green"),
    domain = c(0, 551))) %>% 
  data_color(columns = INJURY.PCT, colors = scales::col_numeric(
    palette = c("green","red"),
    domain = c(0,100))) %>% 
  data_color(columns = FPRank, colors = scales::col_numeric(
    palette = c("green","red"),
    domain = c(0,100))) %>% 
  data_color(columns = ENO, colors = scales::col_numeric(
    palette = c("green","red"),
    domain = c(1,100))) %>% 
  data_color(columns = PPSTUFF., colors = scales::col_numeric(
    palette = c("red", "green"),
    domain = c(80, 134))) %>% 
  data_color(columns = IP, colors = scales::col_numeric(
    palette = c("red", "green"),
    domain = c(140, 220))) %>% 
  data_color(columns = PPK, colors = scales::col_numeric(
    palette = c("red", "green"),
    domain = c(0.2, 0.36))) %>% gtsave(filename = "2023 02 26 - MLB The Dinger - Starting Pitchers Overview.html") 

# 4.0 IF ------------------------------------------------------------------

infielders <- rankings %>% filter(slotName == "IF") %>% arrange(feb26) %>%  drop_na() %>% 
  left_join(teams_colors_logos %>% select(team_name, team_logo_espn), by=c('teamName'='team_name'))

infielders$name <- iconv(infielders$name, to='ASCII//TRANSLIT')

infielders %>% slice_head(n=10) %>% gt() %>% gt_img_rows(columns = team_logo_espn)

df <- baseballr::fg_batter_leaders(x=2022, y=2022, q = 200) %>% select(playerid, Name, Team, G, PA, HR, OPS, wOBA, wRC)

test <- infielders %>% 
  left_join(df, by=c('name'='Name')) %>% 
  mutate(playerid = as.double(playerid)) #%>% left_join(mlbplotR::load_headshots(), by=c("playerid"="fangraphs_id")) 

headshots <- load_headshots()

test2 <- test %>% 
  left_join(load_headshots() %>% select(fangraphs_id, espn_headshot), by=c("playerid"="fangraphs_id")) 


# 5.0 OF ------------------------------------------------------------------

outfielders <- rankings %>% filter(slotName == "OF") %>%  arrange(feb26) %>%  drop_na() %>% left_join(teams_colors_logos %>% select(team_name, team_logo_espn), by=c('teamName'='team_name'))

outfielders %>% slice_head(n=10) %>% gt() %>% gt_img_rows(columns = team_logo_espn)

