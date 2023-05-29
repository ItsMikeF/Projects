#mlb season 
#source for the gt table
#https://jthomasmock.github.io/gtExtras/reference/gt_img_rows.html

#load packages
suppressMessages({
  library(tidyverse) #metapackage
  library(fs) #Cross-Platform File System Operations Based on 'libuv'
  library(gt)
  library(gtExtras)
  library(webshot2)
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
date2 = "mar26"

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
rankings <- rankings_2 %>% 
  left_join(rankings_1 %>% select(name, adp, projectedPoints), by=c("name")) %>% 
  mutate(delta = adp.y-adp.x, 
         percent_change = round(delta/adp.x,digits = 2), 
         projectedPoints_delta = projectedPoints.x - projectedPoints.y) %>% 
  rename(projectedPoints = projectedPoints.x, 
         projectedPoints_jan23 = projectedPoints.y) %>% 
  select(name, slotName, adp.x, adp.y, delta, percent_change, projectedPoints, projectedPoints_jan23, projectedPoints_delta, positionRank, teamName) %>% 
  rename(.,"jan23"=adp.y) %>% 
  rename(.,"mar26"=adp.x) %>% 
  arrange(mar26) %>% 
  left_join(teams_colors_logos %>% select(team_name, team_abbr, team_logo_espn),by=c('teamName'='team_name')) %>% 
  rename(., team = team_logo_espn) %>% 
  distinct()

#get all player ids
{
  #pitchers <- baseballr::fg_pitcher_leaders(x=2022, y=2022, q = 100, pitcher_type = "sta")
  #batters <- baseballr::fg_bat_leaders(x=2022, y=2022, qual = 0)
  
  #abc <- batters %>% select(playerid, Name)
  #def <- pitchers %>% select(playerid, Name)
  
  #playerids <- rbind(abc, def)
  
  }

playerids <- read.csv("./data/playerids.csv")

rankings$name <- iconv(rankings$name, to='ASCII//TRANSLIT')

rankings <- rankings %>% 
  drop_na(mar26) %>% 
  left_join(playerids, by=c('name'='Name')) %>% 
  mutate(playerid = as.double(playerid)) %>% 
  left_join(mlbplotR::load_headshots() %>% select(fangraphs_id, espn_headshot) %>% drop_na(fangraphs_id), by=c("playerid"="fangraphs_id"))

rankings %>% 
  arrange(mar26) %>% 
  select(team, espn_headshot, name, mar26, delta, percent_change, projectedPoints) %>% 
  distinct() %>% 
  gt() %>% 
  gt_img_rows(columns = team, height = 50) %>% 
  gt_img_rows(columns = espn_headshot, height = 50)

rankings %>% 
  filter(projectedPoints_delta !=0) %>% 
  gt() %>% 
  gt_img_rows(columns = team, height = 50) %>% 
  gt_img_rows(columns = espn_headshot, height = 50) %>% 
  data_color(columns = projectedPoints_delta, colors = scales::col_numeric(
    palette = c("red", "green"),
    domain = c(-500, 500))) %>% 
  gtsave(filename = "rankings_delta.html")

# 2.0 team data -----------------------------------------------------------

team_1 <- rankings_2 %>% 
  drop_na(adp) %>% 
  filter(adp < 230) %>% 
  group_by(teamName) %>% 
  summarise(adp_mean = round(mean(adp, na.rm = T),digits = 1), 
            proj = sum(projectedPoints))

team <- rankings %>% 
  drop_na() %>% 
  filter(mar26 < 239) %>% 
  group_by(teamName) %>% 
  summarise(adp_mean = round(mean(mar26, na.rm = T),digits = 1),
            adp_delta = round(mean(delta, na.rm = T),digits = 1), 
            percent_change= round(mean(percent_change, na.rm = T),digits = 2)) %>% 
  arrange(-adp_delta) %>% 
  left_join(teams_colors_logos %>% select(team_name, team_logo_espn),by=c('teamName'='team_name'))

team %>%
  arrange(adp_mean) %>% 
  mutate(Rank = 1:30) %>% 
  select(Rank, teamName, team_logo_espn, adp_mean, adp_delta, percent_change) %>% 
  gt() %>% 
  tab_header(title = "Playoff Best Ball - Mean Team ADP Movement", 
             subtitle = "Period: Jan23 to mar26") %>% 
  gt_img_rows(columns=team_logo_espn) %>% 
  tab_footnote(footnote = "Data from Underdog MLB Rankings, players ADP > 239 filtered out") %>% 
  data_color(columns = percent_change, colors = scales::col_numeric(
    palette = c("red", "green"),
    domain = c(min(team$percent_change), max(team$percent_change)))) %>% 
  gt_theme_dark()

#lets look at mean adp movement by team
tab1 <- team %>%
  select(teamName, team_logo_espn, adp_mean, adp_delta, percent_change) %>% 
  arrange(adp_mean) %>% 
  slice(1:15) %>% 
  gt() %>% 
  tab_header(title = "Playoff Best Ball - Mean Team ADP Movement", 
             subtitle = "Period: Jan23 to mar26") %>% 
  gt_img_rows(columns=team_logo_espn) %>% 
  tab_footnote(footnote = "Data from Underdog MLB Rankings, players ADP > 239 filtered out") %>% 
  data_color(columns = percent_change, colors = scales::col_numeric(
    palette = c("red", "green"),
    domain = c(min(team$percent_change), max(team$percent_change)))) %>% 
  gt_theme_dark()

#lets look at mean adp movement by team
tab2 <- team %>%
  select(teamName, team_logo_espn, adp_mean, adp_delta, percent_change) %>% 
  arrange(adp_mean) %>%
  slice(16:30) %>% 
  gt() %>% 
  tab_header(title = "Playoff Best Ball - Mean Team ADP Movement", 
             subtitle = "Period: Jan23 to mar26") %>% 
  gt_img_rows(columns=team_logo_espn) %>% 
  tab_footnote(footnote = "Data from Underdog MLB Rankings, players ADP > 239 filtered out") %>% 
  data_color(columns = percent_change, colors = scales::col_numeric(
    palette = c("red", "green"),
    domain = c(min(team$percent_change), max(team$percent_change)))) %>% 
  gt_theme_dark()

listed_tables <- list(tab1, tab2)

#split the table into two tables side by side with 16 rows each
gt_two_column_layout(listed_tables, output = "save", filename = "C:/Users/mikef/Documents/GitHub/Projects/MLB/gt_split.png")

# 3.0 Pitchers ------------------------------------------------------------

sp <- rankings %>% filter(slotName == "P") %>%  arrange(mar26) %>%  drop_na()

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
  select(team, espn_headshot, name, jan23, mar26, delta, percent_change, projectedPoints, positionRank, FP, FPRank, SD, ENO, IP, PPERA, PPK, PPSTUFF., INJURY.PCT, NFC.ADP) %>% 
  arrange(mar26) %>% 
  #slice_head(n=10) %>% 
  gt() %>% 
  gt_img_rows(columns= espn_headshot, height = 50) %>% 
  gt_img_rows(columns = team, height = 50) %>% 
  tab_header(title = "MLB The Dinger - Starting Pitchers",
             subtitle = "Period: Jan23 to mar26") %>% 
  tab_spanner(label = "Underdog", columns = c(jan23, mar26, delta, percent_change, projectedPoints, positionRank)) %>% 
  tab_spanner(label = "Eno Fantasy Ranks", columns = c(FP, FPRank, SD)) %>% 
  tab_spanner(label = "Eno Pitching Ranks", columns = c(ENO, IP, PPERA, PPK, PPSTUFF., INJURY.PCT, NFC.ADP)) %>% 
  tab_footnote(footnote = "Data from Underdog MLB Rankings, players ADP > 240 filtered out") %>% 
  #opt_stylize(style = 6, color = "gray", add_row_striping = T) %>% 
  gt_theme_dark() %>% 
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
    domain = c(0.2, 0.36))) %>% 
  gtsave(filename = glue("{date2}, 2023 - MLB The Dinger - Starting Pitchers Overview.html"))

# 4.0 IF ------------------------------------------------------------------

infielders <- rankings %>% filter(slotName == "IF") %>% arrange(mar26) %>%  drop_na() 

infielders$name <- iconv(infielders$name, to='ASCII//TRANSLIT')

infielders %>% gt() %>% gt_img_rows(columns = team)

df <- baseballr::fg_batter_leaders(x=2022, y=2022, q = 200) %>% select(playerid, Name, Team, G, PA, HR, OPS, wOBA, wRC)

test <- infielders %>% 
  left_join(df, by=c('name'='Name')) #mutate(playerid = as.double(playerid)) #%>% left_join(mlbplotR::load_headshots(), by=c("playerid"="fangraphs_id")) 

headshots <- load_headshots()

test2 <- test %>% 
  left_join(load_headshots() %>% select(fangraphs_id, espn_headshot), by=c("playerid"="fangraphs_id")) 


# 5.0 OF ------------------------------------------------------------------

outfielders <- rankings %>% filter(slotName == "OF") %>%  arrange(mar26) %>% 
  drop_na() %>% distinct()

outfielders %>% 
  #arrange() %>% 
  select(team, espn_headshot, name, mar26, delta, percent_change, projectedPoints) %>% 
  gt() %>% gt_img_rows(columns = team, height = 50) %>% gt_img_rows(columns = espn_headshot, height = 50) %>% 
  tab_header(title = "MLB The Dinger - Biggest Risers", subtitle = "Period: Jan23 - mar26") %>% 
  data_color(projectedPoints, colors = scales::col_numeric(
    palette = c("red", "green"),
    domain = c(400, 1650)))


# 6.0 test ----------------------------------------------------------------

rankings %>% 
  arrange(mar26) %>% 
  select(team, espn_headshot, name, positionRank,mar26, jan23, delta, percent_change, projectedPoints) %>% 
  distinct() %>% 
  #slice_head(n=10) %>% 
  gt() %>% gt_img_rows(columns = team, height = 50) %>% gt_img_rows(columns = espn_headshot, height = 50) %>% 
  tab_header(title = "2023 MLB The Dinger - Top 10 Fallers", subtitle = "Period: Jan23 - mar26") %>% 
  tab_footnote(footnote = "Data from Underdog Fantasy") %>% 
  data_color(projectedPoints, colors = scales::col_numeric(
    palette = c("red", "green"),
    domain = c(400, 1700))) %>% 
  data_color(mar26, colors = scales::col_numeric(
    palette = c("green", "red"),
    domain = c(1,240)
  )) %>% 
  data_color(percent_change, colors = scales::col_numeric(
    palette = c("red", "green"), 
    domain = c(-.6,.6)
  )) %>% 
  gt_theme_dark() %>% 
  gtsave(filename = "MLB The Dinger - Board.html")
