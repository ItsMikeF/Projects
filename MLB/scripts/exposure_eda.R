#check draft positions
#review best drafts


# 0.0 load packages -------------------------------------------------------

suppressMessages({
  library(tidyverse) #ggplot2 dplyr tibble tidyr purrr forecats 
  library(ggrepel) #automatically position non-overlapping text labels
  library(glue) #interpreted literal strings
  library(gt)
  library(gtExtras)
  library(paletteer)
  library(mlbplotR)
})

# 1.0 load mlbplotr data --------------------------------------------------------


#load team logos
teams_colors_logos <- mlbplotR::load_mlb_teams() %>% 
  filter(!team_abbr %in% c("AL", "NL", "MLB")) %>% 
  mutate(
    a = rep(1:6, 5), 
    b = sort(rep(1:5, 6), decreasing=T), 
    alpha = ifelse(grepl("A", team_abbr),1,0.75),
    color = ifelse(grepl("E", team_abbr), "b/w", NA)
  )


# 2.0 load exposures and projections--------------------------------------------

exposure <- read.csv("./data/exposure_mar20.csv")

exposure <- exposure %>% 
  mutate(Picked.At = as.Date(as.POSIXct(exposure$Picked.At, format="%Y-%m-%d %H:%M:%S", tz="UTC")), 
         name = paste(First.Name, Last.Name)) %>% 
  select(name, Team, Position, Picked.At, Pick.Number, Draft) %>% 
  left_join(teams_colors_logos %>% select(team_abbr, team_logo_espn), by=c('Team'='team_abbr')) %>% 
  left_join(read.csv("./data/playerids.csv"), by=c('name'='Name')) %>% 
  mutate(
    copy = paste0(name, Draft),
    playerid = as.double(playerid)) %>% 
  distinct(copy, .keep_all = T) %>% 
  left_join(mlbplotR::load_headshots() %>% select(fangraphs_id, espn_headshot) %>% drop_na(fangraphs_id), by=c("playerid"="fangraphs_id")) %>% 
  select(-copy) 
  
projections_udd <- read.csv("./projections_season/rankings_mar20.csv") %>% 
  mutate(name = paste(firstName, lastName), 
         adp = as.numeric(adp),
         group = substr(positionRank) %>% 
  select(name, adp, projectedPoints, positionRank)

exposure_adp <- exposure %>% 
  left_join(projections_udd, by=c("name")) %>% 
  mutate(value = Pick.Number-adp, 
         rel_value = round(value/adp, digits = 2)) %>% 
  drop_na(adp)

#drafts by date
drafts_by_date <- exposure_adp %>% 
  group_by(Picked.At) %>% 
  summarize(total_picks = n(),
            total_value = sum(value, na.rm = T), 
            total_rel_value = sum(rel_value, na.rm = T)) %>% 
  mutate(value_per_pick = round(total_value/total_picks,digits = 2), 
         rel_value_per_pick = round(total_rel_value/total_picks,digits=2))

#best drafts
drafts <- exposure_adp %>% 
  #drop_na() %>% 
  group_by(Draft) %>% 
  summarize(total_picks = n(),
            total_value = sum(value), 
            total_rel_value = sum(rel_value), 
            Picked.At = last(Picked.At)) %>% 
  mutate(value_per_pick = round(total_value/total_picks, digits = 2),
         rel_value_per_pick = round(total_rel_value/total_picks, digits = 2)) %>% 
  arrange(-rel_value_per_pick)


# best draft  -------------------------------------------------------------

best <- exposure_adp %>% 
  filter(Draft=="6889ddc6-edf2-48b9-bd71-71bb32b50c9d") %>% 
  select(name, team_logo_espn, espn_headshot, Pick.Number, adp, value, rel_value, projectedPoints) %>% 
  arrange(Pick.Number)

best %>% 
  gt() %>% 
  gt_img_rows(columns = team_logo_espn, height = 50) %>% 
  gt_img_rows(columns = espn_headshot, height = 50) %>% 
  gt_color_rows(rel_value, palette = c("red","green"), domain = c(-.5,.5)) %>% 
  gt_theme_dark() %>% 
  gtsave(filename = "best_draft.html")

# #top ten highest value picks-------------------------------------------------------------------------

exposure_adp %>% 
  select(name, Pick.Number, adp, value, rel_value, Picked.At, Draft) %>% 
  arrange(-rel_value) %>% 
  slice_head(n=10)

#group by team drafted

exposure_adp %>% 
  group_by(Team, team_logo_espn) %>% 
  summarise(count=n()) %>% 
  arrange(-count) %>% 
  ungroup() %>% 
  rename(team = team_logo_espn) %>% 
  slice_head(n=10) %>% 
  gt() %>% 
  gt_img_rows(columns = team) %>% 
  gt_theme_dark() 

#group by player
exposure_adp %>% 
  group_by(name, espn_headshot) %>% 
  summarise(count=n()) %>% 
  arrange(-count) %>% 
  ungroup() %>% 
  mutate(own = round(count/dim(drafts)[1],digits = 2)) %>% 
  gt() %>% 
  gt_img_rows(columns = espn_headshot, height = 50)


#group by position
exposure_adp %>% 
  group_by(Position) %>% 
  summarise(count=n()) %>% 
  arrange(-count) %>% 
  ungroup() %>% 
  mutate(own = round(count/sum(count),digits = 2)) %>% 
  gt() 


#group by position
exposure_adp %>% 
  group_by(Draft, Team) %>% 
  summarise(count=n()) %>% 
  arrange(-count) %>% 
  ungroup() %>% 
  group_by(Team) %>% 
  summarise(count=n()) %>% 
  arrange(-count) %>% 
  gt() %>% 
  gt_theme_dark()

