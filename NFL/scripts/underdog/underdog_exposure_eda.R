#check draft positions


# 0.0 load packages -------------------------------------------------------

suppressMessages({
  library(nflfastR) #nflfastr nflseedr nflplotr
  library(tidyverse) #ggplot2 dplyr tibble tidyr purrr forecats 
  library(ggrepel) #automatically position non-overlapping text labels
  library(glue) #interpreted literal strings
  library(gt)
  library(gtExtras)
  library(paletteer)
  library(nflverse)
})


# 1.0 define values -------------------------------------------------------

year = 2023
season = 0
playoffs = 1


# 2.0 load exposures and projections--------------------------------------------

exposure <- read.csv("./projections_playoffs/2023_nfl_playoff_expo.csv") 
exposure <- exposure%>% 
  mutate(Picked.At = as.Date(as.POSIXct(exposure$Picked.At, format="%Y-%m-%d %H:%M:%S", tz="UTC")), 
         name = paste(First.Name, Last.Name)) %>% 
  select(name, Team, Position, Picked.At, Pick.Number, Draft)

projections_udd <- read.csv("./projections_playoffs/underdog_playoff_projections_jan14.csv") %>% 
  mutate(name = paste(firstName, lastName), 
         adp = as.numeric(adp)) %>% 
  select(name, adp, projectedPoints, positionRank)

exposure_adp <- exposure %>% 
  left_join(projections_udd, by=c("name")) %>% 
  mutate(value = Pick.Number-adp, 
         rel_value = round(value/adp, digits = 2))

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
  group_by(Draft) %>% 
  summarize(total_picks = n(),
            total_value = sum(value), 
            total_rel_value = sum(rel_value), 
            Picked.At = last(Picked.At)) %>% 
  mutate(value_per_pick = total_value/total_picks, 
         rel_value_per_pick = total_rel_value/total_picks) %>% 
  arrange(-rel_value_per_pick)


# load pbp players --------------------------------------------------------


pbp_players <- function() {
  pbp <- load_pbp(2022)
  qb <- pbp %>% select(passer_id, passer) %>% drop_na() %>% unique() %>% rename(id=passer_id, player=passer) 
  rb <- pbp %>% select(rusher_id, rusher) %>% drop_na() %>% unique() %>% rename(id=rusher_id, player=rusher) 
  wr <- pbp %>% select(receiver_id, receiver) %>% drop_na() %>% unique() %>% rename(id=receiver_id, player=receiver) 
  
  players <<- rbind(qb, rb, wr) %>% unique()
}
pbp_players()


# best draft  -------------------------------------------------------------


best <- exposure_adp %>% 
  filter(Draft=="2608c829-c72a-4fbf-9376-50e367ae927e") %>% 
  select(name, Team, Pick.Number, adp, value, rel_value) %>% 
  arrange(Pick.Number)  %>% 
  separate(name, into = c("first_name", "last_name"), sep=" ") %>% 
  mutate(first = substr(first_name, 1, 1), 
         name = paste(first, last_name, sep = "."), 
         player = paste(first_name, last_name)) %>% 
  left_join(players, by=c('name'='player')) %>% 
  left_join(teams_colors_logos %>% select(team_abbr, team_logo_espn), by=c('Team'='team_abbr')) %>% 
  select(player, team_logo_espn, Pick.Number,adp, value, rel_value, id)

best %>% 
  gt() %>% 
  gt_img_rows(columns = team_logo_espn) %>% 
  gt_color_rows(rel_value, palette = c("red","green"))

# #top ten highest value picks-------------------------------------------------------------------------

exposure_adp %>% 
  select(name, Pick.Number, adp, value, rel_value, Picked.At, Draft) %>% 
  arrange(-rel_value) %>% 
  slice_head(n=10)