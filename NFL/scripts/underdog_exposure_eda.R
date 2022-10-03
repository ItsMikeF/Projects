#check draft positions

#load packages
suppressMessages({
  library(nflfastR) #nflfastr nflseedr nflplotr
  library(tidyverse) #ggplot2 dplyr tibble tidyr purrr forecats 
  library(ggrepel) #automatically position non-overlapping text labels
  library(glue) #interpreted literal strings
})

exposure <- read.csv("underdog_exposure_2022.csv") 
exposure <- exposure%>% 
  mutate(Picked.At = as.Date(as.POSIXct(exposure$Picked.At, format="%Y-%m-%d %H:%M:%S", tz="UTC")), 
         name = paste(First.Name, Last.Name)) %>% 
  select(name, Team, Position, Picked.At, Pick.Number, Draft)

projections_udd <- read.csv("./season_projections/projections_underdog.csv") %>% 
  mutate(name = paste(firstName, lastName), 
         adp = as.numeric(adp)) %>% 
  select(name, adp, projectedPoints, positionRank)

exposure_adp <- exposure %>% 
  left_join(projections_udd, by=c("name")) %>% 
  mutate(value = Pick.Number-adp, 
         rel_value = round(value/adp, digits = 2))

exposure_adp %>% 
  group_by(Picked.At) %>% 
  summarize(total_picks = n(),
            total_value = sum(value), 
            total_rel_value = sum(rel_value)) %>% 
  mutate(value_per_pick = total_value/total_picks, 
         rel_value_per_pick = total_rel_value/total_picks)

exposure_adp %>% 
  group_by(Draft) %>% 
  summarize(total_picks = n(),
            total_value = sum(value), 
            total_rel_value = sum(rel_value)) %>% 
  mutate(value_per_pick = total_value/total_picks, 
         rel_value_per_pick = total_rel_value/total_picks) %>% 
  arrange(-rel_value_per_pick)

exposure_adp %>% 
  select(name, Pick.Number, adp, value, rel_value, Picked.At) %>% 
  arrange(-rel_value) %>% 
  slice_head(n=10)
