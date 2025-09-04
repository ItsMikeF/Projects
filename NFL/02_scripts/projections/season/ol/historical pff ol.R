# ol pff historical grades

# ---- packages ----
library(tidyverse)
library(nflverse)
library(randomForest)
library(glue)

# ---- 0) Load & filter base data ----
ol_grades_player <- readRDS("./01_data/season_grades/nfl/ol_grades_player.RDS")

ol_grades_team <- ol_grades_player %>% 
  group_by(season, team_name) %>% 
  summarize(team_pass_block = round(weighted.mean(grades_pass_block, snap_counts_pass_block), digits = 1),
            team_run_block = round(weighted.mean(grades_run_block, snap_counts_run_block), digits = 1), 
            .groups = "drop") %>%
  mutate(join = paste(season, team_name, sep = "_"))

saveRDS(ol_grades_team, file="./01_data/season_grades/nfl/ol_grades_team.RDS")
