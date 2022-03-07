library(nflfastR)
library(tidyverse)
library(ggrepel)

pbp <- load_pbp(2021)
qbs <- pbp %>%
  filter(season_type == "REG", !is.na(epa)) %>%
  group_by(id, name) %>%
  summarize(
    epa = mean(qb_epa),
    cpoe = mean(cpoe, na.rm = T),
    n_dropbacks = sum(pass),
    n_plays = n(),
    team = last(posteam)
  ) %>%
  ungroup() %>%
  filter(n_dropbacks > 300)

qbs <- qbs %>%
  left_join(teams_colors_logos, by = c('team' = 'team_abbr'))
