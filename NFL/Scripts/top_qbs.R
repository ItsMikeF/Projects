#load packages
suppressMessages({
  library(tidyverse) #metapackage
  library(nflverse) #functions to efficiently access NFL pbp data
})

pbp <- load_pbp(seasons = 2018:2021)

names <- tibble(names(pbp))

qbs <- pbp %>% 
  group_by(passer, passer_id) %>% 
  summarise(epa = round(sum(epa, na.rm = T), digits = 0), 
            qb_epa = round(sum(qb_epa, na.rm = T), digits = 0), 
            cpoe = round(mean(cpoe, na.rm = T), digits = 2),
            dropbacks = sum(qb_dropback)) %>% 
  arrange(-qb_epa) %>%
  slice_head(n=10)

qbs %>% 
  arrange(-qb_epa)

geom_nfl_headshots(data = qbs)

ggplot(df, aes(x = a, y = b)) +
  geom_nfl_headshots(aes(player_gsis = player_gsis), height = 0.2) +
  geom_label(aes(label = player_name), nudge_y = -0.35, alpha = 0.5) +
  coord_cartesian(xlim = c(0.75, 3.25), ylim = c(0.7, 3.25)) +
  theme_void()