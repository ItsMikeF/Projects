#plays per game

# load packages
library(nflverse)
library(tidyverse)
library(httr)
library(rvest)

# load pbp
pbp <- load_pbp(2022)

plays <- pbp %>% 
  filter(season_type == "REG", down <= 4, play_type == "pass" | play_type == "run") %>% 
  group_by(posteam) %>% 
  summarise(plays_2022 = round(n()/17, digits = 1))

# run slate odds script #

# join points with plays
chart1 <- points %>% left_join(plays, by=c("team_abbr"="posteam"))

# ggplot with nfl team logo
plot <- ggplot(data = chart1, aes(x=proj_points, y=plays_2022)) +
  geom_nfl_logos(aes(team_abbr = team_abbr), width = .05) + 
  labs(title = "2023 Week 2 Team Projections", 
       subtitle = "@ItsMikeF", 
       y = "2022 Offensive Plays per Game", 
       x = "Projected Points") +
  geom_hline(yintercept = mean(chart1$plays_2022), linetype="dashed") +
  geom_vline(xintercept = mean(chart1$proj_points), linetype="dashed") +
  theme_bw() +
  theme(plot.title = element_text(size = 30), 
        plot.subtitle = element_text(size = 24),
        axis.title.x = element_text(size = 24),
        axis.text.x = element_text(size = 20),
        axis.title.y = element_text(size = 24), 
        axis.text.y = element_text(size = 20))

# save image
ggsave(plot = plot, 
       filename = "./03_plots/2023week2_1080p.png", 
       width = 26.7, 
       height = 15, 
       dpi = 300)

