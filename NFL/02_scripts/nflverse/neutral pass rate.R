# team neutral pass rate

# load packages
library(nflverse)
library(tidyverse)
library(httr)
library(rvest)

# load pbp
pbp <- load_pbp(2023)

n_pass <- pbp %>% 
  filter(down <= 4, .25 < wp, wp < .75, half_seconds_remaining > 120) %>% 
  group_by(posteam) %>% 
  summarise(pass = sum(pass)) %>% 
  arrange(-pass)

plays <- pbp %>% 
  filter(down <= 4, .25 < wp, wp < .75, half_seconds_remaining > 120) %>% 
  group_by(posteam) %>% 
  summarise(plays = n()) %>% 
  arrange(-plays)

neutral_pass <- n_pass %>% 
  left_join(plays, by=c("posteam")) %>% 
  mutate(neutral_pass_rate = round(pass/plays, digits = 2)) %>% 
  arrange(-neutral_pass_rate)

plot <- ggplot(neutral_pass, aes(y= reorder(posteam, neutral_pass_rate), x=neutral_pass_rate)) +
  geom_col(aes(color=posteam, fill = posteam), width = 0.5) +
  geom_nfl_logos(aes(team_abbr=posteam), width = 0.025) +
  scale_color_nfl(type = "secondary") +
  scale_fill_nfl(alpha = 0.4) + 
  theme_minimal() +
  labs(
    title = "2023 Neutral Pass Rate", 
    subtitle = "@ItsMikeF | Data: nflfastr | ",
    x = "Neutral Pass rate"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 30), 
    plot.title.position = "plot", 
    
    plot.subtitle = element_text(size = 24),
    
    axis.title.y = element_blank(), 
    #axis.text.y = element_nfl_logo(size = 1.5),
    axis.text.y = element_text(size = 20),
    
    axis.title.x = element_text(size = 24), 
    axis.text.x = element_text(size = 20)
  )

ggsave(plot, 
       filename = "./03_plots/neutral_pass_rate.png", 
       width = 26.7, 
       height = 15, 
       dpi = 300)