#load packages
suppressMessages({
  library(nflverse) #	Easily Install and Load the 'nflverse'
  library(tidyverse) # Easily Install and Load the 'Tidyverse'
  library(ggrepel) # Automatically Position Non-Overlapping Text Labels with 'ggplot2'
  library(ggimage) # Use Image in 'ggplot2'
})

data <- load_pbp(2021)

data %>% 
  select(posteam, defteam, desc, rush, pass) %>% 
  head()

plot_down_pass <- function(number) {
  pass <- data %>% 
    filter(pass == 1 | rush == 1) %>%
    filter(down == number) %>% 
    group_by(posteam) %>% 
    summarise(
      pass = sum(pass),
      rush = sum(rush), 
      sum = pass + rush
    ) %>% 
    mutate(pass_per = round(pass / (pass + rush), digits = 3), 
           rush_per = round(rush / (pass + rush), digits = 3)) %>% 
    arrange(-pass_per) %>% 
    left_join(teams_colors_logos, by=c("posteam"="team_abbr"))
  
  ggplot(pass, aes(x=posteam, y=pass_per)) +
    geom_hline(yintercept = mean(pass$pass_per), color="red", linetype="dashed", alpha=0.5)+
    geom_image(aes(image = team_logo_espn), size=pass$sum / (15 *max(pass$sum)), asp=16/9) +
    theme_bw() +
    labs(
      title = paste("2021 First Down Passing%"), 
      caption = "Twitter: @Its_MikeF \n Data from nflverse"
    )
}

plot_down_pass(3)

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
