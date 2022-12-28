#advance rates analysis

#load packages
suppressMessages({
  library(tidyverse) #metapackage
  library(nflverse) #functions to efficiently access NFL pbp data
  library(fs) #Cross-Platform File System Operations Based on 'libuv'
  library(ggrepel) #Automatically Position Non-Overlapping Text Labels with 'ggplot2'
})

# 1.0 projections --------------------------------------------------------------

#load the projections

adv_rate <- read.csv("./season_projections/bbm3_advance_rates.csv") %>%
  mutate(pbp_name = Player) %>% 
  separate(pbp_name, into = c("first_name", "last_name"), sep=" ") %>% 
  mutate(first = substr(first_name, 1, 1), 
         name = paste(first, last_name, sep = ".")) %>% 
  select(Player, Team, Pos, Alive.Team.Rate, ADP, Drafted.Count, Round.2.Count, Round.3.Count, Final.Round.Count, Adv.From.Round.1, Adv.From.Round.2, Adv.From.Round.3, name)

adv_rate %>% 
  filter(Alive.Team.Rate > .1) %>% 
  ggplot(aes(x=ADP, y=Alive.Team.Rate)) +
  geom_point() +
  geom_text_repel(aes(label=Player))

adv_rate %>% 
  filter(ADP < 36) %>% 
  ggplot(aes(x=ADP, y=Alive.Team.Rate)) +
  geom_point() +
  geom_text_repel(aes(label=Player))


# 2.0 pbp for player names and ids ---------------------------------------------------------------------

pbp <- nflreadr::load_pbp(2022) %>% 
  dplyr::filter(season_type == "REG") %>%
  dplyr::filter(!is.na(posteam) & (rush == 1 | pass == 1))

#names <- as.data.frame(names(pbp))

qbs <- pbp %>%
  dplyr::filter(pass == 1 | rush == 1) %>%
  dplyr::filter(down %in% 1:4) %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(
    name = dplyr::first(name),
    team = dplyr::last(posteam),
    plays = dplyr::n()
    #qb_epa = round(mean(qb_epa, na.ram = TRUE), digits = 3)
  ) %>%
  dplyr::filter(plays > 100)

rbs <- pbp %>%
  dplyr::filter(rush == 1) %>%
  dplyr::group_by(rusher_id) %>%
  dplyr::summarise(
    name = dplyr::first(name),
    team = dplyr::last(posteam),
    plays = dplyr::n()) %>% 
  filter(plays > 25) %>% 
  rename(., id=rusher_id) %>% 
  arrange(-plays)

wrs <- pbp %>% 
  group_by(receiver_id) %>% 
  summarise(
    name = first(receiver), 
    team = last(posteam), 
    plays = n()) %>% 
  drop_na() %>% 
  filter(plays >25) %>% 
  rename(., id=receiver_id) %>% 
  arrange(-plays)

players <- rbind(qbs, rbs, wrs) %>% 
  unique()


# 3.0 merge pbp and adv rates ---------------------------------------------

plot <- adv_rate %>% 
  left_join(players, by=c('name')) %>% 
  filter(ADP < 13)

ggplot2::ggplot(plot, aes(x = reorder(id, ADP), y = Alive.Team.Rate)) +
  ggplot2::geom_col(aes(color = team, fill = team), width = 0.5) +
  nflplotR::scale_color_nfl(type = "secondary") +
  nflplotR::scale_fill_nfl(alpha = 0.4) +
  ggplot2::labs(
    title = "BBM3 Alive Team Rates",
    y = "Alive Team Rate", 
    caption = "By: @Its_MikeF | Source: Underdog Fantasy via @HaydenWinks"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    plot.title.position = "plot",
    # it's obvious what the x-axis is so we remove the title
    axis.title.x = ggplot2::element_blank(),
    # this line triggers the replacement of gsis ids with player headshots
    axis.text.x = element_nfl_headshot(size = 2)
  )
