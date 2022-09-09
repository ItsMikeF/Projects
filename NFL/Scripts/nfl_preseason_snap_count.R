#lets check preseason snap counts for espn starters

#load preseason data

summary_pre_qb <- read.csv("./preseason/2022/passing_summary.csv")
summary_pre_rb <- read.csv("./preseason/2022/rushing_summary.csv")
summary_pre_wr <- read.csv("./preseason/2022/receiving_summary.csv")
summary_pre_ol <- read.csv("./preseason/2022/offense_blocking.csv")
summary_pre_def <- read.csv("./preseason/2022/defense_summary.csv")

qb_pre <- qb1 %>% 
  left_join(summary_pre_qb, by=c("player"))

qb_pre_plot <- qb_pre %>% 
  select(player, team, player_id, grades_pass, passing_snaps)

#load pbp data for player ids
pbp_2021 <- nflreadr::load_pbp(2021) %>% 
  filter(!is.na(posteam) & (rush == 1 | pass == 1)) %>% 
  group_by(passer) %>% 
  summarize(
    player_id =
    epa = mean(epa, na.rm=T)
  )

#rb plot
plot_rb_pbp %>% 
  ggplot(aes(x = rblk, y = elusive_rating)) +
  #geom_nfl_logos(aes(team_abbr=team.x), width=0.065, alpha=0.7) +
  geom_nfl_headshots(aes(player_gsis = id), width = 0.075, vjust = 0.45) +
  geom_label_repel(aes(label = player)) +
  xlab("Avg IOL 2021 RBLK Grades") +
  labs(
    title = "2022 RB Review",
    caption = "2021 IOL Grade based on starting LG, C, RG on 2022 ESPN Depth Chart. \n 
    The PFF Elusive Rating distills the success and impact of a runner with the ball independently of the blocking in front of him by looking at how hard he was to bring down.",
    y = "Elusiveness Rating"
  )

ggplot2::ggplot(qb_pre, aes(x = reorder(id, -passing_snaps), y = passing_snaps)) +
  ggplot2::geom_col(aes(color = team, fill = team), width = 0.5) +
  nflplotR::scale_color_nfl(type = "secondary") +
  nflplotR::scale_fill_nfl(alpha = 0.4) +
  ggplot2::labs(
    title = "2020 NFL Quarterback EPA per Play",
    y = "EPA/play"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    plot.title.position = "plot",
    # it's obvious what the x-axis is so we remove the title
    axis.title.x = ggplot2::element_blank(),
    # this line triggers the replacement of gsis ids with player headshots
    axis.text.x = element_nfl_headshot(size = 1)
  )
