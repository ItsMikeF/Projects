#lets check preseason snap counts for espn starters

#load packages
suppressMessages({
  library(tidyverse) #ggplot2 dplyr tidyr readr stringr forcats purrr tibble
  library(nflverse) #nflfastr nflseedr nfl4th nflreadr nflplotr
  library(janitor) #simple little tools for examining and cleaning dirty data
  library(ggrepel) #automatically position non-overlapping text labels with ggplot2
  library(gt) #easily create presentation ready 
})

#load preseason data
summary_pre_qb <- read.csv("./preseason/2022/passing_summary.csv")
summary_pre_rb <- read.csv("./preseason/2022/rushing_summary.csv")
summary_pre_wr <- read.csv("./preseason/2022/receiving_summary.csv")
summary_pre_ol <- read.csv("./preseason/2022/offense_blocking.csv")
summary_pre_def <- read.csv("./preseason/2022/defense_summary.csv")

#combine starting qbs with pff passing summary
qb_pre <- qb1 %>% 
  left_join(summary_pre_qb, by=c("player"))

#select columns and name column to join with pbp names
qb_pre <- qb_pre %>% 
  select(player, grades_pass, passing_snaps) %>% 
  mutate(name = paste0(substr(player,1,1),".", str_extract(player, '[^ ]+$')),
         passing_snaps = replace_na(passing_snaps,0))

#load pbp data for player ids
pbp_2021 <- nflreadr::load_pbp(2021) 

pbp_2021_qbs <- pbp_2021 %>%
  dplyr::filter(pass == 1 | rush == 1) %>%
  dplyr::filter(down %in% 1:4) %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(
    name = dplyr::first(name),
    team = dplyr::last(posteam),
    plays = dplyr::n(),
    qb_epa = round(mean(qb_epa, na.ram = TRUE), digits = 3)
  ) 

#join pbp data to qb preseaon
qb_pre <- qb_pre %>% 
  left_join(pbp_2021_qbs, by=c("name")) 

#would like to add the logo instead of the team abbreviation
#https://gt.rstudio.com/reference/local_image.html
qb_pre %>% 
  select(player, team, grades_pass, passing_snaps) %>% 
  arrange(-passing_snaps) %>% 
  gt()

#plot passing snaps
ggplot2::ggplot(qb_pre, aes(x = reorder(id, -passing_snaps), y = passing_snaps)) +
  ggplot2::geom_col(aes(color = team, fill = team), width = 0.5) +
  nflplotR::scale_color_nfl(type = "secondary") +
  nflplotR::scale_fill_nfl(alpha = 0.4) +
  ggplot2::labs(
    title = "2022 NFL Quarterback Preseason Snap Counts",
    y = "Passing Snap Counts"
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

#qb plot
qb_pre %>% 
  drop_na(grades_pass) %>% 
  ggplot(aes(x = passing_snaps, y = grades_pass)) +
  geom_vline(xintercept = mean(qb_pre$passing_snaps, na.rm = T), color="red",linetype="dashed", alpha=0.5) +
  geom_hline(yintercept = mean(qb_pre$grades_pass, na.rm=T), color="red",linetype="dashed", alpha=0.5) +
  #geom_nfl_logos(aes(team_abbr=team.x), width=0.065, alpha=0.7) +
  geom_nfl_headshots(aes(player_gsis = id), width = 0.075, vjust = 0.45) +
  geom_label_repel(aes(label = player)) +
  ylim(min(qb_pre$grades_pass), 110) +
  xlab("Preason Passing Snaps") +
  labs(
    title = "2022 QB Preseason Review",
    caption = "Starting qbs who didnt play are not shown",
    y = "Passing Grade"
  )
