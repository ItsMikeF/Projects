#load packages
suppressMessages({
  library(tidyverse) #metapackage
  library(nflverse) #functions to efficiently access NFL pbp data
})

pbp <- nflreadr::load_pbp(2021) %>% 
  dplyr::filter(season_type == "REG") %>%
  dplyr::filter(!is.na(posteam) & (rush == 1 | pass == 1))

offense <- pbp %>%
  dplyr::group_by(team = posteam) %>%
  dplyr::summarise(off_epa = mean(epa, na.rm = TRUE))

defense <- pbp %>%
  dplyr::group_by(team = defteam) %>%
  dplyr::summarise(def_epa = mean(epa, na.rm = TRUE))

combined <- offense %>%
  dplyr::inner_join(defense, by = "team")

qbs <- pbp %>%
  dplyr::filter(pass == 1 | rush == 1) %>%
  dplyr::filter(down %in% 1:4) %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(
    name = dplyr::first(name),
    team = dplyr::last(posteam),
    plays = dplyr::n(),
    qb_epa = mean(qb_epa, na.ram = TRUE)
  ) %>%
  dplyr::filter(plays > 200) %>%
  dplyr::slice_max(qb_epa, n = 10)

#Logos in Scatter Plots
ggplot2::ggplot(combined, aes(x = off_epa, y = def_epa)) +
  ggplot2::geom_abline(slope = -1.5, intercept = seq(0.4, -0.3, -0.1), alpha = .2) +
  nflplotR::geom_mean_lines(aes(v_var = off_epa , h_var = def_epa)) +
  nflplotR::geom_nfl_logos(aes(team_abbr = team), width = 0.065, alpha = 0.7) +
  ggplot2::labs(
    x = "Offense EPA/play",
    y = "Defense EPA/play",
    caption = "Data: @nflfastR",
    title = "2021 NFL Offensive and Defensive EPA per Play"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    plot.title.position = "plot"
  ) +
  ggplot2::scale_y_reverse()

#Player Headshots as Axis Labels
ggplot2::ggplot(qbs, aes(x = reorder(id, -qb_epa), y = qb_epa)) +
  ggplot2::geom_col(aes(color = team, fill = team), width = 0.5) +
  nflplotR::scale_color_nfl(type = "secondary") +
  nflplotR::scale_fill_nfl(alpha = 0.4) +
  ggplot2::labs(
    title = "2021 NFL Quarterback EPA per Play",
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

df <- mtcars |> 
  dplyr::mutate(
    team = sample(c("LAC", "BUF", "DAL", "ARI"), nrow(mtcars), TRUE),
    player = sample(c("00-0033873", "00-0035228", "00-0036355", "00-0019596"), nrow(mtcars), TRUE)
  )

ggplot(df, aes(x = mpg, y = disp)) +
  geom_point() +
  facet_wrap(vars(team)) +
  labs(
    title = tools::toTitleCase("These are random teams and data"),
    subtitle = "I just want to show how the nflplotR theme elements work",
    caption = "https://github.com/nflverse/nflseedR/raw/master/man/figures/caption.png"
  ) +
  theme_minimal() +
  theme(
    plot.title.position = "plot",
    plot.title = ggplot2::element_text(face = "bold"),
    axis.title = element_blank(),
    # make wordmarks of team abbreviations
    strip.text = element_nfl_wordmark(size = 1),
    # load image from url in caption
    plot.caption = element_path(hjust = 1, size = 0.4)
  )
