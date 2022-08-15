#https://cran.r-project.org/web/packages/rtweet/vignettes/intro.html

#load packages
library(tidyverse) #ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, forcats, 
library(rtweet) #calls designed to collect and organize Twitter data
library(httpuv) #HTTP and WebSocket Server Library

#You'll need to run this function once per computer
#so that rtweet can use your personal Twitter account.
auth_setup_default()

#add the keys
api_key <- "61108922-fcovElctrgVepoeZURjpYVWjX3bFuGvZVdNkvj2vd"
api_secret_key <- "BJi9x2qjvUDo8hgqqoVySEpyBexaCKUvq1GZHX4y1nA69"

## search for 18000 tweets using the rstats hashtag
rt <- search_tweets(
  "#rstats", n = 100, include_rts = F
)

names <- tibble(names(nfl_rt))

## preview tweets data
rt

## preview users data
users_data(rt)

## plot time series (if ggplot2 is installed)
ts_plot(rt)

## plot time series of tweets
ts_plot(rt, "3 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of #rstats Twitter statuses from past 9 days",
    subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

rt <- stream_tweets("")
rt <- stream_tweets(lookup_coords("london, uk"), timeout = 10)

me_fds <- get_friends("Its_MikeF")
me_fds_data <- lookup_users(me_fds$user_id) %>% 
  select(user_id, created_at, screen_name, text, source)

pyq <- lookup_users("its_mikef")
pyq$followers_count
pyq$verified

pyq_select <- pyq %>% 
  select(id, screen_name, followers_count, friends_count, favourites_count)

## get user IDs of accounts followed by CNN
tmls <- get_timelines(c("cnn", "BBCWorld", "foxnews"), n = 320)
tmls %>%
  dplyr::filter(created_at > "2017-10-29") %>%
  dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by news organization",
    subtitle = "Twitter status (tweet) counts aggregated by day from October/November 2017",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

#Get the 3,000 most recently favorited statuses by JK Rowling.

jkr <- get_favorites("jk_rowling", n = 3000)
mjf <- get_favorites("Its_MikeF", n= 12)

sf <- get_trends("new york")

usrs <- search_users("#rstats", n = 10)

gf <- get_followers(user = "Its_MikeF")
mjf$id[1]
