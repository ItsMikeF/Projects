#explore the torvik package

#load packages
suppressMessages({
  library(tidyverse) #ggplot2 dplyr tidyr readr stringr forcats purrr tibble
  library(toRvik) #extensive and tidy ncaa mens cbb data
  library(gt)
})

player_recruiting_rankings()
transfer_portal()
bart_players()
bart_poy()

#dev page
#https://t.co/xL5j8bsIdJ

ratings <- bart_ratings()
