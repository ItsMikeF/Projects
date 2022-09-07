#load packages
suppressMessages({
  library(tidyverse)
  library(cfbfastR)
})

#run the following to add the key to the Renviron
#usethis::edit_r_environ()
#cfbd_key <- "lDpblntVUpUafh2geyo/tqM6QMErJgcl8FnpnugstY7wBLPmllehGfB/EYknH0VV"

#read csv
dks <- read_csv("./contests/2022_w0/DKSalaries.csv")

#get teams on the slate
teams <- unique(dks$TeamAbbrev)

#team
test <- list()

for (i in 1:length(teams)) {
  test[i] <- cfbd_game_team_stats(2021, team = teams[i])
}

#team
cfb_team <- cfbd_game_team_stats(2022, team = teams[2])
