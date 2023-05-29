#load packages
suppressMessages({
  library(tidyverse)
  library(cfbfastR)
  })

#run the following to add the key to the Renviron
#usethis::edit_r_environ()
#cfbd_key <- "lDpblntVUpUafh2geyo/tqM6QMErJgcl8FnpnugstY7wBLPmllehGfB/EYknH0VV"

#team
cfb_team <- cfbd_game_team_stats(2021, team = "LSU")

#write player data to nested years list
cfb_player <- list()
cfb_player_list <- list()

for (j in 2014:2021) {
  
  for (i in 1:15) {
    cfb_player_list[[i]] <- cfbd_game_player_stats(j, week = i)
    cfb_player_list[[i]]$year <- j
    cfb_player_list[[i]]$week <- i
    
    print(paste("Year:",j," Week: ",i, "CFB Player Data written"))
  }
  
  cfb_player[[j-2013]] <- cfb_player_list
}

#bind rows of list
bind <- bind_rows(cfb_player)

bind <- separate(bind, col = c_att, into = c("comp", "attempts"), sep = "/")

write.csv(df, file = "./training_data/pbp_data/cfb_player.csv")
  
df <- as.data.frame(bind_rows(cfb_player))

df <- apply(df,2,as.character)

write.csv(df, file = "./training_data/pbp_data/cfb_player.csv")