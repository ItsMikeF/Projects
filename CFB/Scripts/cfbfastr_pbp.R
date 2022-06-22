#load packages
suppressMessages({
  library(tidyverse)
  library(cfbfastR)
  })

#load data
#usethis::edit_r_environ()
cfbd_key("lDpblntVUpUafh2geyo/tqM6QMErJgcl8FnpnugstY7wBLPmllehGfB/EYknH0VV")

#team
cfb_team <- cfbd_game_team_stats(2019, team = "LSU")

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

#write nested years list to csv
for (j in 2014:2021) {
  
  for(i in 1:15){
    
    write.table(cfb_player[[j-2013]][[i]], 
                file = "./training_data/pbp_data/cfb_player.csv", sep = ",", 
                col.names = !file.exists("./training_data/pbp_data/cfb_player.csv"), 
                append = T, 
                row.names = F)
    
    print(paste("Year:", j,"Week:", i, "CFB Player Data written"))
    
  }
}

#bind rows of list
bind <- bind_rows(cfb_player)
bind <- as.data.frame(bind)

df <- apply(bind,2,as.character)

write.csv(df, file = "./training_data/pbp_data/cfb_player.csv")
