# merge all pff qb data

#load packages
suppressMessages({
  library(dplyr)
  library(glue)
})

# Get training data folder
folder <- list.dirs()[which(list.dirs() == "./01_data/training_data/2014")]
year <- substring(folder, nchar(folder)-3, nchar(folder))

# Define folder indices
folder_index_start <- which(list.dirs() == "./01_data/training_data/2014")
folder_index_end <- folder_index_start + (year(Sys.Date())- 1 - 2014)
# minus 1 in offseason, remove for in season

pbe <- list()
pbe_list <- list()

# Map pbe files to a list
pbe <- map(folder_index_start:folder_index_end, function(index){
  folder <- list.dirs()[index]
  year <- substring(folder, nchar(folder)-3, nchar(folder))
  
  map(1:17, function(game_week){
    print(paste("Year:",year, "week:", game_week))
    
    read.csv(glue("{folder}/line_pass_blocking_efficiency ({game_week}).csv")) %>% 
      mutate(year = year, week = game_week)
    
  })
})

# bind rows of the list
pbe <- map(pbe, bind_rows)
pbe <- bind_rows(pbe)

# save RData file
saveRDS(pbe, file = "./01_data/training_data/position_groups/pbe.RData")
