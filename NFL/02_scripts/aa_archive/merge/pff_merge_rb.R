# Condense pff rb data into 1 object

# Load packages
library(tidyverse, warn.conflicts = F) #metapackage
library(glue)
library(tictoc)

# clock start
tic()

# Get training data folder
folder <- list.dirs()[which(list.dirs() == "./01_data/training_data/2014")]
year <- substring(folder, nchar(folder)-3, nchar(folder))

# Get column names from week 1 files
rushing_summary <- read.csv(paste0(folder,"/rushing_summary (1).csv"))
rushing_summary_cols <- colnames(rushing_summary)

# Define folder indices
folder_index_start <- which(list.dirs() == "./01_data/training_data/2014")
folder_index_end <- folder_index_start + (year(Sys.Date())- 1 - 2014)
# minus 1 in offseason, remove for in season

# create list of pff rb dataframes
df_rb <- map(folder_index_start:folder_index_end, function(folder_index) {
  
  folder <- list.dirs()[folder_index]
  year <- substr(folder, nchar(folder)-3, nchar(folder))
  
  map(1:17, function(folder_week){
    print(glue("Loading Year: {year} Week: {folder_week}"))
    
    rushing_summary <- read.csv(glue("{folder}/rushing_summary ({folder_week}).csv")) %>% 
      select(all_of(rushing_summary_cols)) %>% 
      mutate(year = year, 
             week = folder_week)
    })
}
              )

# bind the list to a dataframe
rbs <- map(df_rb, bind_rows)
rbs <- bind_rows(rbs)

saveRDS(rbs, file = "./01_data/training_data/position_groups/rbs.RData")

# clock end
toc()