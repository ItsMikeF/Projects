# Merge all PFF

#load packages
library(tidyverse, warn.conflicts = F) #metapackage
library(glue)
library(tictoc)

# Get training data folder
folder <- list.dirs()[which(list.dirs() == "./01_data/training_data/2014")]
year <- substring(folder, nchar(folder)-3, nchar(folder))

#obtain column names from week 1 files
offense_blocking <- read.csv(paste0(folder,"/offense_blocking (1).csv"))
offense_blocking_cols <- colnames(offense_blocking)

offense_pass_blocking <- read.csv(paste0(folder,"/offense_pass_blocking (1).csv")) 
offense_pass_blocking_cols <- colnames(offense_pass_blocking)

offense_run_blockng <- read.csv(paste0(folder,"/offense_run_blockng (1).csv"))
offense_run_blockng_cols <- colnames(offense_run_blockng)

#table of number of columns
dim_table <- data.frame()

#ol list
ols <- list()
ols_list <- list()

# Define folder indices
folder_index_start <- which(list.dirs() == "./01_data/training_data/2014")
folder_index_end <- folder_index_start + (year(Sys.Date())- 1 - 2014)
# minus 1 in offseason, remove for in season

# loop for all years into list
for (j in folder_index_start:folder_index_end) {
  
  folder <- list.dirs()[j]
  year <- substring(folder, nchar(folder)-3, nchar(folder))
  
  #write csvs to list
  for (i in 1:17) {
    print(paste("Year:",year, "week:", i))
    
    offense_blocking <- read.csv(paste0(folder,"/offense_blocking (", i,").csv")) %>% 
      select(offense_blocking_cols)
    dim_table[1,i] <- dim(offense_blocking)[2]
    
    offense_pass_blocking <- read.csv(paste0(folder,"/offense_pass_blocking (", i,").csv")) %>% 
      select(offense_pass_blocking_cols[c(2,7:length(offense_pass_blocking_cols))]) %>% 
      select(-c("grades_pass_block", "grades_pass_block", "hits_allowed", 
                "hurries_allowed", "non_spike_pass_block", 
                "non_spike_pass_block_percentage", "pass_block_percent", 
                "pbe", "penalties", "pressures_allowed", "sacks_allowed", 
                "snap_counts_pass_block", "snap_counts_pass_play"))
    dim_table[2,i] <- dim(offense_pass_blocking)[2]
    
    offense_run_blockng <- read.csv(paste0(folder,"/offense_run_blockng (", i,").csv")) %>% 
      select(offense_run_blockng_cols[c(2,6:length(offense_run_blockng_cols))]) %>% 
      select(-grades_run_block)
      
    dim_table[3,i] <- dim(offense_run_blockng)[2]
    
    ols_list[[i]] <- list(offense_blocking, offense_pass_blocking, offense_run_blockng) %>% 
      reduce(left_join, by = "player_id")
    
    ols_list[[i]]$year <- year
    ols_list[[i]]$week <- i
    print(paste("Year:",year, "Week:", i, ", # of Columns:",dim(ols_list[[i]])[2]))
  }
  
  rownames(dim_table) <- c("offense_blocking", "offense_pass_blocking", "offense_run_blockng")
  
  ols[[j-folder_index_start+1]] <- ols_list
  
}

# bind list to a dataframe
ol <- map(ols, bind_rows)
ol <- bind_rows(ol)

# save data 
save(ol, file = "./01_data/training_data/position_groups/ol.RData")
load("./01_data/training_data/position_groups/ol.RData")

team_ol <- ol %>% 
  group_by(team_name, year, week) %>% 
  summarise(grades_pass_block = weighted.mean(grades_pass_block, snap_counts_pass_block)) %>% 
  ungroup() %>% 
  mutate(off_join = paste0(year, week, team_name))
