# merge all pff wr files into 1 

#load packages
library(tidyverse, warn.conflicts = F) #metapackage

# Get training data folder
folder <- list.dirs()[which(list.dirs() == "./01_data/training_data/2014")]
year <- substring(folder, nchar(folder)-3, nchar(folder))

#obtain column names from week 1 files
receiving_summary <- read.csv(paste0(folder,"/receiving_summary (1).csv"))
receiving_summary_cols <- colnames(receiving_summary)

receiving_scheme <- read.csv(paste0(folder,"/receiving_scheme (1).csv"))
receiving_scheme_cols <- colnames(receiving_scheme)

receiving_concept <- read.csv(paste0(folder,"/receiving_concept (1).csv"))
receiving_concept_cols <- colnames(receiving_concept)

receiving_depth <- read.csv(paste0(folder,"/receiving_depth (1).csv"))
receiving_depth_cols <- colnames(receiving_depth)

#table of number of columns
dim_table <- data.frame()

#wr list
wrs <- list()
wrs_list <- list()

# Define folder indices
folder_index_start <- which(list.dirs() == "./01_data/training_data/2014")
folder_index_end <- folder_index_start + (year(Sys.Date())- 1 - 2014)
# minus 1 in offseason, remove for in season

#loop for all years into list
for (j in folder_index_start:folder_index_end) {
  
  folder <- list.dirs()[j]
  year <- substring(folder, nchar(folder)-3, nchar(folder))
  
  #write csvs to list
  for (i in 1:17) {
    print(paste("Year:",year, "week:", i))
    
    receiving_summary <- read.csv(paste0(folder,"/receiving_summary (", i,").csv")) %>% 
      select(receiving_summary_cols)
    dim_table[1,i] <- dim(receiving_summary)[2]
    
    receiving_scheme <- read.csv(paste0(folder,"/receiving_scheme (", i,").csv")) %>% 
      select(receiving_scheme_cols[c(2,7:length(receiving_scheme_cols))])
    dim_table[2,i] <- dim(receiving_scheme)[2]
    
    receiving_concept <- read.csv(paste0(folder,"/receiving_concept (", i,").csv")) %>% 
      select(receiving_concept_cols[c(2,6:length(receiving_concept_cols))])
    dim_table[3,i] <- dim(receiving_concept)[2]
    
    receiving_depth <- read.csv(paste0(folder,"/receiving_depth (", i,").csv")) %>% 
      select(receiving_depth_cols[c(2,8:length(receiving_depth_cols))])
    dim_table[4,i] <- dim(receiving_depth)[2]
    
    wrs_list[[i]] <- list(receiving_summary, receiving_scheme, receiving_concept, receiving_depth) %>% 
      reduce(left_join, by = "player_id")
    
    wrs_list[[i]]$year <- year
    wrs_list[[i]]$week <- i
    print(paste("Year:",year, "Week:", i, ", # of Columns:",dim(wrs_list[[i]])[2]))
  }
  
  rownames(dim_table) <- c("receiving_summary", "receiving_scheme", "receiving_concept", "receiving_depth")
  
  wrs[[j-folder_index_start+1]] <- wrs_list
  
}

wr <- map(wrs, bind_rows)
wr <- bind_rows(wr)

saveRDS(wr, file = "./01_data/training_data/position_groups/wr.RData")
