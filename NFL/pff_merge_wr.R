#load packages
library(tidyverse, warn.conflicts = F) #metapackage

#set year folder
folder <- list.dirs()[50]
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

#loop for all years into list
for (j in 50:length(list.dirs())) {
  
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
  
  wrs[[j-49]] <- wrs_list
  
}

#write nested years list to csv
for (j in 50:length(list.dirs())) {
  
  folder <- list.dirs()[j]
  year <- substring(folder, nchar(folder)-3, nchar(folder))
  
  for(i in 1:17){
    
    write.table(tibble(wrs[[j-49]][[i]]), file = "wrs.csv", sep = ",", col.names = !file.exists("wrs.csv"), append = T, row.names = F)
    print(paste("Year:", year,"Week:", i))
    
  }
}
