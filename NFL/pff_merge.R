#load packages
library(tidyverse, warn.conflicts = F) #metapackage

#set year folder
folder <- list.dirs()[50]
year <- substring(folder, nchar(folder)-3, nchar(folder))

#obtain column names from week 1 files
passing_summary <- read.csv(paste0(folder,"/passing_summary (1).csv"))
passing_summary_cols <- colnames(passing_summary)

passing_pressure <- read.csv(paste0(folder,"/passing_pressure (1).csv"))
passing_pressure_cols <- colnames(passing_pressure)

passing_concept <- read.csv(paste0(folder,"/passing_concept (1).csv"))
passing_concept_cols <- colnames(passing_concept)

passing_depth <- read.csv(paste0(folder,"/passing_depth (1).csv"))
passing_depth_cols <- colnames(passing_depth)

time_in_pocket <- read.csv(paste0(folder,"/time_in_pocket (1).csv"))
time_in_pocket_cols <- colnames(time_in_pocket)

#table of number of columns
dim_table <- data.frame()

#qb list
qbs_list <- list()

#https://statisticsglobe.com/create-nested-list-in-r

#loop for all years
for (j in 50:length(list.dirs())) {
  
  folder <- list.dirs()[j]
  year <- substring(folder, nchar(folder)-3, nchar(folder))
  
  #write csvs to list
  for (i in 1:17) {
    print(paste("Year:",year, "week:", i))
    passing_summary <- read.csv(paste0(folder,"/passing_summary (", i,").csv")) %>% 
      select(passing_summary_cols)
    dim_table[1,i] <- dim(passing_summary)[2]
    
    passing_pressure <- read.csv(paste0(folder,"/passing_pressure (", i,").csv")) %>% 
      select(passing_pressure_cols)
    dim_table[2,i] <- dim(passing_pressure)[2]
    
    passing_concept <- read.csv(paste0(folder,"/passing_concept (", i,").csv")) %>% 
      select(passing_concept_cols)
    dim_table[3,i] <- dim(passing_concept)[2]
    
    passing_depth <- read.csv(paste0(folder,"/passing_depth (", i,").csv")) %>% 
      select(passing_depth_cols)
    dim_table[4,i] <- dim(passing_depth)[2]
    
    time_in_pocket <- read.csv(paste0(folder,"/time_in_pocket (", i,").csv")) %>% 
      select(time_in_pocket_cols)
    dim_table[5,i] <- dim(time_in_pocket)[2]
    
    qbs_list[[j-49]][[i]] <- list(passing_summary, passing_pressure, passing_concept, passing_depth, time_in_pocket) %>% 
      reduce(left_join, by = "player_id")
    
    qbs_list[[j-49]][[i]]$year <- year
    qbs_list[[j-49]][[i]]$week <- i
    print(paste("Year:",year, "Week:", i, ", # of Columns:",dim(qbs_list[[j-49]][[i]])[2]))
  }
  
  rownames(dim_table) <- c("passing_summary", "passing_pressure", "passing_concept", "passing_depth", "time_in_pocket")
  
}

#write list to csv
for(i in 1:length(qbs_list)){
  write.table(tibble(qbs_list[[i]]), file = paste0(year,"_qb_summary.csv"), sep = ",", col.names = !file.exists("2014_qb_summary.csv"), append = T, row.names = F)
  print(paste("Week", i))
}
