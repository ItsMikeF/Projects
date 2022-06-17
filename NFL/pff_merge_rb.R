#load packages
library(tidyverse, warn.conflicts = F) #metapackage

#set year folder
folder <- list.dirs()[50]
year <- substring(folder, nchar(folder)-3, nchar(folder))

#obtain column names from week 1 files
rushing_summary <- read.csv(paste0(folder,"/rushing_summary (1).csv"))
rushing_summary_cols <- colnames(rushing_summary)

#table of number of columns
dim_table <- data.frame()

#wr list
rbs <- list()
rbs_list <- list()

#loop for all years into list
for (j in 50:length(list.dirs())) {
  
  folder <- list.dirs()[j]
  year <- substring(folder, nchar(folder)-3, nchar(folder))
  
  #write csvs to list
  for (i in 1:17) {
    print(paste("Year:",year, "week:", i))
    
    rushing_summary <- read.csv(paste0(folder,"/rushing_summary (", i,").csv")) %>% 
      select(rushing_summary_cols)
    dim_table[1,i] <- dim(rushing_summary)[2]
    
    rbs_list[[i]] <- list(rushing_summary) %>% 
      reduce(left_join, by = "player_id")
    
    rbs_list[[i]]$year <- year
    rbs_list[[i]]$week <- i
    print(paste("Year:",year, "Week:", i, ", # of Columns:",dim(rbs_list[[i]])[2]))
  }
  
  rownames(dim_table) <- c("rushing_summary")
  
  rbs[[j-49]] <- rbs_list
  
}

#write nested years list to csv
for (j in 50:length(list.dirs())) {
  
  folder <- list.dirs()[j]
  year <- substring(folder, nchar(folder)-3, nchar(folder))
  
  for(i in 1:17){
    
    write.table(tibble(rbs[[j-49]][[i]]), file = "rbs.csv", sep = ",", col.names = !file.exists("rbs.csv"), append = T, row.names = F)
    print(paste("Year:", year,"Week:", i))
    
  }
}
