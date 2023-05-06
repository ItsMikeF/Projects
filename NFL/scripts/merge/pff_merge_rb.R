#load packages
library(tidyverse, warn.conflicts = F) #metapackage

#set year folder
folder <- list.dirs()[which(list.dirs() == "./Training_Data/2014")]
year <- substring(folder, nchar(folder)-3, nchar(folder))

#obtain column names from week 1 files
rushing_summary <- read.csv(paste0(folder,"/rushing_summary (1).csv"))
rushing_summary_cols <- colnames(rushing_summary)

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
rbs <- list()
rbs_list <- list()

#loop for all years into list
for (j in which(list.dirs() == "./Training_Data/2014"):(length(list.dirs())-1)) {
  
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
  
  rbs[[j-(which(list.dirs() == "./Training_Data/2014")-1)]] <- rbs_list
  
}

#write nested years list to csv
for (j in which(list.dirs() == "./Training_Data/2014"):(length(list.dirs())-1)) {
  
  folder <- list.dirs()[j]
  year <- substring(folder, nchar(folder)-3, nchar(folder))
  
  for(i in 1:17){
    
    write.table(tibble(rbs[[j-126]][[i]]), file = "rbs.csv", sep = ",", col.names = !file.exists("rbs.csv"), append = T, row.names = F)
    print(paste("Year:", year,"Week:", i))
    
  }
}
