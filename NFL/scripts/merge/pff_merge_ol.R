#load packages
library(tidyverse, warn.conflicts = F) #metapackage

#set year folder
folder <- list.dirs()[which(list.dirs() == "./Training_Data/2014")]
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

#loop for all years into list
for (j in which(list.dirs() == "./Training_Data/2014"):(length(list.dirs())-1)) {
  
  folder <- list.dirs()[j]
  year <- substring(folder, nchar(folder)-3, nchar(folder))
  
  #write csvs to list
  for (i in 1:17) {
    print(paste("Year:",year, "week:", i))
    
    offense_blocking <- read.csv(paste0(folder,"/offense_blocking (", i,").csv")) %>% 
      select(offense_blocking_cols)
    dim_table[1,i] <- dim(offense_blocking)[2]
    
    offense_pass_blocking <- read.csv(paste0(folder,"/offense_pass_blocking (", i,").csv")) %>% 
      select(offense_pass_blocking_cols[c(2,7:length(offense_pass_blocking_cols))])
    dim_table[2,i] <- dim(offense_pass_blocking)[2]
    
    offense_run_blockng <- read.csv(paste0(folder,"/offense_run_blockng (", i,").csv")) %>% 
      select(offense_run_blockng_cols[c(2,6:length(offense_run_blockng_cols))])
    dim_table[3,i] <- dim(offense_run_blockng)[2]
    
    ols_list[[i]] <- list(offense_blocking, offense_pass_blocking, offense_run_blockng) %>% 
      reduce(left_join, by = "player_id")
    
    ols_list[[i]]$year <- year
    ols_list[[i]]$week <- i
    print(paste("Year:",year, "Week:", i, ", # of Columns:",dim(ols_list[[i]])[2]))
  }
  
  rownames(dim_table) <- c("offense_blocking", "offense_pass_blocking", "offense_run_blockng")
  
  ols[[j-(which(list.dirs() == "./Training_Data/2014")-1)]] <- ols_list
  
}

#write nested years list to csv
for (j in which(list.dirs() == "./Training_Data/2014"):(length(list.dirs())-1)) {
  
  folder <- list.dirs()[j]
  year <- substring(folder, nchar(folder)-3, nchar(folder))
  
  for(i in 1:17){
    
    write.table(tibble(ols[[j-(length(list.dirs())-1-9)]][[i]]), file = "ols.csv", sep = ",", col.names = !file.exists("ols.csv"), append = T, row.names = F)
    print(paste("Year:", year,"Week:", i))
    
  }
}
