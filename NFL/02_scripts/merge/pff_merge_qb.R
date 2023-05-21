#load packages
suppressMessages({library(tidyverse)}) 

#loop values
start <- which(list.dirs() == "./Training_Data/2014")
stop <- length(list.dirs())-1

#set year folder
folder <- list.dirs()[start]
year <- substring(folder, nchar(folder)-3, nchar(folder))

#obtain column names from week 1 files
#passing summary
passing_summary <- read.csv(paste0(folder,"/passing_summary (1).csv"))
passing_summary_cols <- colnames(passing_summary)

#passing pressure
passing_pressure <- read.csv(paste0(folder,"/passing_pressure (1).csv"))
passing_pressure_cols <- colnames(passing_pressure)
passing_pressure_cols <- passing_pressure_cols[
  -which(passing_pressure_cols %in% c("blitz_grades_pass_route",
                                      "no_blitz_grades_pass_route",
                                      "no_pressure_grades_pass_route",
                                      "pressure_grades_pass_route"))
  ]

#passing concept
passing_concept <- read.csv(paste0(folder,"/passing_concept (1).csv"))
passing_concept_cols <- colnames(passing_concept)
passing_concept_cols <- passing_concept_cols[
  -which(passing_concept_cols %in% c("no_screen_grades_pass_route",
                                     "npa_grades_pass_route",
                                     "pa_grades_pass_route", 
                                     "screen_grades_pass_route"))
  ]

#passing depth
passing_depth <- read.csv(paste0(folder,"/passing_depth (1).csv"))
passing_depth_cols <- colnames(passing_depth)

#time in pocket
time_in_pocket <- read.csv(paste0(folder,"/time_in_pocket (1).csv"))
time_in_pocket_cols <- colnames(time_in_pocket)
time_in_pocket_cols <- time_in_pocket_cols[
  -which(time_in_pocket_cols %in% c("less_grades_pass_route",
                                    "more_grades_pass_route"))
  ]

pbe <- read.csv(paste0(folder,"/line_pass_blocking_efficiency (1).csv"))

#table of number of columns
dim_table <- data.frame()

#qb list
qbs <- list()
qbs_list <- list()

pbe <- list()
pbe_list <- list()

#loop for all years into list
for (j in start:stop) {
  
  folder <- list.dirs()[j]
  year <- substring(folder, nchar(folder)-3, nchar(folder))
  
  #write csvs to list
  for (i in 1:17) {
    print(paste("Year:",year, "week:", i))
    
    passing_summary <- read.csv(paste0(folder,"/passing_summary (", i,").csv")) %>% 
      select(passing_summary_cols)
    dim_table[1,i] <- dim(passing_summary)[2]
    
    passing_pressure <- read.csv(paste0(folder,"/passing_pressure (", i,").csv")) %>% 
      select(passing_pressure_cols[c(2,7:length(passing_pressure_cols))])
    dim_table[2,i] <- dim(passing_pressure)[2]
    
    passing_concept <- read.csv(paste0(folder,"/passing_concept (", i,").csv")) %>% 
      select(passing_concept_cols[c(2,6:length(passing_concept_cols))])
    dim_table[3,i] <- dim(passing_concept)[2]
    
    passing_depth <- read.csv(paste0(folder,"/passing_depth (", i,").csv")) %>% 
      select(passing_depth_cols[c(2,8:length(passing_depth_cols))])
    dim_table[4,i] <- dim(passing_depth)[2]
    
    time_in_pocket <- read.csv(paste0(folder,"/time_in_pocket (", i,").csv")) %>% 
      select(time_in_pocket_cols[c(2,6:length(time_in_pocket_cols))])
    dim_table[5,i] <- dim(time_in_pocket)[2]
    
    qbs_list[[i]] <- list(passing_summary, passing_pressure, passing_concept, passing_depth, time_in_pocket) %>% 
      reduce(left_join, by = "player_id")
    
    qbs_list[[i]]$year <- year
    qbs_list[[i]]$week <- i
    print(paste("Year:",year, "Week:", i, ", # of Columns:",dim(qbs_list[[i]])[2]))
  }
  
  rownames(dim_table) <- c("passing_summary", "passing_pressure", "passing_concept", "passing_depth", "time_in_pocket")
  
  qbs[[j-(start-1)]] <- qbs_list

}

#write nested qb years list to csv
for (j in start:stop) {
  
  folder <- list.dirs()[j]
  year <- substring(folder, nchar(folder)-3, nchar(folder))
  
  for(i in 1:17){
    
    write.table(tibble(qbs[[j-(start-1)]][[i]]), 
                file = "./Training_Data/position_groups/qbs.csv", sep = ",", 
                col.names = !file.exists("./Training Data/position_groups/qbs.csv"), 
                append = T, row.names = F)
    print(paste("Year:", year,"Week:", i))
    
  }
}

#write pbe list
for (j in start:stop) {
  
  folder <- list.dirs()[j]
  year <- substring(folder, nchar(folder)-3, nchar(folder))
  
  #write csvs to list
  for (i in 1:17) {
    print(paste("Year:",year, "week:", i))
    
    pbe_list[[i]] <- read.csv(paste0(folder,"/line_pass_blocking_efficiency (", i,").csv"))
    
    pbe_list[[i]]$year <- year
    pbe_list[[i]]$week <- i
    print(paste("Year:",year, "Week:", i, ", # of Columns:",dim(pbe_list[[i]])[2]))
  }
  
  pbe[[j-(start-1)]] <- pbe_list
  
}

#delete the file before writing
if (file.exists("./Training_Data/position_groups/pbe.csv")) {
  unlink("./Training_Data/position_groups/pbe.csv")
  cat("The file is deleted.")
} else {
  cat("The file was not found.")
}

#write nested pbe years list to csv
for (j in start:stop) {
  
  folder <- list.dirs()[j]
  year <- substring(folder, nchar(folder)-3, nchar(folder))
  
  for(i in 1:17){
    
    write.table(tibble(pbe[[j-(start-1)]][[i]]), 
                file = "./Training_Data/position_groups/pbe.csv", sep = ",", 
                col.names = !file.exists("./Training_Data/position_groups/pbe.csv"), 
                append = T, row.names = F)
    print(paste("Year:", year,"Week:", i))
    
  }
}
