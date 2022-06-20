#load packages
library(tidyverse, warn.conflicts = F) #metapackage

#set year folder
folder <- list.dirs()[50]
year <- substring(folder, nchar(folder)-3, nchar(folder))

#obtain column names from week 1 files
defense_summary <- read.csv(paste0(folder,"/defense_summary (1).csv"))
defense_summary_cols <- colnames(defense_summary)

pass_rush_summary <- read.csv(paste0(folder,"/pass_rush_summary (1).csv"))
pass_rush_summary_cols <- colnames(pass_rush_summary[c(2,6:length(pass_rush_summary))])

run_defense_summary <- read.csv(paste0(folder,"/run_defense_summary (1).csv"))
run_defense_summary_cols <- colnames(run_defense_summary[c(2,6:length(run_defense_summary))])

defense_coverage_summary <- read.csv(paste0(folder,"/defense_coverage_summary (1).csv"))
defense_coverage_summary_cols <- colnames(defense_coverage_summary[c(2,6:length(defense_coverage_summary))])

defense_coverage_scheme <- read.csv(paste0(folder,"/defense_coverage_scheme (1).csv"))
defense_coverage_scheme_cols <- colnames(defense_coverage_scheme[c(2,6:length(defense_coverage_scheme))])

slot_coverage <- read.csv(paste0(folder,"/slot_coverage (1).csv"))
slot_coverage_cols <- colnames(slot_coverage[c(2,6:length(slot_coverage))])

pass_rush_productivity <- read.csv(paste0(folder,"/pass_rush_productivity (1).csv"))
pass_rush_productivity_cols <- colnames(pass_rush_productivity[c(2,6:length(pass_rush_productivity))])

#table of number of columns
dim_table <- data.frame()

#wr list
def <- list()
def_list <- list()

#loop for all years into list
for (j in 50:length(list.dirs())) {
  
  folder <- list.dirs()[j]
  year <- substring(folder, nchar(folder)-3, nchar(folder))
  
  #write csvs to list
  for (i in 1:17) {
    print(paste("Year:",year, "week:", i))
    
    defense_summary <- read.csv(paste0(folder,"/defense_summary (", i,").csv")) %>% 
      select(defense_summary_cols)
    dim_table[1,i] <- dim(defense_summary)[2]
    
    pass_rush_summary <- read.csv(paste0(folder,"/pass_rush_summary (", i,").csv")) %>% 
      select(pass_rush_summary_cols)
    dim_table[2,i] <- dim(pass_rush_summary)[2]
    
    run_defense_summary <- read.csv(paste0(folder,"/run_defense_summary (", i,").csv")) %>% 
      select(run_defense_summary_cols)
    dim_table[3,i] <- dim(run_defense_summary)[2]
    
    defense_coverage_summary <- read.csv(paste0(folder,"/defense_coverage_summary (", i,").csv")) %>% 
      select(defense_coverage_summary_cols)
    dim_table[4,i] <- dim(defense_coverage_summary)[2]
    
    defense_coverage_scheme <- read.csv(paste0(folder,"/defense_coverage_scheme (", i,").csv")) %>% 
      select(defense_coverage_scheme_cols)
    dim_table[5,i] <- dim(defense_coverage_scheme)[2]
    
    slot_coverage <- read.csv(paste0(folder,"/slot_coverage (", i,").csv")) %>% 
      select(slot_coverage_cols)
    dim_table[6,i] <- dim(slot_coverage)[2]
    
    pass_rush_productivity <- read.csv(paste0(folder,"/pass_rush_productivity (", i,").csv")) %>% 
      select(pass_rush_productivity_cols)
    dim_table[7,i] <- dim(pass_rush_productivity)[2]
    
    def_list[[i]] <- list(defense_summary, pass_rush_summary, run_defense_summary, defense_coverage_summary, defense_coverage_scheme, slot_coverage, pass_rush_productivity) %>% 
      reduce(left_join, by = "player_id")
    
    def_list[[i]]$year <- year
    def_list[[i]]$week <- i
    print(paste("Year:",year, "Week:", i, ", # of Columns:",dim(def_list[[i]])[2]))
  }
  
  rownames(dim_table) <- c("defense_summary", "pass_rush_summary", "run_defense_summary", "defense_coverage_summary", "defense_coverage_scheme", "slot_coverage", "pass_rush_productivity")
  
  def[[j-49]] <- def_list
  
}

#write nested years list to csv
for (j in 50:length(list.dirs())) {
  
  folder <- list.dirs()[j]
  year <- substring(folder, nchar(folder)-3, nchar(folder))
  
  for(i in 1:17){
    
    write.table(tibble(def[[j-49]][[i]]), file = "def.csv", sep = ",", col.names = !file.exists("def.csv"), append = T, row.names = F)
    print(paste("Year:", year,"Week:", i))
    
  }
}
