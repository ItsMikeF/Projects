#load packages
suppressMessages({
  library(tidyverse) #metapackage
  library(ggrepel) #automatically position non-overlapping text labels
  library(lubridate) #make dealing with dates a little easier
  library(utils) #R utility functions
  library(lpSolve) #solver for linear / integer programs
  library(stats) #R statistical functions
  library(binr) #cut numeric values into evenly distributed groups
})

#Inputs
entries <- 20
salary_filter <- 6500

#Import CSVs
golfers <- read.csv(paste0("./Results/golfers_",entries,".csv"))

#Optimal Lineup
optimal <- lp(direction = "max", 
              objective.in = golfers$course_fit, 
              rbind(golfers$Salary, golfers$Salary, golfers$one), 
              c("<=", ">=", "="), 
              c("50000", "49500", "6"), 
              binary.vec = c(1:dim(golfers)[1]))

optimal_lineup <- golfers[c(which(optimal$solution == 1)),]
optimal_lineup <- optimal_lineup %>% 
  bind_rows(summarise(.,
                      across(where(is.numeric), sum), 
                      across(where(is.character), ~"")))

#Ownership table
golfer_own <- matrix(nrow = dim(golfers)[1], ncol = 3)
golfer_own <- golfers$Name
golfer_own <- tibble(golfer_own)

own_multiplier <- 100 / entries

#Salary Filter

golfers2 <- golfers %>% 
  filter(Salary >= salary_filter)

#Optimal Lineup Table
optimal_list <- list()
fpts_limit <- optimal$objval
lineup_names <- NULL
dlist <- list()
dlist_df <- data.frame()

for (i in 1:entries) {
  
  optimal <- lp(direction = "max", 
                objective.in = golfers2$course_fit, 
                rbind(golfers2$Salary, golfers2$Salary, golfers2$one, golfers2$course_fit), 
                c("<=", ">=", "=", "<"), 
                c("50000", "49500", "6", fpts_limit-.01),
                binary.vec = c(1:dim(golfers2)[1]))
  
  optimal_lineup <- golfers2[c(which(optimal$solution == 1)),]
  optimal_lineup <- optimal_lineup %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum), 
                        across(where(is.character), ~"")))
  
  fpts_limit <- optimal_lineup$course_fit[7]
  
  optimal_list[[i]] <- optimal_lineup
  
  new_names<- optimal_lineup$Name[1:6]
  lineup_names <- c(lineup_names, new_names)
  uniq <- unique(lineup_names)
  
  for (k in uniq) {
    cat(k, ': ', sum(lineup_names == k), '\n', sep='')
    dlist[[k]] <- data.frame(i, sum(lineup_names == k)/entries)
    dlist[[k]][3] <- golfers2$adj_own[which(golfers2$Name == k)]/100
  }
  
  for (l in 1:length(dlist)) {
    dlist_df[l,1] <- names(dlist)[l]
    dlist_df[l,2] <- dlist[[l]][[1]]
    dlist_df[l,3] <- dlist[[l]][[2]]
    dlist_df[l,4] <- dlist[[l]][[3]]
  }
  
  golfers2 <- golfers2 %>% left_join(dlist_df, by = c("Name" = "V1"))
  
  golfers2$filter <- if_else(golfers2$V3 >= (golfers2$adj_own/100), 1,0)
  golfers2[is.na(golfers2)] <- 0
  
  golfers2 <- golfers2 %>% filter(filter == 0)
  
  golfers2 <- golfers2[,-c(55:58)]
}

optimal_table <- do.call("rbind", optimal_list)

#Lineup Check
entries_wp <- data.frame()
for (m in 1:entries) {
  entries_wp[m,1] <- optimal_table$course_fit[(m*7)]
}
names(entries_wp) <- "Lineup WP"

#Check ownership
ownership_table <- golfers %>% 
  select(Name, ID, Salary, ceil, fpts, course_fit, total_points, final_prediction, win, residuals, course_fit, proj_own_avg, own_change)

for(i in 1:dim(golfers)[1]){
  ownership_table$own[i] <- sum(str_count(optimal_table$Name, ownership_table$Name[i])) / entries
}

ownership_table %>% 
  arrange(-own) %>% 
  view(title = "Golfers Own")

#Create Entries CSV
entries_upload <- tibble(.rows = entries)

for (i in 1:entries) {
  for (j in 1:7) {
    entries_upload[i,j] <- optimal_table$ID[(i+(j-1) + (6*(i-1)))]
  }
}

entries_upload <- entries_upload[,-c(7)]
names(entries_upload) <- c("G","G","G","G","G","G")
entries_upload <- unique(entries_upload)

#Write
write.csv(ownership_table, file = paste0("./Results/ownership_table_",entries,".csv"))
write.csv(entries_upload, file = paste0("./Results/entries_upload_",entries,".csv"))
