#lets build cfb lineups

# 0.0 load packages -------------------------------------------------------

suppressMessages({
  library(tidyverse) #metapackage
  library(ggrepel) #automatically position non-overlapping text labels
  library(lubridate) #make dealing with dates a little easier
  library(utils) #R utility functions
  library(lpSolve) #solver for linear / integer programs
  library(stats) #R statistical functions
  library(binr) #cut numeric values into evenly distributed groups
})

# 1.0 lineup optimizer ----------------------------------------------------

dks$one <- 1

dks_opt <- dks %>% select(Name, Salary, fpts, one) %>% drop_na()

#Optimal Lineup Table
optimal_list <- list()
lineup_names <- NULL
dlist <- list()
dlist_df <- data.frame()

for (i in 1:entries) {
  
  optimal <- lp(direction = "max", 
                objective.in = dks_opt$fpts, 
                rbind(dks_opt$Salary, dks_opt$fpts, dks_opt$one), 
                c("<=", ">=", "="), 
                c("50000", "49500", "6"),
                binary.vec = c(1:dim(dks_opt)[1]))
  
  optimal_lineup <- dks_opt[c(which(optimal$solution == 1)),]
  optimal_lineup <- optimal_lineup %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum), 
                        across(where(is.character), ~"")))
  
  odds_limit <- optimal_lineup$odds_close[7]
  
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
  
  golfers2 <- golfers2[,-c(19:22)]
}

optimal_table <- do.call("rbind", optimal_list)

#Lineup Check
entries_wp <- data.frame()
for (m in 1:entries) {
  entries_wp[m,1] <- optimal_table$odds_close[(m*7)]
}
names(entries_wp) <- "Lineup WP"

#Check ownership
ownership_table <- golfers %>% 
  select(Name, ID, Salary, fpts, proj_own ,odds_open, odds_close, odds_delta, odds_delta_per, odds_rank, own_change)

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

#entries_upload <- entries_upload[,-c(7)]
#names(entries_upload) <- c("G","G","G","G","G","G")
#entries_upload <- unique(entries_upload)

#Write
#write.csv(golfers, file = "golfers.csv")
#write.csv(ownership_table, file = "ownership_table.csv")
#write.csv(entries_upload, file = "entries_upload.csv")
