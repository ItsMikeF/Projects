### List with lineups ###

tic("Generating lineups")
lineup_count <- 1*(10^2)
lineups <- list()
pb <- txtProgressBar(1,lineup_count, style = 3)

for(i in 1:lineup_count) {
  lineup <- tibble(sample(golfers$Name, 6, replace = F))
  names(lineup)[1] <- "Name"
  
  lineup <- lineup %>% 
    left_join(golfer_table, by = c("Name")) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(where(is.character), ~"")))
  
  lineups[[i]] <- lineup
  setTxtProgressBar(pb, i)
}

#if_else(10 %% i == 0, print(paste("lineup", i, "created")), NULL)
#setTxtProgressBar()

lineup_table <- tibble(1:lineup_count)
names(lineup_table)[1] <- "Lineup"

for(i in 1:lineup_count){
  lineup_table[i,2] <- lineups[[i]][7,2]
  lineup_table[i,3] <- lineups[[i]][7,3]
  lineup_table[i,4] <- lineups[[i]][7,4]
  lineup_table[i,5] <- lineups[[i]][7,5]
}

lineup_table <- lineup_table %>% 
  filter(Salary < 50000) %>% 
  drop_na() %>% 
  arrange(-Win) %>% 
  view(title = "lineups")

toc()

lineups_table_top <- list()
entries <- 20

for(i in 1:entries){
  lineups_table_top[[i]] <- lineups[[lineup_table$Lineup[i]]]
}

###Create lineup file###

write.csv(lineups_table_top[[1]], file = "lineups.csv")

for(i in 2:entries){
  write.table(tibble(lineups_table_top[[i]]), file = "lineups.csv", sep = ",", col.names = !file.exists("lineups.csv"), append = T)
}

"lineups" <- read.csv("lineups.csv")

### Next step is add player IDs and create upload ready csv ###

### Check ownership ###

ownership_table <- golfers %>% 
  select(Name, ID, Salary, Win_rank)

for(i in 1:dim(golfers)[1]){
  ownership_table$own[i] <- sum(str_count(lineups$Name, ownership_table$Name[i])) / entries
}

ownership_table %>% 
  arrange(-own) %>% 
  view(title = "Golfers Own")

entries_upload <- tibble()
entries_upload <- tibble(.rows = entries)

for (i in 1:entries) {
  entries_upload[i,1] <- lineups$ID[(i+0 + (6*(i-1)))]
  entries_upload[i,2] <- lineups$ID[(i+1 + (6*(i-1)))]
  entries_upload[i,3] <- lineups$ID[(i+2 + (6*(i-1)))]
  entries_upload[i,4] <- lineups$ID[(i+3 + (6*(i-1)))]
  entries_upload[i,5] <- lineups$ID[(i+4 + (6*(i-1)))]
  entries_upload[i,6] <- lineups$ID[(i+5 + (6*(i-1)))]
  entries_upload[i,7] <- lineups$ID[(i+6 + (6*(i-1)))]
}

entries_upload <- entries_upload[,-c(7)]
names(entries_upload) <- c("1","2","3","4","5","6")

write.csv(entries_upload, file = "entries_upload.csv")

toc()