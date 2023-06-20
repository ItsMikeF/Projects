library(tidyverse, warn.conflicts = F)
library(ggrepel)
library(lubridate, warn.conflicts = F)
library(utils)
library(filesstrings, warn.conflicts = F)
library(xtable)
library(tictoc)
library(lpSolve)
library(stats)
library(XML)
library(binr)
library(httr)

### Inputs ###

tournament <- "Zurich Classic"
date <- c("2022-04-21")
entries <- 21
write <- "no"

### Set Working Directory ###

setwd(paste0("C://Users//",unlist(strsplit(getwd(), "/"))[3],"//Documents//GitHub//DFS_Data//Data_Golf//", date," ", tournament))

### Import csV Files ###

golf_salaries <- read.csv("DKSalaries.csv")
golf_odds <- read.csv("golf-odds-pinnacle-split.csv", header = F)
rg <- read.csv(list.files(pattern = "projections_draftkings_golf"))

### Odds Adjustments ###

names(golf_odds) <- c("Golfer", "PNMLO", "PNBEO", "PNMLC", "PNBEC", "win_delta", "win_delta_per")

convert_ML <- function(odds) {
  breakeven <- if_else(odds > 0, 100 / (100 + odds), abs(odds) / (abs(odds) + 100))
  return(round(breakeven, digits = 4))
}

golf_odds$win_delta_sd <- round(golf_odds$win_delta / sd(golf_odds$win_delta), digits = 2)
golf_odds$win_delta_per_sd <- round(golf_odds$win_delta_per / sd(golf_odds$win_delta_per), digits = 2)

### Create golfer tibble ###

golfers <- golf_salaries %>% 
  left_join(golf_odds, by = c("LastName" = "Golfer"))

golfers$Win_rank <- round(rank(-golfers$PNBEC), digits =0)

### Create golfer table ###

golfer_table <- golfers %>% 
  select(LastName, ID, Salary, Win_rank, PNBEO, PNBEC, win_delta, win_delta_sd, win_delta_per, win_delta_per_sd) %>% 
  drop_na(PNBEC)

rg <- rg %>% 
  select(lastname, fpts, proj_own, ceil, floor)

golfer_table <- golfer_table %>% 
  left_join(rg, by=c("LastName" = "lastname")) %>% 
  drop_na(fpts)

golfer_table$win_per_dollar <- round(golfer_table$PNBEC / golfer_table$Salary * 10^6, digits = 2)
golfer_table$one <- 1

golfer_table$residuals <- round(residuals(loess(win_per_dollar ~ Salary, golfer_table)), digits = 2)

### Golfers Out ###

out <- NULL

golfer_table <- if_else(is.null(out), NULL, golfer_table[-c(which(golfer_table$Name %in% out)),])

### Adjusting Ownership based on bins ###

golfer_bins <- bins(golfer_table$Salary, target.bins = 6, exact.groups = T, max.breaks = 6)
golfer_bins$binct

### Golfer Plot ###

{golfer_table %>%
  ggplot(aes(x = Salary , y = win_per_dollar)) +
  geom_hline(yintercept = mean(golfer_table$win_per_dollar), color = "red", linetype = "dashed", alpha=0.5) +
  geom_vline(xintercept =  mean(golfer_table$Salary), color = "red", linetype = "dashed", alpha=0.5) +
  geom_smooth(method=loess, se=F) +
  geom_point(aes(color = win_per_dollar), alpha = 0.7, cex = 3) +
  scale_color_gradient(low = "red", high = "green", guide = "colourbar") +
  geom_text_repel(aes(label=LastName)) +
  labs(x = "Salary",
       y = "win_per_dollar",
       title = paste(tournament, "Golfers"),
       caption = "Twitter: Its_MikeF | Data: DraftKings") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
}

### Salary Filter ###

golfer_table <- golfer_table %>% filter(Salary >= 7000)

### Optimal Lineup ###

optimal <- lp(direction = "max", 
              objective.in = golfer_table$PNBEC, 
              rbind(golfer_table$Salary, golfer_table$Salary, golfer_table$one), 
              c("<=", ">=", "="), 
              c("50000", "49500", "6"), 
              binary.vec = c(1:dim(golfer_table)[1]))

optimal_lineup <- golfer_table[c(which(optimal$solution == 1)),]
optimal_lineup <- optimal_lineup %>% 
  bind_rows(summarise(.,
                      across(where(is.numeric), sum), 
                      across(where(is.character), ~"")))

### Ownership  ###

own_multiplier <- 100 / entries

golfer_table$own_change <- round(own_multiplier * ((0.5 * golfer_table$win_delta_sd) + (0.5 * golfer_table$win_delta_per_sd)), digits = 2)
#golfer_table$own_change <- if_else(golfer_table$residuals > 1, own_multiplier * golfer_table$residuals, own_multiplier * golfer_table$residuals *2)

golfer_table$adj_own <- round(if_else(golfer_table$proj_own + golfer_table$own_change < 0, 0, 
                                round((golfer_table$proj_own + golfer_table$own_change)/own_multiplier)*own_multiplier), digits = 2)

golfer_table <- golfer_table %>% 
  filter(adj_own > 0)

### Optimal Lineup Table ###

optimal_list <- list()
win_limit <- optimal$objval
lineup_names <- NULL
df <- data.frame()
dlist <- list()
golfer_table2 <- golfer_table
dlist_df <- data.frame()

for (i in 1:entries) {
  
  optimal <- lp(direction = "max", 
                 objective.in = golfer_table2$PNBEC, 
                 rbind(golfer_table2$Salary, golfer_table2$Salary, golfer_table2$one, golfer_table2$PNBEC), 
                 c("<=", ">=", "=", "<"), 
                 c("50000", "49500", "6", win_limit),
                 binary.vec = c(1:dim(golfer_table2)[1]))
  
  optimal_lineup <- golfer_table2[c(which(optimal$solution == 1)),]
  optimal_lineup <- optimal_lineup %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum), 
                        across(where(is.character), ~"")))
  
  optimal_list[[i]] <- optimal_lineup
  
  new_names<- optimal_lineup$LastName[1:6]
  lineup_names <- c(lineup_names, new_names)
  uniq <- unique(lineup_names)
  
  for (k in uniq) {
    cat(k, ': ', sum(lineup_names == k), '\n', sep='')
    dlist[[k]] <- data.frame(i, sum(lineup_names == k)/entries)
    dlist[[k]][3] <- golfer_table2$adj_own[which(golfer_table2$LastName == k)]/100
  }
  
  for (l in 1:length(dlist)) {
    dlist_df[l,1] <- names(dlist)[l]
    dlist_df[l,2] <- dlist[[l]][[1]]
    dlist_df[l,3] <- dlist[[l]][[2]]
    dlist_df[l,4] <- dlist[[l]][[3]]
  }
  
  golfer_table2 <- golfer_table2 %>% 
    left_join(dlist_df, by = c("LastName" = "V1"))
  
  golfer_table2$filter <- if_else(golfer_table2$V3 >= (golfer_table2$adj_own/100), 1,0)
  golfer_table2[is.na(golfer_table2)] <- 0

  golfer_table2 <- golfer_table2 %>% 
    filter(filter == 0)
  
  golfer_table2 <- golfer_table2[,-c(20:24)]
  
}

optimal_table <- do.call("rbind", optimal_list)

### Check ownership ###

ownership_table <- golfer_table %>% 
  select(LastName, ID, Salary, Win_rank)

for(i in 1:dim(golfer_table)[1]){
  ownership_table$own[i] <- round(sum(str_count(optimal_table$LastName, ownership_table$LastName[i])) / (entries-1), digits = 2)
}

ownership_table %>% 
  arrange(-own) %>% 
  view(title = "Golfers Own")

### Create Entries in CSV ###

entries_upload <- tibble(.rows = entries)

for (i in 1:entries) {
  for (j in 1:7) {
    entries_upload[i,j] <- optimal_table$ID[(i+(j-1) + (6*(i-1)))]
  }
}

entries_upload <- entries_upload[,-c(7)]
names(entries_upload) <- c("G","G","G","G","G","G")
entries_upload <- unique(entries_upload)

ifelse(write == "yes", write.csv(entries_upload, file = "entries_upload.csv"), NULL)
ifelse(write == "yes", write.csv(ownership_table, file = "ownership_table.csv"), NULL)