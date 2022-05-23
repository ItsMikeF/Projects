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

### Inputs ###
tournament <- "Mexico Open"
date <- c("2022-04-28")
entries <- 20
write <- "no"

### Set Working Directory ###
setwd(paste0("C://Users//",unlist(strsplit(getwd(), "/"))[3],"//Documents//GitHub//DFS_Data//Data_Golf//", date," ", tournament))

### Import CSVs ###
golf_salaries <- read.csv("DKSalaries.csv")
rg <- read.csv(list.files(pattern = "projections_draftkings_golf"))

odds_dk <- read.csv("odds_dk.csv", header = F)
odds_fd <- read.csv("odds_fd.csv", header = F)
odds_pn <- read.csv("odds_pn.csv", header = F)
odds_rw <- read.csv("golf-odds-rotowire.csv", header = T)
odds_sh <- read.csv("odds_sh.csv", header = F)

### Add Functions ###
convert_ML <- function(odds) {
  breakeven <- if_else(odds > 0, 100 / (100 + odds), abs(odds) / (abs(odds) + 100))
  return(round(breakeven, digits = 4))
}

### FD Odds Adjustments ###
odds_fd_golfers <- odds_fd[seq(1, nrow(odds_fd), 2),]
odds_fd_odds <- odds_fd[seq(2, nrow(odds_fd), 2),]

odds_fd <- data.frame(cbind(odds_fd_golfers, odds_fd_odds))
odds_fd[,2] <- as.numeric(unlist(odds_fd[,2]))

### PN Odds Adjustment ###
odds_pn[,2] <- sapply(odds_pn[,2], convert_ML)

### Adjust Column Names ###
names(odds_dk) <- c("Golfer", "odds")
names(odds_fd) <- c("Golfer", "odds")
names(odds_sh) <- c("Golfer", "odds")
names(odds_pn) <- c("Golfer", "odds")

### DK + FD Odds ###
odds <- odds_dk %>% left_join(odds_fd, by = c("Golfer"))
odds[,2:3] <- sapply(odds[2:3], convert_ML)
names(odds)[2:3] <- c("DK", "FD")
odds$delta <- odds$DK - odds$FD
odds$avg <- (odds$DK + odds$FD)/2

### RW Odds Adjustments ###
odds_rw <- odds_rw[-c(1),-c(3,5,7:17)]
names(odds_rw) <- c("Golfer", "Win", "Top 5", "Top 20")
odds_rw[,c(2:4)] <- sapply(odds_rw[,c(2:4)], as.numeric)

odds_rw[,c(2:4)] <- sapply(odds_rw[,c(2:4)], convert_ML)

### Golf Odds by Date ###
odds_rw_open <- read.csv("golf-odds-rotowire-open.csv", header = T)
odds_rw_close <- read.csv("golf-odds-rotowire-close.csv", header = T)

### Odds Adjustment by Date ###

odds_rw_open <- odds_rw_open[-c(1),-c(3,5,7:17)]
names(odds_rw_open) <- c("Golfer", "Win", "Top 5", "Top 20")
odds_rw_open[,c(2:4)] <- sapply(odds_rw_open[,c(2:4)], as.numeric)
odds_rw_open[,c(2:4)] <- sapply(odds_rw_open[,c(2:4)], convert_ML)
odds_rw_open <- odds_rw_open[,-c(3,4)]

odds_rw_close <- odds_rw_close[-c(1),-c(3,5,7:17)]
names(odds_rw_close) <- c("Golfer", "Win", "Top 5", "Top 20")
odds_rw_close[,c(2:4)] <- sapply(odds_rw_close[,c(2:4)], as.numeric)
odds_rw_close[,c(2:4)] <- sapply(odds_rw_close[,c(2:4)], convert_ML)
odds_rw_close <- odds_rw_close[,-c(3,4)]

### Create golfer tibble ###

golfers <- golf_salaries %>% 
  left_join(odds_pn, by = c("Name" = "Golfer"))

golfers$Win_rank <- round(rank(-golfers$Win), digits =0)

### Create golfer table ###

golfer_table <- golfers %>% 
  select(Name, ID, Salary, AvgPointsPerGame, Win, Win_rank) %>% 
  drop_na(Win)

rg <- rg %>% 
  select(name, fpts, proj_own, ceil, floor)

golfer_table <- golfer_table %>% 
  left_join(rg, by=c("Name" = "name"))

golfer_table$win_per_dollar <- round(golfer_table$Win / golfer_table$Salary * 10^6, digits = 2)
golfer_table$one <- 1

golfer_table$residuals <- round(residuals(loess(win_per_dollar ~ Salary, golfer_table)), digits = 2)

### Compare Odds ###

odds_rw_delta <- odds_rw_open %>% 
  left_join(odds_rw_close, by=c("Golfer"))

odds_rw_delta$win_delta <- odds_rw_delta$Win.y - odds_rw_delta$Win.x

odds_rw_delta %>% 
  arrange(-win_delta) %>% 
  view(title = "Golf Odds Delta")

odds_rw_delta <- odds_rw_delta[,-c(2,3)]

golfer_table <- golfer_table %>% 
  left_join(odds_rw_delta, by=c("Name" = "Golfer"))

### Golfers Out ###

out <- c()

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
  geom_text_repel(aes(label=Name)) +
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

golfer_table <- golfer_table %>% filter(Salary >= 6500)

### Optimal Lineup ###

optimal <- lp(direction = "max", 
              objective.in = golfer_table$Win, 
              rbind(golfer_table$Salary, golfer_table$Salary, golfer_table$one), 
              c("<=", ">=", "="), 
              c("50000", "49000", "6"), 
              binary.vec = c(1:dim(golfer_table)[1]))

optimal_lineup <- golfer_table[c(which(optimal$solution == 1)),]
optimal_lineup <- optimal_lineup %>% 
  bind_rows(summarise(.,
                      across(where(is.numeric), sum), 
                      across(where(is.character), ~"")))

### Start making ownership table ###

golfer_own <- matrix(nrow = dim(golfer_table)[1], ncol = 3)
golfer_own <- golfer_table$Name
golfer_own <- tibble(golfer_own)

entries <- 24
own_multiplier <- 100 / entries

golfer_table$own_change <- own_multiplier * (1000 * golfer_table$win_delta)
golfer_table$own_change <- if_else(golfer_table$residuals > 1, own_multiplier * golfer_table$residuals, own_multiplier * golfer_table$residuals *2)

golfer_table$adj_own <- if_else(golfer_table$proj_own + golfer_table$own_change < 0, 0, 
                                round((golfer_table$proj_own + golfer_table$own_change)/own_multiplier)*own_multiplier)

#count(optimal_lineup, vars = golfer_own)

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
                 objective.in = golfer_table2$Win, 
                 rbind(golfer_table2$Salary, golfer_table2$Salary, golfer_table2$one, golfer_table2$Win), 
                 c("<=", ">=", "=", "<"), 
                 c("50000", "49500", "6", win_limit-(.0005*i)),
                 binary.vec = c(1:dim(golfer_table2)[1]))
  
  optimal_lineup <- golfer_table2[c(which(optimal$solution == 1)),]
  optimal_lineup <- optimal_lineup %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum), 
                        across(where(is.character), ~"")))
  
  optimal_list[[i]] <- optimal_lineup
  
  new_names<- optimal_lineup$Name[1:6]
  lineup_names <- c(lineup_names, new_names)
  uniq <- unique(lineup_names)
  
  for (k in uniq) {
    cat(k, ': ', sum(lineup_names == k), '\n', sep='')
    dlist[[k]] <- data.frame(i, sum(lineup_names == k)/entries)
    dlist[[k]][3] <- golfer_table2$adj_own[which(golfer_table2$Name == k)]/100
  }
  
  for (l in 1:length(dlist)) {
    dlist_df[l,1] <- names(dlist)[l]
    dlist_df[l,2] <- dlist[[l]][[1]]
    dlist_df[l,3] <- dlist[[l]][[2]]
    dlist_df[l,4] <- dlist[[l]][[3]]
  }
  
  golfer_table2 <- golfer_table2 %>% 
    left_join(dlist_df, by = c("Name" = "V1"))
  
  golfer_table2$filter <- if_else(golfer_table2$V3 >= (golfer_table2$adj_own/100), 1,0)
  golfer_table2[is.na(golfer_table2)] <- 0

  golfer_table2 <- golfer_table2 %>% 
    filter(filter == 0)
  
  golfer_table2 <- golfer_table2[,-c(16:19)]
  
}

optimal_table <- do.call("rbind", optimal_list)

### Check ownership ###

ownership_table <- golfer_table %>% 
  select(Name, ID, Salary, Win_rank)

for(i in 1:dim(golfer_table)[1]){
  ownership_table$own[i] <- sum(str_count(optimal_table$Name, ownership_table$Name[i])) / entries
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

write <- readline(prompt = "Write lineups to csv:")
#write.csv(entries_upload, file = "entries_upload.csv")
