library(tidyverse, warn.conflicts = F)
library(ggrepel)
library(lubridate, warn.conflicts = F)
library(utils)
library(filesstrings, warn.conflicts = F)
library(xtable)
library(lpSolve)
library(stats)
library(XML)
library(binr)

### Inputs ###
tournament <- "The Masters"
date <- c("2022-04-07")
entries <- 20
salary_filter <- 7000

### Set Working Directory ###
setwd(paste0("C://Users//",unlist(strsplit(getwd(), "/"))[3],"//Documents//GitHub//DFS_Data//Data_Golf//", date," ", tournament))

### Import CSVs ###
golf_salaries <- read.csv("DKSalaries.csv")
rg <- read.csv(list.files(pattern = "projections_draftkings_golf"))
odds_pn <- read.csv("wells_fargo_championship_win_american_ch.csv", header = T)

### Add Functions ###
convert_ML <- function(odds) {
  breakeven <- if_else(odds > 0, 100 / (100 + odds), abs(odds) / (abs(odds) + 100))
  return(round(breakeven, digits = 4))
}

### PN Odds Adjustment ###
odds_pn[,2:3] <- sapply(odds_pn[,2:3], convert_ML)
names(odds_pn) <- c("Golfer", "odds_open", "odds_close")
odds_pn$odds_delta <- odds_pn$odds_close - odds_pn$odds_open
odds_pn$odds_delta_per <- round((odds_pn$odds_close - odds_pn$odds_open)/odds_pn$odds_open, digits = 4)

### Create golfer tibble ###
golfers <- golf_salaries %>% left_join(odds_pn, by = c("Name" = "Golfer"))

### Add Odds Rank ###
golfers$odds_rank <- round(rank(-golfers$odds_close), digits =0)

### Create golfer table ###
golfers <- golfers %>% select(Name, ID, Salary, AvgPointsPerGame, odds_open, odds_close, odds_rank, odds_delta, odds_delta_per) %>% drop_na(odds_close)
rg <- rg %>% select(name, fpts, proj_own, ceil, floor)

golfers <- golfers %>% left_join(rg, by=c("Name" = "name"))

golfers$odds_per_dollar <- round(golfers$odds_close / golfers$Salary * 10^6, digits = 2)
golfers$one <- 1

golfers$residuals <- round(residuals(loess(odds_per_dollar ~ Salary, golfers)), digits = 2)

### Golfers Out ###
out <- c("Kevin Chappell", "Nick Hardy")
golfers <- golfers[-c(which(golfers$Name %in% out)),]

### Bins ###
golfer_bins <- bins(golfers$Salary, target.bins = 6, exact.groups = T, max.breaks = 6)
golfer_bins$binct

### Golfer Plot ###

{golfers %>%
  ggplot(aes(x = Salary , y = odds_per_dollar)) +
  geom_hline(yintercept = mean(golfers$odds_per_dollar), color = "red", linetype = "dashed", alpha=0.5) +
  geom_vline(xintercept =  mean(golfers$Salary), color = "red", linetype = "dashed", alpha=0.5) +
  geom_smooth(method=loess, se=F) +
  geom_point(aes(color = odds_per_dollar), alpha = 0.7, cex = 3) +
  scale_color_gradient(low = "red", high = "green", guide = "colourbar") +
  geom_text_repel(aes(label=Name)) +
  labs(x = "Salary",
       y = "odds_per_dollar",
       title = paste(tournament, "Golfers"),
       caption = "Twitter: Its_MikeF | Data: DraftKings") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
}

### Optimal Lineup ###
optimal <- lp(direction = "max", 
              objective.in = golfers$odds_close, 
              rbind(golfers$Salary, golfers$Salary, golfers$one), 
              c("<=", ">=", "="), 
              c("50000", "49000", "6"), 
              binary.vec = c(1:dim(golfers)[1]))

optimal_lineup <- golfers[c(which(optimal$solution == 1)),]
optimal_lineup <- optimal_lineup %>% 
  bind_rows(summarise(.,
                      across(where(is.numeric), sum), 
                      across(where(is.character), ~"")))

### Ownership table ###
golfer_own <- matrix(nrow = dim(golfers)[1], ncol = 3)
golfer_own <- golfers$Name
golfer_own <- tibble(golfer_own)

own_multiplier <- 100 / entries

### Ownership Change formula
golfers$own_change <- round(
  (own_multiplier * 0.15 * golfers$residuals) +
  (own_multiplier * 2) +
  (2 * (1000 * golfers$odds_delta)) +
  (own_multiplier * 2 * if_else(golfers$odds_delta_per > 1, 1, golfers$odds_delta_per)^2), digits = 2)

golfers$adj_own <- case_when(golfers$proj_own + golfers$own_change <= 0 ~ 0,
                             golfers$proj_own + golfers$own_change < 100 ~ round((golfers$proj_own + golfers$own_change)/own_multiplier)*own_multiplier, 
                             golfers$proj_own + golfers$own_change >= 100 ~ 100)
                           

#count(optimal_lineup, vars = golfer_own)

### Salary Filter ###
golfers2 <- golfers %>% filter(Salary >= salary_filter)

### Optimal Lineup Table ###

optimal_list <- list()
odds_limit <- optimal$objval
lineup_names <- NULL
dlist <- list()
dlist_df <- data.frame()

for (i in 1:entries) {
  
  optimal <- lp(direction = "max", 
                 objective.in = golfers2$odds_close, 
                 rbind(golfers2$Salary, golfers2$Salary, golfers2$one, golfers2$odds_close), 
                 c("<=", ">=", "=", "<"), 
                 c("50000", "49500", "6", odds_limit-.00005),
                 binary.vec = c(1:dim(golfers2)[1]))
  
  optimal_lineup <- golfers2[c(which(optimal$solution == 1)),]
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

### Lineup Check ###
entries_wp <- data.frame()
for (m in 1:entries) {
  entries_wp[m,1] <- optimal_table$odds_close[(m*7)]
}
names(entries_wp) <- "Lineup WP"

### Check ownership ###
ownership_table <- golfers %>% 
  select(Name, ID, Salary, odds_open, odds_close, odds_delta, odds_delta_per, odds_rank, own_change)

for(i in 1:dim(golfers)[1]){
  ownership_table$own[i] <- sum(str_count(optimal_table$Name, ownership_table$Name[i])) / entries
}

ownership_table %>% 
  arrange(-own) %>% 
  view(title = "Golfers Own")

### Create Entries CSV ###
entries_upload <- tibble(.rows = entries)

for (i in 1:entries) {
  for (j in 1:7) {
    entries_upload[i,j] <- optimal_table$ID[(i+(j-1) + (6*(i-1)))]
  }
}

entries_upload <- entries_upload[,-c(7)]
names(entries_upload) <- c("G","G","G","G","G","G")
entries_upload <- unique(entries_upload)

### Write ###
#write.csv(golfers, file = "golfers.csv")
#write.csv(ownership_table, file = "ownership_table.csv")
#write.csv(entries_upload, file = "entries_upload.csv")
