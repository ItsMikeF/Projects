library(tidyverse)
library(ggrepel)
library(lubridate, warn.conflicts = F)
library(utils)
library(filesstrings, warn.conflicts = F)
library(xtable)
library(tictoc)
library(lpSolve)

tic("total")

user <- unlist(strsplit(getwd(), "/"))
user <- user[3]

tournament <- "Valero Texas Open"

setwd(paste0("C://Users//",user,"//Documents//GitHub//DFS_Data//Data_Golf//2022-03-31 ", tournament))

### Import CSVs ###

data_golf <- read.csv("datagolf_rankings_current.csv")
golf_salaries <- read.csv("DKSalaries.csv")
golf_odds <- read.csv("golf-odds-rotowire.csv", header = T)
owgr <- read.csv("owgr.csv", header = F)

### Odds Adjustments ###

golf_odds <- golf_odds[-c(1),-c(3,5,7:17)]
names(golf_odds) <- c("Golfer", "Win", "Top 5", "Top 20")
golf_odds[,c(2:4)] <- sapply(golf_odds[,c(2:4)], as.numeric)

convert_ML <- function(odds) {
  breakeven <- if_else(odds > 0, 100 / (100 + odds), abs(odds) / (abs(odds) + 100))
  return(round(breakeven, digits = 4))
}

golf_odds[,c(2:4)] <- sapply(golf_odds[,c(2:4)], convert_ML)

### Data Golf Adjustments ###

data_golf <- data_golf[,-c(2,3,8)]
data_golf <- data_golf %>% 
  separate(player_name, into = c("name", "First_Name"), sep = ",")
data_golf[1] <- str_trim(paste(data_golf$First_Name, data_golf$name))
data_golf <- data_golf[,-2]

### Create golfer tibble ###

golfers <- golf_salaries %>% 
  left_join(golf_odds, by = c("Name" = "Golfer"))

golfers <- golfers %>% 
  left_join(data_golf, by = c("Name" = "name"))

golfers <- golfers %>% 
  left_join(owgr, by = c("Name" = "V5"))

golfers <- golfers[,-c(1,5,7,8)]

golfers$AvgPointsPerGame_sd <- round((golfers$AvgPointsPerGame - mean(golfers$AvgPointsPerGame, na.rm=T)) / sd(golfers$AvgPointsPerGame, na.rm = T), digits = 2)
golfers$Win_sd <- round((golfers$Win - mean(golfers$Win, na.rm=T)) / sd(golfers$Win, na.rm = T), digits = 2)
golfers$dg_rank_sd <- round((golfers$dg_rank - mean(golfers$dg_rank, na.rm=T)) / sd(golfers$dg_rank, na.rm = T), digits = 2)
golfers$dg_change_sd <- round((golfers$dg_change - mean(golfers$dg_change, na.rm=T)) / sd(golfers$dg_change, na.rm = T), digits = 2)
golfers$sum_sd_dg <- 
  (0.7 * golfers$Win_sd) -
  (0.2 * golfers$dg_rank_sd) +
  (0.05 * golfers$dg_change_sd) +
  (0.05 * golfers$AvgPointsPerGame_sd)

golfers$sum_sd <- round(
  (0.98 * golfers$Win_sd) -
  (0.02 * golfers$AvgPointsPerGame_sd), digits = 2)

golfers$Win_rank <- round(rank(-golfers$Win), digits =0)
golfers$sum_sd_rank <- round(rank(-golfers$sum_sd), digits =0)
golfers$sum_sd_dg_rank <- round(rank(-golfers$sum_sd_dg), digits =0)

### Create golfer table ###

golfer_table <- golfers %>% 
  select(Name, ID, Salary, AvgPointsPerGame, Win, Win_rank)

golfer_table$win_per_dollar <- round(golfer_table$Win / golfer_table$Salary * 10^6, digits = 2)
golfer_table$one <- 1

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
view(optimal_lineup)

### Start making ownership table ###

golfer_own <- matrix(nrow = dim(golfer_table)[1], ncol = 3)
golfer_own <- golfer_table$Name
golfer_own <- tibble(golfer_own)

count(optimal_lineup, vars = golfer_own)
count(golfer_own, vars = optimal_lineup)

### 2nd Optimal Lineup ###

win_limit <- optimal$objval

optimal2 <- lp(direction = "max", 
              objective.in = golfer_table$Win, 
              rbind(golfer_table$Salary, golfer_table$Salary, golfer_table$one, golfer_table$Win), 
              c("<=", ">=", "=", "<"), 
              c("50000", "49000", "6", win_limit-.0005),
              binary.vec = c(1:dim(golfer_table)[1]))

optimal2_lineup <- golfer_table[c(which(optimal2$solution == 1)),]
optimal2_lineup <- optimal2_lineup %>% 
  bind_rows(summarise(.,
                      across(where(is.numeric), sum), 
                      across(where(is.character), ~"")))
view(optimal2_lineup)

### Optimal Lineup Loop ###

