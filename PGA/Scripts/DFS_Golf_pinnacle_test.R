#Import packages
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
library(officer)
library(janitor, warn.conflicts = F)

#Inputs
tournament <- "Memorial Tournament"
date <- c("2022-06-02")
entries <- 115

#Import CSVs
folder <- paste0("./", date," ", tournament)
file_list <- list.files(path = folder, pattern = "*.csv")

for (i in 1:length(file_list)){
  assign(file_list[i], 
         read.csv(paste0(folder, "/", file_list[i]))
  )}

#Rename CSVs
golfer_salaries <- get(ls(pattern = "DKSalaries"))
rg <- get(ls(pattern = "projections_draftkings_golf"))
odds_pn_open <- get(ls(pattern = "_open.csv"))
odds_pn_close <- get(ls(pattern = "_close.csv"))
dg_pred <- get(ls(pattern = "_preds_ch_model"))

#Merge Open and Close Odds
odds_pn_open <- odds_pn_open %>% 
  separate(player_name, into = c("last", "first"), sep = ",") 
odds_pn_open$player_name <- trimws(paste(odds_pn_open$first, odds_pn_open$last))
odds_pn_open <- odds_pn_open %>%
  select(player_name, pinnacle_odds)

odds_pn_open$pinnacle_odds[is.infinite(odds_pn_open$pinnacle_odds)] <- 
  max(odds_pn_open$pinnacle_odds[is.finite(odds_pn_open$pinnacle_odds)], na.rm = T)

odds_pn_close <- odds_pn_close %>% 
  separate(player_name, into = c("last", "first"), sep = ",") 
odds_pn_close$player_name <- trimws(paste(odds_pn_close$first, odds_pn_close$last))
odds_pn_close <- odds_pn_close %>% select(player_name, pinnacle_odds)

odds_pn_close$pinnacle_odds[is.infinite(odds_pn_close$pinnacle_odds)] <- 
  max(odds_pn_close$pinnacle_odds[is.finite(odds_pn_close$pinnacle_odds)], na.rm = T)

odds_pn <- odds_pn_open %>% 
  left_join(odds_pn_close, by = c("player_name"))

#Add Functions
convert_ML <- function(odds) {
  breakeven <- if_else(odds > 0, 100 / (100 + odds), abs(odds) / (abs(odds) + 100))
  return(round(breakeven, digits = 4))
}

#PN Odds Adjustment
odds_pn[,2:3] <- sapply(odds_pn[,2:3], convert_ML)
names(odds_pn) <- c("Golfer", "odds_open", "odds_close")
odds_pn$odds_delta <- odds_pn$odds_close - odds_pn$odds_open
odds_pn$odds_delta_per <- round((odds_pn$odds_close - odds_pn$odds_open)/odds_pn$odds_open, digits = 4)

#DG predictions adjustments
dg_pred <- dg_pred %>% 
  separate(player_name, into = c("last", "first"), sep = ",") 

dg_pred$player_name <- trimws(paste(dg_pred$first, dg_pred$last))

dg_pred <- dg_pred %>% 
  select(player_name, make_cut, top_20, top_10, top_5, win)

dg_pred[,2:6] <- sapply(dg_pred[,2:6], convert_ML)

#Rotogrinders adjustments
rg <- rg %>%
  select(name, fpts, proj_own, ceil, floor)

#Create golfer tibble
golfers <- golfer_salaries %>%
  left_join(odds_pn, by = c("Name" = "Golfer"))
 
#Add Odds Rank
golfers$odds_rank <- round(rank(-golfers$odds_close), digits =0)

#Adjust golfer table
golfers <- golfers %>%
  select(Name, ID, Salary, AvgPointsPerGame, odds_open, odds_close, odds_rank, odds_delta, odds_delta_per) %>%
  drop_na(odds_close)

golfers <- golfers %>%
  left_join(rg, by=c("Name" = "name"))

golfers <- golfers %>% 
  left_join(dg_pred, by = c("Name" = "player_name"))

golfers$odds_per_dollar <- round(golfers$odds_close / golfers$Salary * 10^6, digits = 2)
golfers$one <- 1

golfers$residuals <- round(residuals(loess(odds_per_dollar ~ Salary, golfers)), digits = 2)

#Golfers Out
out <- NULL
#golfers <- golfers[-c(which(golfers$Name %in% out)),]

#Bins
golfer_bins <- bins(golfers$Salary, target.bins = 6, exact.groups = T, max.breaks = 6)
golfer_bins$binct

#Golfer Plot
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

#Optimal Lineup
optimal <- lp(direction = "max", 
              objective.in = golfers$odds_close, 
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

#golfers$ceil_rank <- round((golfers$ceil - mean(golfers$ceil, na.rm=T)) / sd(golfers$ceil, na.rm = T), digits = 2)
#golfers$floor_rank <- round((golfers$floor - mean(golfers$floor, na.rm=T)) / sd(golfers$floor, na.rm = T), digits = 2)

#Max ownership formula
golfers$own_change <- round(
  (own_multiplier * 1.5 * ((golfers$ceil - mean(golfers$ceil, na.rm=T)) / sd(golfers$ceil, na.rm = T))) +
  (own_multiplier * 1.0 * ((golfers$AvgPointsPerGame - mean(golfers$AvgPointsPerGame, na.rm=T)) / sd(golfers$AvgPointsPerGame, na.rm = T))) +
  (own_multiplier * 0.6 * golfers$residuals) +
  (own_multiplier * 0.7 * if_else(golfers$odds_delta_per > 1, 1, golfers$odds_delta_per)) +
  (own_multiplier * 2), digits = 2)

golfers$adj_own <- case_when(golfers$proj_own + golfers$own_change <= 0 ~ 0,
                             golfers$proj_own + golfers$own_change < 40 ~ round((golfers$proj_own + golfers$own_change)/own_multiplier)*own_multiplier, 
                             golfers$proj_own + golfers$own_change >= 40 ~ 40)

golfers$total_pts <- 0

#Write
write.csv(golfers, file = paste0("./", date," ", tournament,"/golfers.csv"))
write.csv(golfers, file = paste0("./Results/golfers.csv"))
