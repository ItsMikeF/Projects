library(tidyverse)
library(ggrepel)
library(lubridate)
library(utils)
library(xlsx)

user <- unlist(strsplit(getwd(), "/"))
user <- user[3]

setwd(paste("C://Users//",user,"//Documents//Github//DFS_Data//Data_CBB//2021", sep = ""))

cbb_schools <- read.csv("CBB_schools.csv")
cbb <- read.csv("cbb.slate.csv")
sbr <- read.csv("ncaa basketball 2021-22.csv")
#sbr <- read.xlsx("ncaa basketball 2021-22.xlsx", sheetName = "Sheet1")

###Adjustments###

sbr <- sbr %>% 
  left_join(cbb_schools, by =c("Team" ="SBR"))

cbb <- unique(cbb)

###Sports Book Review###
###https://www.sportsbookreviewsonline.com/scoresoddsarchives/ncaabasketball/ncaabasketballoddsarchives.htm 

sbr.away <- sbr[seq(1, nrow(sbr), 2), ]
sbr.away <- sbr.away[,-c(8:11)]
names(sbr.away)[3:7] <- c("V", "Visitor", "V_1st", "V_2nd", "V_Final")
sbr.away[,4] <- sbr.away[,8]
sbr.away <- sbr.away[-c(5:6,8)]
sbr.away$Date <- as.numeric(trimws(paste(sbr.away$Date, if_else(sbr.away$Date >1100, 2021,2022), sep = "")))
sbr.away$Date <- mdy(sbr.away$Date)

sbr.home <- sbr[seq(2, nrow(sbr), 2), ]
sbr.home <- sbr.home[,-c(8:11)]
names(sbr.home)[3:7] <- c("H", "Home", "H_1st", "H_2nd", "H_Final")
sbr.home[,4] <- sbr.home[,8]
sbr.home <- sbr.home[-c(1:3, 5:6, 8)]

sbr.season <- cbind(sbr.away, sbr.home)
sbr.season$total <- sbr.season$V_Final + sbr.season$H_Final
sbr.season$date_away_home <- paste(sbr.season$Date, sbr.season$Visitor, sbr.season$Home)

###CBB Adjustments###
cbb$date <- as.POSIXct(cbb$DateTime.x, format = "%m/%d/%Y")
cbb$date_away_home <- paste(cbb$date, cbb$Away_Home)

cbb <- cbb %>% 
  left_join(sbr.season, by = c("date_away_home"))

cbb$total.minus.Total.Points.y <- cbb$total - cbb$Total.Points.y
cbb$Adjtempo_minus_Points_correct <- if_else(sign(cbb$Adjtempo_minus_Points) == sign(cbb$total.minus.Total.Points.y), 1, 0)

cbb$abs_Adjtempo_minus_Points <- abs(cbb$Adjtempo_minus_Points)

cbb <- cbb %>% 
  filter(abs_Adjtempo_minus_Points < 20)

###CBB Adjtemp Point Table###

max(cbb$Adjtempo_minus_Points, na.rm = T)
min(cbb$Adjtempo_minus_Points, na.rm = T)

limit <- if_else( abs(min(cbb$Adjtempo_minus_Points, na.rm = T)) > abs(max(cbb$Adjtempo_minus_Points, na.rm = T)), 
                  floor(abs(max(cbb$Adjtempo_minus_Points, na.rm = T))), floor(abs(min(cbb$Adjtempo_minus_Points, na.rm = T))))

###Win Table by Point Limit###

cbb_season <- cbb %>% 
  filter(Adjtempo_minus_Points < -1 | Adjtempo_minus_Points > 1) %>% 
  drop_na(Adjtempo_minus_Points_correct)

win_chart_totals <- data.frame(1:14)

cbb_season$abs_Adjtempo_minus_Points <- abs(cbb_season$Adjtempo_minus_Points)

cbb_season <- cbb_season %>% 
  filter(abs_Adjtempo_minus_Points < 15)

cbb_season$duplicate <- paste(cbb_season$Squad.x, cbb_season$DateTime.x)
cbb_season <- cbb_season %>% 
  filter(duplicated(duplicate))

for(i in 1:14){
  cbb_season <- cbb %>% 
    filter(Adjtempo_minus_Points < -i | Adjtempo_minus_Points > i) %>% 
    drop_na(Adjtempo_minus_Points_correct)
  
  win_chart_totals[i,2] <- round(sum(cbb_season$Adjtempo_minus_Points_correct, na.rm = T) / dim(cbb_season)[1], digits =3)
  win_chart_totals[i,3] <- sum(cbb_season$Adjtempo_minus_Points_correct, na.rm = T)
  win_chart_totals[i,4] <- dim(cbb_season)[1]
}

names(win_chart_totals) <- c("Adjtempo_minus_Points", "WinPer", "Wins", "Bets")
win_chart_totals <- add_column(win_chart_totals, win_chart_totals$Bets - win_chart_totals$Wins, .after = "Wins")
names(win_chart_totals)[4] <- "Losses"

odds = -110
unit_size = 100
unit_win = round(unit_size * (100/abs(odds)), digits =2)
win_chart_totals$Profit <- round((win_chart_totals$Wins * unit_win) - (win_chart_totals$Losses * unit_size), digits = 0)
win_chart_totals$Wagered <- win_chart_totals$Bets * unit_size
win_chart_totals$Units <- round(win_chart_totals$Profit / unit_size, digits = 1)
win_chart_totals$ROI <- round(win_chart_totals$Profit / win_chart_totals$Wagered, digits = 4)

###KP Adjusted Tempo Model Win Chart###

win_coefs <- coef(lm(WinPer ~ Adjtempo_minus_Points, data = win_chart_totals))

win_chart %>% 
  ggplot(aes(x = Adjtempo_minus_Points, y = WinPer)) +
  geom_point(alpha = 0.5, cex = 100 * win_chart$ROI) +
  geom_smooth(method = "lm", se = F) +
  geom_text_repel(aes(label=paste(Bets,"Bets", "\n", "+", Units, "Units"))) +
  labs(x = "Adjtempo_minus_Points",
       y = "Bet Win Percentage",
       title = "2021-2022 CBB Season: O/U Betting System",
       subtitle = paste("Qualified Games:", win_chart[1,5], "of", dim(cbb)[1]),
       caption = paste("Twitter: Its_MikeF | Data: Kenpom |", today())) +
  theme_bw() +
  theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold")) +
  annotate("text", x = 12, y = win_chart[1,2], label = paste("r2 =",round(summary(lm(WinPer ~ Adjtempo_minus_Points, data = win_chart))$r.squared, digits =3))) +
  annotate("text", x = 12, y = win_chart[2,2], 
           label = paste("Win% =", "(", round(win_coefs[2], digits =4), "* Adjtempo_minus_Points ) +", round(win_coefs[1], digits = 2))) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n=14))

###AdjEM System###

cbb$winner <- if_else(cbb$V_Final > cbb$H_Final, cbb$Visitor, cbb$Home)
cbb$favorite <- if_else(cbb$Cover.x < 0, cbb$Squad.x, cbb$Squad.y)
cbb$dog <- if_else(cbb$Cover.x < 0, cbb$Squad.y, cbb$Squad.x)
cbb$favorite_spread <- if_else(cbb$Cover.x < 0, cbb$Cover.x, cbb$Cover.y)
cbb$favorite_margin <- if_else(cbb$Cover.x < 0, cbb$V_Final - cbb$H_Final, cbb$H_Final - cbb$V_Final)

cbb$team_covered <- if_else(cbb$favorite_margin < 0, cbb$dog, cbb$favorite)

cbb$team_pick_to_cover <- if_else(sign(cbb$Cover.plus.AdjEM) < 0, cbb$Squad.x, cbb$Squad.y)
cbb$team_pick_to_cover_spread <- if_else(sign(cbb$Cover.plus.AdjEM) < 0, cbb$Cover.x, cbb$Cover.y)
cbb$team_pick_to_cover_result <- if_else(sign(cbb$Cover.plus.AdjEM) < 0, cbb$V_Final - cbb$H_Final, cbb$H_Final - cbb$V_Final)

cbb$Cover.plus.AdjEM.correct <- if_else(cbb$team_covered == cbb$team_pick_to_cover,1,0)

cbb$Cover.plus.AdjEM.spread.minus.result <- if_else(cbb$team_pick_to_cover_spread < 0, abs(cbb$team_pick_to_cover_spread) - cbb$team_pick_to_cover_result,
                                                    cbb$team_pick_to_cover_spread - team_pick_to_cover_result)

###Win Table by Point Limit###

cbb_spreads <- cbb %>% 
  drop_na(Cover.plus.AdjEM)

win_chart_spreads <- data.frame(1:10)

for(i in 1:10){
  cbb_spreads <- cbb_spreads %>% 
    filter(Cover.plus.AdjEM < -i | Cover.plus.AdjEM > i)
  
  win_chart_spreads[i,2] <- round(sum(cbb_spreads$Cover.plus.AdjEM.correct, na.rm = T) / dim(cbb_spreads)[1], digits =3)
  win_chart_spreads[i,3] <- sum(cbb_spreads$Cover.plus.AdjEM.correct, na.rm = T)
  win_chart_spreads[i,4] <- dim(cbb_spreads)[1]
}

names(win_chart_spreads) <- c("Cover.plus.AdjEM", "WinPer", "Wins", "Bets")
win_chart_spreads <- add_column(win_chart_spreads, win_chart_spreads$Bets - win_chart_spreads$Wins, .after = "Wins")
names(win_chart_spreads)[4] <- "Losses"

win_chart_spreads$Profit <- round((win_chart_spreads$Wins * unit_win) - (win_chart_spreads$Losses * unit_size), digits = 0)
win_chart_spreads$Wagered <- win_chart_spreads$Bets * unit_size
win_chart_spreads$Units <- round(win_chart_spreads$Profit / unit_size, digits = 1)
win_chart_spreads$ROI <- round(win_chart_spreads$Profit / win_chart_spreads$Wagered, digits = 4)

###AdjEM System Plot###

win_spreads_coefs <- coef(lm(WinPer ~ Cover.plus.AdjEM, data = win_chart_spreads))

win_chart_spreads %>% 
  ggplot(aes(x = Cover.plus.AdjEM, y = WinPer)) +
  geom_point(alpha = 0.5, cex = 100 * win_chart_spreads$ROI) +
  geom_smooth(method = "lm", se = F) +
  geom_text_repel(aes(label=paste(Bets,"Bets", "\n", "+", Units, "Units"))) +
  labs(x = "Cover.plus.AdjEM",
       y = "Bet Win Percentage",
       title = "2021-2022 CBB Season: Spreads Betting System",
       subtitle = paste("Qualified Games:", win_chart_spreads[1,5], "of", dim(cbb)[1]),
       caption = paste("Twitter: Its_MikeF | Data: Kenpom |", today())) +
  theme_bw() +
  theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold")) +
  annotate("text", x = 7, y = win_chart_spreads[1,2], label = paste("r2 =",round(summary(lm(WinPer ~ Cover.plus.AdjEM, data = win_chart_spreads))$r.squared, digits =3))) +
  annotate("text", x = 7, y = win_chart_spreads[2,2], 
           label = paste("Win% =", "(", round(win_coefs[2], digits =4), "* Cover.plus.AdjEM ) +", round(win_coefs[1], digits = 2))) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n=10))
