library(tidyverse)
library(ggrepel)
library(lubridate)
library(utils)
library(filesstrings)
library(gt)

user <- unlist(strsplit(getwd(), "/"))
user <- user[3]

date <- format(if_else(format(Sys.time(), format = "%H:%M:%S") > 13, today()+1, today()), format = "%Y-%m-%d")

if(!exists(paste("C://Users//",user,"//Documents//GitHub//DFS_Data//Data_CBB//",date, sep = "")))
{
  dir.create(paste("C://Users//",user,"//Documents//GitHub//DFS_Data//Data_CBB//",date, sep = ""))
  setwd(paste("C://Users//",user,"//Downloads", sep = ""))
  file.move(c("summary22.csv", "cbb-odds-rotowire.csv"), paste("C://Users//",user,"//Documents//GitHub//DFS_Data//Data_CBB//",date, sep = ""), 
            overwrite = T)
}

setwd(paste("C://Users//",user,"//Documents//GitHub//DFS_Data//Data_CBB//",date, sep = ""))

###Kenpom CBB Ratings###

kp <- read.csv("summary22.csv")

kp$AdjEM <- round(kp$AdjEM, digits = 3)
kp$AdjOE <- round(kp$AdjOE, digits = 3)
kp$AdjDE <- round(kp$AdjDE, digits = 3)
kp$AdjTempo <- round(kp$AdjTempo, digits = 1)

kp <- kp %>% 
  select(TeamName, 
         AdjEM,
         RankAdjEM, 
         AdjTempo,
         RankAdjTempo, 
         AdjOE, 
         RankAdjOE, 
         AdjDE, 
         RankAdjDE) %>%
  arrange(-AdjEM)

###Rotowire Adjustments###

rotowire <- read.csv("cbb-odds-rotowire.csv")

rotowire <- mutate_if(rotowire, is.character, str_replace_all, pattern = "State", replacement = "St.")

names(rotowire)[c(1,2,11)] <- c('Squad', 'DateTime', 'Opp.Score')

rotowire <- rotowire[-c(1),-c(4,6,8)]

rotowire[4:8] <- as.numeric(unlist(rotowire[4:8]))

rotowire$Squad <- gsub("-", " ", rotowire$Squad)

#####

rotowire <- replace(rotowire, rotowire == 'UNC-Asheville', 'UNC Asheville')
rotowire <- replace(rotowire, rotowire == 'UNC-Wilmington', 'UNC Wilmington')
rotowire <- replace(rotowire, rotowire == 'LIU-Brooklyn', 'LIU')
rotowire <- replace(rotowire, rotowire == 'Detroit', 'Detroit Mercy')
rotowire <- replace(rotowire, rotowire == 'Texas-San Antonio', '')
rotowire <- replace(rotowire, rotowire == 'Texas-San Antonio', 'UTSA')
rotowire <- replace(rotowire, rotowire == "St. Mary's (CAL)", "Saint Mary's")
rotowire <- replace(rotowire, rotowire == 'Wisconsin-Green Bay', 'Green Bay')
rotowire <- replace(rotowire, rotowire == 'Wisconsin-Milwaukee', 'Milwaukee')
rotowire <- replace(rotowire, rotowire == 'IPFW', 'Purdue Fort Wayne')
rotowire <- replace(rotowire, rotowire == 'Illinois-Chicago', 'Illinois Chicago')
rotowire <- replace(rotowire, rotowire == 'California Baptist', 'Cal Baptist')
rotowire <- replace(rotowire, rotowire == 'Cal-Poly San Luis Obispo', 'Cal Poly')
rotowire <- replace(rotowire, rotowire == 'Citadel', 'The Citadel')
rotowire <- replace(rotowire, rotowire == 'Tennessee-Martin', 'Tennessee Martin')
rotowire <- replace(rotowire, rotowire == 'St. Francis (NY)', 'St. Francis NY')
rotowire <- replace(rotowire, rotowire == 'E. Tennessee St.', 'East Tennessee St.')
rotowire <- replace(rotowire, rotowire == 'Texas A&M Corpus Christi', 'Texas A&M Corpus Chris')
rotowire <- replace(rotowire, rotowire == 'Southern Mississippi', 'Southern Miss')
rotowire <- replace(rotowire, rotowire == 'Arkansas-Little Rock', 'Little Rock')
rotowire <- replace(rotowire, rotowire == 'Miami', 'Miami FL')
rotowire <- replace(rotowire, rotowire == 'Central Florida', 'UCF')
rotowire <- replace(rotowire, rotowire == 'Arkansas-Pine Bluff', 'Arkansas Pine Bluff')
rotowire <- replace(rotowire, rotowire == 'Loyola (MD)', 'Loyola MD')
rotowire <- replace(rotowire, rotowire == 'Texas Rio Grande Valley', 'UT Rio Grande Valley')
rotowire <- replace(rotowire, rotowire == 'Loyola-Chicago', 'Loyola Chicago')
rotowire <- replace(rotowire, rotowire == 'Middle Tennessee St.', 'Middle Tennessee')
rotowire <- replace(rotowire, rotowire == 'Maryland-Baltimore County', '')
rotowire <- replace(rotowire, rotowire == "St. Joseph's", "Saint Joseph's")
rotowire <- replace(rotowire, rotowire == 'St. Francis (Pa.)', 'St. Francis PA')
rotowire <- replace(rotowire, rotowire == 'Massachusetts-Lowell', 'UMass Lowell')
rotowire <- replace(rotowire, rotowire == 'Florida International', 'FIU')
rotowire <- replace(rotowire, rotowire == 'North Carolina St.', 'N.C. State')
rotowire <- replace(rotowire, rotowire == 'Miami (OH)', 'Miami OH')
rotowire <- replace(rotowire, rotowire == 'Pennsylvania', 'Penn')
rotowire <- replace(rotowire, rotowire == 'LIU Brooklyn', 'LIU')
rotowire <- replace(rotowire, rotowire == 'Wisconsin Green Bay', 'Green Bay')
rotowire <- replace(rotowire, rotowire == 'Prairie View', 'Prairie View A&M')
rotowire <- replace(rotowire, rotowire == 'Wisconsin Milwaukee', 'Milwaukee')
rotowire <- replace(rotowire, rotowire == 'Cal Poly San Luis Obispo', 'Cal Poly')
rotowire <- replace(rotowire, rotowire == 'SE Louisiana', 'Southeastern Louisiana')
rotowire <- replace(rotowire, rotowire == 'Grambling', 'Grambling St.')
rotowire <- replace(rotowire, rotowire == 'Arkansas Little Rock', 'Little Rock')
rotowire <- replace(rotowire, rotowire == 'Louisiana Lafayette', 'Lafayette')
rotowire <- replace(rotowire, rotowire == 'Sam Houston', 'Sam Houston St.')
rotowire <- replace(rotowire, rotowire == 'Cal Santa Barbara', 'UC Santa Barbara')
rotowire <- replace(rotowire, rotowire == 'Louisiana St.', 'LSU')
rotowire <- replace(rotowire, rotowire == 'UNC Charlotte', 'Charlotte')
rotowire <- replace(rotowire, rotowire == 'Texas San Antonio', 'UTSA')
rotowire <- replace(rotowire, rotowire == 'Massachusetts Lowell', 'UMass Lowell')
rotowire <- replace(rotowire, rotowire == 'Texas Arlington', 'UT Arlington')
rotowire <- replace(rotowire, rotowire == 'Omaha', 'Nebraska Omaha')
rotowire <- replace(rotowire, rotowire == 'SE Missouri St.', 'Missouri St.')
rotowire <- replace(rotowire, rotowire == 'Maryland Baltimore County', 'UMBC')
rotowire <- replace(rotowire, rotowire == 'College of Charleston', 'Charleston')
rotowire <- replace(rotowire, rotowire == "St. Peter's", "Saint Peter's")
rotowire <- replace(rotowire, rotowire == 'UM Kansas City', 'UMKC')
rotowire <- replace(rotowire, rotowire == '', '')
rotowire <- replace(rotowire, rotowire == '', '')

#####

###Draftkings Slate###

rotowire.away <- rotowire[seq(1, nrow(rotowire), 2), ]
rotowire.home <- rotowire[seq(2, nrow(rotowire), 2), ]

rotowire.away$DateTimeScorePoints <- paste(rotowire.away$DateTime, rotowire.away$Opp.Score, rotowire.away$Total.Points)
rotowire.home$DateTimeScorePoints <- paste(rotowire.home$DateTime, rotowire.home$Implied.Score, rotowire.home$Total.Points)

rotowire.away <- rotowire.away %>% 
  left_join(kp, by = c("Squad" = "TeamName"))

rotowire.home <- rotowire.home %>% 
  left_join(kp, by = c("Squad" = "TeamName"))

rotowire.slate <- rotowire.away %>% 
  left_join(rotowire.home, by = c("DateTimeScorePoints" = "DateTimeScorePoints")) 

rotowire.slate[, c(5:9)] <- sapply(rotowire.slate[, c(5:9)], as.numeric)

rotowire.slate$AdjEM.diff <- (rotowire.slate$AdjEM.y - rotowire.slate$AdjEM.x)
rotowire.slate$Cover.plus.AdjEM <- rotowire.slate$Cover.y + rotowire.slate$AdjEM.diff

rotowire.slate$AdjTempo.diff <- rotowire.slate$AdjTempo.y - rotowire.slate$AdjTempo.x
rotowire.slate$AdjTempo.sum <- rotowire.slate$AdjTempo.y + rotowire.slate$AdjTempo.x
rotowire.slate$Adjtempo_minus_Points <- rotowire.slate$AdjTempo.sum - rotowire.slate$Total.Points.y
rotowire.slate$Home.Squad.Cover <- paste(rotowire.slate$Squad.y, rotowire.slate$Cover.y)
rotowire.slate$Away_Home <- paste(rotowire.slate$Squad.x, rotowire.slate$Squad.y)

rotowire.slate$rank_sum <- rotowire.slate$RankAdjEM.x + rotowire.slate$RankAdjEM.y
rotowire.slate$rank_Total.Points.y <- round(rank(-rotowire.slate$Total.Points.y), digits = 0)
rotowire.slate$rank_AdjTempo.sum <- round(rank(-rotowire.slate$AdjTempo.sum), digits = 0)
rotowire.slate$Cover.y_rank <- round(rank(abs(rotowire.slate$Cover.y)), digits = 0)

rotowire.slate$Cover.plus.AdjEM.abs <- abs(rotowire.slate$Cover.plus.AdjEM)

rotowire.slate$thrill_score <- round(
  (.40 * rotowire.slate$rank_sum) + 
  (.30 * abs(rotowire.slate$Cover.y)*10) +
  (.10 * rotowire.slate$rank_Total.Points.y) +
  (.20 * rotowire.slate$rank_AdjTempo.sum), digits = 0)

rotowire.slate %>% 
  #filter(Adjtempo_minus_Points < -10 | Adjtempo_minus_Points > 10) %>% 
  select(RankAdjOE.x,
         RankAdjDE.x,
         RankAdjEM.x,
         Squad.x,
         RankAdjOE.y,
         RankAdjDE.y,
         RankAdjEM.y,
         Squad.y,
         Cover.y,
         thrill_score,
         AdjEM.diff, 
         Cover.plus.AdjEM,
         Total.Points.y,
         AdjTempo.sum, 
         Adjtempo_minus_Points,
         AdjTempo.diff) %>% 
  arrange(thrill_score) %>%
  #drop_na() %>% 
  view(title = "CBB Slate")

###CBB Chart###

rotowire.slate %>%
  ggplot(aes(x = Cover.plus.AdjEM , y = AdjTempo.diff)) +
  geom_hline(yintercept = mean(rotowire.slate$AdjTempo.diff, na.rm = TRUE), color = "red", linetype = "dashed", alpha=0.5) +
  geom_vline(xintercept =  mean(rotowire.slate$Cover.plus.AdjEM, na.rm = TRUE), color = "red", linetype = "dashed", alpha=0.5) +
  geom_point(color = rainbow(dim(rotowire.slate)[1]), alpha = .6) +
  geom_text_repel(aes(label=Home.Squad.Cover)) +
  labs(x = "Cover.plus.AdjEM",
       y = "AdjTempo.diff",
       title = paste("CBB Slate", today()),
       caption = "Twitter: Its_MikeF | Data: Kenpom") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold")) +
  #scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_reverse(n.breaks = 10)

###Daily Picks

daily_spreads <- rotowire.slate %>% 
  filter(Cover.plus.AdjEM.abs >= 5 & Cover.plus.AdjEM.abs <= 9) %>% 
  select(Squad.x, Cover.x, Squad.y, Cover.y, Cover.plus.AdjEM) %>% 
  arrange(-Cover.plus.AdjEM) %>% 
  view("Daily Spreads")

daily_totals <- rotowire.slate %>%
  filter(Adjtempo_minus_Points >= 12 & Adjtempo_minus_Points <= 15) %>% 
  gt()

###Write to season file###

setwd(paste("C://Users//",user,"//Documents//GitHub//DFS_Data//Data_CBB//2021", sep = ""))
write.table(rotowire.slate, file = "cbb.slate.csv", sep = ",", col.names = !file.exists("cbb.slate.csv"), append = T)