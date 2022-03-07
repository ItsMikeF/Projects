library(tidyverse)
library(ggrepel)
library(lubridate)
library(utils)
library(xlsx)
library(stringr)
library(janitor)

setwd(paste("C://Users//Mike Francis//Documents//Projects//DFS_Data//Data_CBB",as.character(today()-1), sep = "//"))

###Kenpom CBB Ratings###

kp.a <- read.csv("summary22.csv")

kp.a$AdjEM <- round(kp.a$AdjEM, digits = 3)
kp.a$AdjOE <- round(kp.a$AdjOE, digits = 3)
kp.a$AdjDE <- round(kp.a$AdjDE, digits = 3)
kp.a$AdjTempo <- round(kp.a$AdjTempo, digits = 1)

kp.a <- kp.a %>% 
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

rotowire.a <- read.csv("cbb-odds-rotowire.csv")

rotowire.a <- mutate_if(rotowire.a, is.character, str_replace_all, pattern = "State", replacement = "St.")

names(rotowire.a)[c(1,2,11)] <- c('Squad', 'DateTime', 'Opp.Score')

rotowire.a <- rotowire.a[-c(1),-c(4,6,8)]

rotowire.a[4:8] <- as.numeric(unlist(rotowire.a[4:8]))

rotowire.a$Squad <- gsub("-", " ", rotowire.a$Squad)

rotowire.a <- replace(rotowire.a, rotowire.a == 'UNC-Asheville', 'UNC Asheville')
rotowire.a <- replace(rotowire.a, rotowire.a == 'UNC-Wilmington', 'UNC Wilmington')
rotowire.a <- replace(rotowire.a, rotowire.a == 'LIU-Brooklyn', 'LIU')
rotowire.a <- replace(rotowire.a, rotowire.a == 'Detroit', 'Detroit Mercy')
rotowire.a <- replace(rotowire.a, rotowire.a == 'Texas-San Antonio', '')
rotowire.a <- replace(rotowire.a, rotowire.a == 'Texas-San Antonio', 'UTSA')
rotowire.a <- replace(rotowire.a, rotowire.a == "St. Mary's (CAL)", "Saint Mary's")
rotowire.a <- replace(rotowire.a, rotowire.a == 'Wisconsin-Green Bay', 'Green Bay')
rotowire.a <- replace(rotowire.a, rotowire.a == 'Wisconsin-Milwaukee', 'Milwaukee')
rotowire.a <- replace(rotowire.a, rotowire.a == 'IPFW', 'Purdue Fort Wayne')
rotowire.a <- replace(rotowire.a, rotowire.a == 'Illinois-Chicago', 'Illinois Chicago')
rotowire.a <- replace(rotowire.a, rotowire.a == 'California Baptist', 'Cal Baptist')
rotowire.a <- replace(rotowire.a, rotowire.a == 'Cal-Poly San Luis Obispo', 'Cal Poly')
rotowire.a <- replace(rotowire.a, rotowire.a == 'Citadel', 'The Citadel')
rotowire.a <- replace(rotowire.a, rotowire.a == 'Tennessee-Martin', 'Tennessee Martin')
rotowire.a <- replace(rotowire.a, rotowire.a == 'St. Francis (NY)', 'St. Francis NY')
rotowire.a <- replace(rotowire.a, rotowire.a == 'E. Tennessee St.', 'East Tennessee St.')
rotowire.a <- replace(rotowire.a, rotowire.a == 'Texas A&M-Corpus Christi', 'Texas A&M Corpus Chris')
rotowire.a <- replace(rotowire.a, rotowire.a == 'Southern Mississippi', 'Southern Miss')
rotowire.a <- replace(rotowire.a, rotowire.a == 'Arkansas-Little Rock', 'Little Rock')
rotowire.a <- replace(rotowire.a, rotowire.a == 'Miami', 'Miami FL')
rotowire.a <- replace(rotowire.a, rotowire.a == 'Central Florida', '')
rotowire.a <- replace(rotowire.a, rotowire.a == 'Arkansas-Pine Bluff', 'Arkansas Pine Bluff')
rotowire.a <- replace(rotowire.a, rotowire.a == 'Loyola (MD)', 'Loyola MD')
rotowire.a <- replace(rotowire.a, rotowire.a == 'Texas Rio Grande Valley', 'UT Rio Grande Valley')
rotowire.a <- replace(rotowire.a, rotowire.a == 'Loyola-Chicago', 'Loyola Chicago')
rotowire.a <- replace(rotowire.a, rotowire.a == 'Middle Tennessee St.', 'Middle Tennessee')
rotowire.a <- replace(rotowire.a, rotowire.a == 'Maryland-Baltimore County', '')
rotowire.a <- replace(rotowire.a, rotowire.a == "St. Joseph's", "Saint Joseph's")
rotowire.a <- replace(rotowire.a, rotowire.a == 'St. Francis (Pa.)', 'St. Francis PA')
rotowire.a <- replace(rotowire.a, rotowire.a == 'Massachusetts-Lowell', 'UMass Lowell')
rotowire.a <- replace(rotowire.a, rotowire.a == 'Florida International', 'FIU')
rotowire.a <- replace(rotowire.a, rotowire.a == 'North Carolina St.', 'N.C. State')
rotowire.a <- replace(rotowire.a, rotowire.a == 'Miami (OH)', 'Miami OH')
rotowire.a <- replace(rotowire.a, rotowire.a == 'Pennsylvania', 'Penn')
rotowire.a <- replace(rotowire.a, rotowire.a == 'LIU Brooklyn', 'LIU')
rotowire.a <- replace(rotowire.a, rotowire.a == '', '')
rotowire.a <- replace(rotowire.a, rotowire.a == '', '')

###DraftKings slate from rotowire###

rotowire.a.away <- rotowire.a[seq(1, nrow(rotowire.a), 2), ]
rotowire.a.home <- rotowire.a[seq(2, nrow(rotowire.a), 2), ]

rotowire.a.away$DateTimeScorePoints <- paste(rotowire.a.away$DateTime, rotowire.a.away$Opp.Score, rotowire.a.away$Total.Points)
rotowire.a.home$DateTimeScorePoints <- paste(rotowire.a.home$DateTime, rotowire.a.home$Implied.Score, rotowire.a.home$Total.Points)

rotowire.a.away <- rotowire.a.away %>% 
  left_join(kp.a, by = c("Squad" = "TeamName"))

rotowire.a.home <- rotowire.a.home %>% 
  left_join(kp.a, by = c("Squad" = "TeamName"))

rotowire.a.slate <- rotowire.a.away %>% 
  left_join(rotowire.a.home, by = c("DateTimeScorePoints" = "DateTimeScorePoints")) 

rotowire.a.slate[, c(5:9)] <- sapply(rotowire.a.slate[, c(5:9)], as.numeric)

rotowire.a.slate$AdjEM.diff <- (rotowire.a.slate$AdjEM.y - rotowire.a.slate$AdjEM.x)
rotowire.a.slate$Cover.plus.AdjEM <- rotowire.a.slate$Cover.y + rotowire.a.slate$AdjEM.diff

rotowire.a.slate$AdjTempo.diff <- rotowire.a.slate$AdjTempo.y - rotowire.a.slate$AdjTempo.x
rotowire.a.slate$AdjTempo.sum <- rotowire.a.slate$AdjTempo.y + rotowire.a.slate$AdjTempo.x
rotowire.a.slate$Adjtempo_minus_Points <- rotowire.a.slate$AdjTempo.sum - rotowire.a.slate$Total.Points.y
rotowire.a.slate$Home.Squad.Cover <- paste(rotowire.a.slate$Squad.y, rotowire.a.slate$Cover.y)
rotowire.a.slate$Away_Home <- paste(rotowire.a.slate$Squad.x, rotowire.a.slate$Squad.y)

###Scores###

cbb_scores <- read.csv("cbb.scores.csv", header = F)

cbb_scores <- cbb_scores %>% 
  mutate(across(where(is.character), str_trim))

cbb_scores <- mutate_if(cbb_scores, is.character, str_replace_all, pattern = "State", replacement = "St.")
cbb_scores <- mutate_if(cbb_scores, is.character, str_replace_all, pattern = "-", replacement = " ")
cbb_scores <- mutate_if(cbb_scores, is.character, str_replace_all, pattern = "[(1)]", replacement = "")
cbb_scores <- mutate_if(cbb_scores, is.character, str_replace_all, pattern = "[(2)]", replacement = "")
cbb_scores <- mutate_if(cbb_scores, is.character, str_replace_all, pattern = "[(3)]", replacement = "")
cbb_scores <- mutate_if(cbb_scores, is.character, str_replace_all, pattern = "[(4)]", replacement = "")
cbb_scores <- mutate_if(cbb_scores, is.character, str_replace_all, pattern = "[(5)]", replacement = "")
cbb_scores <- mutate_if(cbb_scores, is.character, str_replace_all, pattern = "[(6)]", replacement = "")
cbb_scores <- mutate_if(cbb_scores, is.character, str_replace_all, pattern = "[(7)]", replacement = "")
cbb_scores <- mutate_if(cbb_scores, is.character, str_replace_all, pattern = "[(8)]", replacement = "")
cbb_scores <- mutate_if(cbb_scores, is.character, str_replace_all, pattern = "[(9)]", replacement = "")
cbb_scores <- mutate_if(cbb_scores, is.character, str_replace_all, pattern = "[(10)]", replacement = "")
cbb_scores <- mutate_if(cbb_scores, is.character, str_replace_all, pattern = "[(11)]", replacement = "")
cbb_scores <- mutate_if(cbb_scores, is.character, str_replace_all, pattern = "[(12)]", replacement = "")
cbb_scores <- mutate_if(cbb_scores, is.character, str_replace_all, pattern = "[(13)]", replacement = "")
cbb_scores <- mutate_if(cbb_scores, is.character, str_replace_all, pattern = "[(14)]", replacement = "")
cbb_scores <- mutate_if(cbb_scores, is.character, str_replace_all, pattern = "[(15)]", replacement = "")
cbb_scores <- mutate_if(cbb_scores, is.character, str_replace_all, pattern = "[(16)]", replacement = "")
cbb_scores <- mutate_if(cbb_scores, is.character, str_replace_all, pattern = "[(17)]", replacement = "")
cbb_scores <- mutate_if(cbb_scores, is.character, str_replace_all, pattern = "[(18)]", replacement = "")
cbb_scores <- mutate_if(cbb_scores, is.character, str_replace_all, pattern = "[(19)]", replacement = "")
cbb_scores <- mutate_if(cbb_scores, is.character, str_replace_all, pattern = "[(20)]", replacement = "")
cbb_scores <- mutate_if(cbb_scores, is.character, str_replace_all, pattern = "[(21)]", replacement = "")
cbb_scores <- mutate_if(cbb_scores, is.character, str_replace_all, pattern = "[(22)]", replacement = "")
cbb_scores <- mutate_if(cbb_scores, is.character, str_replace_all, pattern = "[(23)]", replacement = "")
cbb_scores <- mutate_if(cbb_scores, is.character, str_replace_all, pattern = "[(24)]", replacement = "")
cbb_scores <- mutate_if(cbb_scores, is.character, str_replace_all, pattern = "[(25)]", replacement = "")
cbb_scores <- mutate_if(cbb_scores, is.character, str_replace_all, pattern = "[()]", replacement = "")

cbb_scores <- replace(cbb_scores, cbb_scores == "California Baptist", "Cal Baptist")
cbb_scores <- replace(cbb_scores, cbb_scores == "St. Thomas (MN)", "St. Thomas")
cbb_scores <- replace(cbb_scores, cbb_scores == "Detroit", "Detroit Mercy")
cbb_scores <- replace(cbb_scores, cbb_scores == "Albany (NY)", "Albany")
cbb_scores <- replace(cbb_scores, cbb_scores == "NC St.", "N.C. State")
cbb_scores <- replace(cbb_scores, cbb_scores == "Pitt", "Pittsburgh")
cbb_scores <- replace(cbb_scores, cbb_scores == "Buffalo St.", "Buffalo")
cbb_scores <- replace(cbb_scores, cbb_scores == "", "")
cbb_scores <- replace(cbb_scores, cbb_scores == "", "")
cbb_scores <- replace(cbb_scores, cbb_scores == "", "")

scores.away <- cbb_scores[seq(1, nrow(cbb_scores), 2), ]
names(scores.away) <- c("Away_Team","Away_Score","Final")
scores.away <- scores.away %>% 
  mutate(across(where(is.character), str_trim))

scores.home <- cbb_scores[seq(2, nrow(cbb_scores), 2), ]
names(scores.home) <- c("Home_Team", "Home_Score", "OT")
scores.home <- scores.home %>% 
  mutate(across(where(is.character), str_trim))

###Slate Scores###

scores.slate <- tibble(scores.away, scores.home)
scores.slate$Total <- scores.slate$Away_Score + scores.slate$Home_Score
scores.slate$Away_Home <- paste(scores.slate$Away_Team, scores.slate$Home_Team)

scores.slate  <- rotowire.a.slate %>% 
  left_join(scores.slate, by = "Away_Home") 

#scores.slate  <- scores.slate %>% 
  #left_join(rotowire.a.slate, by = "Away_Home")

scores.slate$Spread.pick <- if_else((scores.slate$Cover.y + scores.slate$AdjEM.diff)<0,scores.slate$Cover.x, scores.slate$Cover.y)
scores.slate$dog.minus.fav <- if_else(scores.slate$Cover.y < 0, scores.slate$Away_Score - scores.slate$Home_Score, scores.slate$Home_Score - scores.slate$Away_Score)
scores.slate$Spread.pick.correct <- if_else(scores.slate$Spread.pick < scores.slate$dog.minus.fav, 0,1)

scores.slate$Total.minus.Total.Points.y <- scores.slate$Total - scores.slate$Total.Points.y
scores.slate$Delta <- scores.slate$Adjtempo_minus_Points - scores.slate$Total.minus.Total.Points.y

scores.slate$Over_under <- if_else(scores.slate$Total.minus.Total.Points.y < 0,"Under","Over", missing = "")
scores.slate$AdjTempo.sum.correct <- if_else(sign(scores.slate$Adjtempo_minus_Points) == sign(scores.slate$Total.minus.Total.Points.y),1,0)

scores.slate %>% 
  select(RankAdjEM.x,
         Squad.x,
         Away_Score,
         RankAdjEM.y,
         Squad.y,
         Home_Score,
         Cover.y,
         AdjEM.diff, 
         Cover.plus.AdjEM,
         Spread.pick,
         dog.minus.fav,
         Spread.pick.correct,
         Total.Points.y,
         AdjTempo.sum, 
         Adjtempo_minus_Points,
         Total,
         Total.minus.Total.Points.y,
         Over_under,
         AdjTempo.sum.correct,
         Delta) %>% 
  arrange(-Adjtempo_minus_Points) %>% 
  adorn_totals("row", na.rm = T) %>% 
  view(title = "CBB Slate Results")

scores.slate.subset <- unlist(lapply(scores.slate, is.numeric))
colSums(scores.slate[ ,scores.slate.subset], na.rm = T)


###Test code###

#x <- "~!@#$%^&*(){}_+:\"<>?,./;'[]-="
#str_replace_all(x, "[[:punct:]]", "")

#str_replace_all(cbb_scores,"()","")

#df <- data.frame(name = rep(letters[1:3], each = 3), var1 = rep('""', 9), var2 = rep('<3', 9))
#df %>% 
  #mutate(var1 = str_replace(var1, "[()]", ""))

#x = ("(40.703707008, -73.943257966)")
#cbb_scores <- gsub("[()]", "", cbb_scores)

#x
#sapply(gregexpr("[A-Z]", x), `[`, 1)

#setwd("C://Users//Mike Francis//Documents//")