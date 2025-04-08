# bracketology

library(tidyverse)
library(lubridate)
library(stats)


bracket <- read.csv("/Training_data/2022MM/2022MM bracket.csv", header = F)
names(bracket) <- c("rank", "team")

bracket <- replace(bracket, bracket == 'Uconn', 'Connecticut')
bracket <- replace(bracket, bracket == 'CS Fullerton', 'Cal St. Fullerton')
bracket <- replace(bracket, bracket == 'Miami(FL)', 'Miami FL')

kp <- read.csv("./01_data/mm/2025/summary25.csv")

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
  
###Round of 64 ###

bracket <- bracket %>% 
  left_join(kp, by=c("team" = "TeamName"))

favorites <- bracket[seq(1, nrow(bracket), 2), ]
dogs <- bracket[seq(2, nrow(bracket), 2), ]
names(dogs) <- paste(names(dogs),".y", sep = "")

round_of_64 <- cbind(favorites, dogs)

round_of_64$winner <- if_else(round_of_64$AdjEM > round_of_64$AdjEM.y, round_of_64$team, round_of_64$team.y)

###Round of 32 ###
round_of_32 <- tibble(round_of_64$winner)
names(round_of_32) <- "Team"

round_of_32 <- round_of_32 %>% 
  left_join(kp, by = c("Team" = "TeamName"))

favorites <- round_of_32[seq(1, nrow(round_of_32), 2), ]
dogs <- round_of_32[seq(2, nrow(round_of_32), 2), ]
names(dogs) <- paste(names(dogs),".y", sep = "")

round_of_32 <- cbind(favorites, dogs)

round_of_32$winner <- if_else(round_of_32$AdjEM > round_of_32$AdjEM.y, round_of_32$Team, round_of_32$Team.y)

### Round of 16 ###
round_of_16 <- tibble(round_of_32$winner)
names(round_of_16) <- "Team"

round_of_16 <- round_of_16 %>% 
  left_join(kp, by = c("Team" = "TeamName"))

favorites <- round_of_16[seq(1, nrow(round_of_16), 2), ]
dogs <- round_of_16[seq(2, nrow(round_of_16), 2), ]
names(dogs) <- paste(names(dogs),".y", sep = "")

round_of_16 <- cbind(favorites, dogs)

round_of_16$winner <- if_else(round_of_16$AdjEM > round_of_16$AdjEM.y, round_of_16$Team, round_of_16$Team.y)

### Elite Eight ###
elite_eight <- tibble(round_of_16$winner)
names(elite_eight) <- "Team"

elite_eight <- elite_eight %>% 
  left_join(kp, by = c("Team" = "TeamName"))

favorites <- elite_eight[seq(1, nrow(elite_eight), 2), ]
dogs <- elite_eight[seq(2, nrow(elite_eight), 2), ]
names(dogs) <- paste(names(dogs),".y", sep = "")

elite_eight <- cbind(favorites, dogs)

elite_eight$winner <- if_else(elite_eight$AdjEM > elite_eight$AdjEM.y, elite_eight$Team, elite_eight$Team.y)

### Final Four ###
final_four <- tibble(elite_eight$winner)
names(final_four) <- "Team"

final_four <- final_four %>% 
  left_join(kp, by = c("Team" = "TeamName"))

favorites <- final_four[seq(1, nrow(final_four), 2), ]
dogs <- final_four[seq(2, nrow(final_four), 2), ]
names(dogs) <- paste(names(dogs),".y", sep = "")

final_four <- cbind(favorites, dogs)

final_four$winner <- if_else(final_four$AdjEM > final_four$AdjEM.y, final_four$Team, final_four$Team.y)

### Championship ###

championship <- tibble(final_four$winner)
names(championship) <- "Team"

championship <- championship %>% 
  left_join(kp, by = c("Team" = "TeamName"))

favorites <- championship[seq(1, nrow(championship), 2), ]
dogs <- championship[seq(2, nrow(championship), 2), ]
names(dogs) <- paste(names(dogs),".y", sep = "")

championship <- cbind(favorites, dogs)

championship$winner <- if_else(championship$AdjEM > championship$AdjEM.y, championship$Team, championship$Team.y)

###Bracket Predictions ###

predictions <- tibble(bracket$team)
names(predictions) <- "Round of 64"
predictions <- cbind(predictions, round_of_64$winner)
predictions <- cbind(predictions, round_of_32$winner)
predictions <- cbind(predictions, round_of_16$winner)
predictions <- cbind(predictions, elite_eight$winner)
predictions <- cbind(predictions, final_four$winner)
predictions <- cbind(predictions, championship$winner)