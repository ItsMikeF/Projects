library(tidyverse)

user <- unlist(strsplit(getwd(), "/"))
user <- user[3]

setwd(paste("C://Users//",user,"//Documents//Github//DFS_Data//Data_CBB//2022MM", sep = ""))

### Import Kenpom Era data ###
kenpom <- read.csv("Kenpom.csv")

### Filter kenpom for current year and NCAA seeds ###
kenpom_2022 <- kenpom %>% 
  filter(year == 2022) %>% 
  drop_na(ncaa_seed)

### Add metric for back testing###
kenpom_2022$metric <- round(kenpom_2022$adj_em^2 * kenpom_2022$sos_adj_em, digits = 1)

###Get the bracket###

bracket <- read.csv("2022MM bracket.csv", header = F)
names(bracket) <- c("rank", "team")
bracket <- tibble(bracket[,2])
names(bracket) <- "team"

###Round of 64 ###

r64 <- bracket %>% 
  left_join(kenpom_2022, by=c("team"))

favorites <- r64[seq(1, nrow(r64), 2), ]
dogs <- r64[seq(2, nrow(r64), 2), ]
names(dogs) <- paste(names(dogs),".y", sep = "")

r64 <- cbind(favorites, dogs)

r64$winner <- if_else(r64$metric > r64$metric.y, r64$team, r64$team.y)

###Round of 32 ###

r32 <- tibble(r64$winner)
names(r32) <- "team"

r32 <- r32 %>% 
  left_join(kenpom_2022, by = c("team"))

favorites <- r32[seq(1, nrow(r32), 2), ]
dogs <- r32[seq(2, nrow(r32), 2), ]
names(dogs) <- paste(names(dogs),".y", sep = "")

r32 <- cbind(favorites, dogs)
r32$winner <- if_else(r32$metric > r32$metric.y, r32$team, r32$team.y)

### Round of 16 ###

r16 <- tibble(r32$winner)
names(r16) <- "team"

r16 <- r16 %>% 
  left_join(kenpom_2022, by = c("team"))

favorites <- r16[seq(1, nrow(r16), 2), ]
dogs <- r16[seq(2, nrow(r16), 2), ]
names(dogs) <- paste(names(dogs),".y", sep = "")

r16 <- cbind(favorites, dogs)

r16$winner <- if_else(r16$metric > r16$metric.y, r16$team, r16$team.y)

### Elite Eight ###

e8 <- tibble(r16$winner)
names(e8) <- "team"

e8 <- e8 %>% 
  left_join(kenpom_2022, by = c("team"))

favorites <- e8[seq(1, nrow(e8), 2), ]
dogs <- e8[seq(2, nrow(e8), 2), ]
names(dogs) <- paste(names(dogs),".y", sep = "")

e8 <- cbind(favorites, dogs)

e8$winner <- if_else(e8$metric > e8$metric.y, e8$team, e8$team.y)

### Final Four ###

f4 <- tibble(e8$winner)
names(f4) <- "team"

f4 <- f4 %>% 
  left_join(kenpom_2022, by = c("team"))

favorites <- f4[seq(1, nrow(f4), 2), ]
dogs <- f4[seq(2, nrow(f4), 2), ]
names(dogs) <- paste(names(dogs),".y", sep = "")

f4 <- cbind(favorites, dogs)

f4$winner <- if_else(f4$metric > f4$metric.y, f4$team, f4$team.y)

### c2 ###

c2 <- tibble(f4$winner)
names(c2) <- "team"

c2 <- c2 %>% 
  left_join(kenpom_2022, by = c("team"))

favorites <- c2[seq(1, nrow(c2), 2), ]
dogs <- c2[seq(2, nrow(c2), 2), ]
names(dogs) <- paste(names(dogs),".y", sep = "")

c2 <- cbind(favorites, dogs)

c2$winner <- if_else(c2$metric > c2$metric.y, c2$team, c2$team.y)

###Predicted Bracket ###

rounds <- c("r64", "r32", "r16", "e8", "f4", "c2")
predictions <- data.frame(matrix(ncol = 6, nrow = 32))
names(predictions) <- rounds

for (round in rounds){
  predictions[,(which(rounds == round))] <- get(round)[,51]
}

view(predictions)
rm(c2, dogs, e8, f4, favorites, r16, r32, r64, round, rounds, user)