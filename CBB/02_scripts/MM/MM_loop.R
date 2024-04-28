# not sure what this does yet

library(tidyverse)

### Import Kenpom Era data ###
kenpom <- read.csv("./Training_data/2022MM/Kenpom.csv")

### Filter kenpom for current year and NCAA seeds ###
kenpom_2022 <- kenpom %>% 
  filter(year == 2022) %>% 
  drop_na(ncaa_seed)

### Add metric for back testing###
kenpom_2022$metric <- round(kenpom_2022$adj_em^2 * kenpom_2022$sos_adj_em, digits = 1)

###Get the bracket###

bracket <- read.csv("./Training_data/2022MM/2022MM bracket.csv", header = F)
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

###Bracket loop

rounds <- c("r64", "r32", "r16", "e8", "f4", "c2")
rounds_loop <- c("r32", "r16", "e8", "f4", "c2")

for(round in rounds_loop[2:5]){
  
  assign(round, get(rounds[(which(rounds == round)-1)])[,51])
  
  #%>% left_join(kenpom_2022, by=c("get(round)" = "team")
  
  favorites <- round[seq(1, nrow(round), 2), ]
  dogs <- round[seq(2, nrow(round), 2), ]
  names(dogs) <- paste(names(dogs),".y", sep = "")
  
  get(round) <- cbind(favorites, dogs)
  round$winner <- if_else(round$metric > round$metric.y, round$team, round$team.y)
}

assign(rounds_loop[2], tibble(get(rounds[(which(rounds == rounds_loop[2])-1)])[,51]))
rm(r16)
tibble(get(rounds[(which(rounds == "r16")-1)])[,51])
