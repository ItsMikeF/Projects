library(tidyverse)
library(ggrepel)
library(lubridate)

setwd("C://Users//Mike Francis//Documents//Projects//DFS_Data//Data_UFC//2022-03-05")

###UFC Slate###

ufc <- read.csv("DKsalaries.csv")

ufc <- replace(ufc, ufc == "FALSE", "F")

ufc <- ufc %>%
  separate(Game.Info, c("Away", "String"), sep = "@") %>%
  separate(String, c("Home", "Date", "Time"), sep = " ")

ufc$Opponent <- if_else(ufc$Home == ufc$TeamAbbrev, ufc$Away, ufc$Home)
ufc$points_rank <- round(rank(-ufc$AvgPointsPerGame), digits = 0)

###UFC Lineup###

#ufc_lineup <- (runif(6, 1, 22), digits = 0)
#ufc_lineup <- ufc_lineup %>% 
  #left_join(ufc, by = c(""))

ufc_lineup2 <- data.frame(fighter = as.character(sample(unique(ufc$Name), 6, replace = TRUE)), stringsAsFactors = FALSE)

ufc_lineup2 <- ufc_lineup2 %>% 
  left_join(ufc, by = c("fighter" = "Name"))

ufc_lineup2 <- ufc_lineup2 %>% 
  select(fighter, 
         Salary, 
         AvgPointsPerGame, 
         Opponent)
 
ufc_test <- bind_rows(summarise(ufc_lineup2,
                                across(where(is.numeric), sum),
                                across(where(is.character), ~"")))
ufc_test <- ufc_test[,1:2]

#ufc_table <- tibble()

ufc_table <- rbind(ufc_table, ufc_test)
max(ufc_table$AvgPointsPerGame)

ufc_lineup2 <- ufc_lineup2 %>% 
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(where(is.character), ~""))) %>% 
  view(title = "UFC Lineup")

lineups <- combn(ufc$AvgPointsPerGame, 6)

###Combinations###

combinations <- function(n,k){
  factorial(n) / (factorial(k)*factorial(n-k))
}

combinations(22, 6)
combinations(dim(ufc)[1], 6)

###Permutations###

permutations <- function(n, r){
  factorial(n) / factorial(n-r)
}

permutations(dim(ufc)[1], 6)

###While loop to build optimal lineup###

#ufc_table <- tibble()

loop_test <- tibble()

counter <- 1

while(ufc_test[1,1]  < 50000) {
  ufc_lineup2 <- data.frame(fighter = as.character(sample(unique(ufc$Name), 6, replace = TRUE)), stringsAsFactors = FALSE)
  
  ufc_lineup2 <- ufc_lineup2 %>% 
    left_join(ufc, by = c("fighter" = "Name"))
  
  ufc_lineup2 <- ufc_lineup2 %>% 
    select(fighter, 
           Salary, 
           AvgPointsPerGame, 
           Opponent)
  
  ufc_test <- bind_rows(summarise(ufc_lineup2,
                                  across(where(is.numeric), sum),
                                  across(where(is.character), ~"")))
  if (ufc_test[1,1] < 50000){
    loop_test <- rbind(loop_test, ufc_test[1,2])
    counter <- counter + 1
  }
}