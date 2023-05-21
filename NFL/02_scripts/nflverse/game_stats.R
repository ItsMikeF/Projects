#post game

#load packages
suppressMessages({
  library(nflfastR) #nflfastr nflseedr nflplotr
  library(tidyverse) #ggplot2 dplyr tibble tidyr purrr forecats 
  library(ggrepel) #automatically position non-overlapping text labels
  library(glue) #interpreted literal strings
})

pbp <- load_pbp(2022)

rushers <- pbp %>%
  filter(season_type == "POST") %>% 
  group_by(game_date, rusher, posteam, defteam) %>% 
  summarise(n=n(), 
            rushing_yards = sum(rushing_yards, na.rm = T),
            rush_attempt = sum(rush_attempt, na.rm = T),
            rush_touchdown = sum(rush_touchdown, na.rm = T),
            fumble_lost = sum(fumble_lost, na.rm = T)) %>% 
  mutate(join =paste0(game_date,rusher))
            
passers <- pbp %>%
  filter(season_type == "POST") %>% 
  group_by(game_date, passer, posteam, defteam) %>% 
  summarise(n=n(), 
            pass_attempt = sum(pass_attempt, na.rm = T),
            completions = sum(complete_pass, na.rm = T),
            passing_yards = sum(passing_yards, na.rm = T),
            pass_touchdown = sum(pass_touchdown, na.rm = T),
            interception = sum(interception, na.rm = T),
            
            rushing_yards = sum(rushing_yards, na.rm = T),
            rush_attempt = sum(rush_attempt, na.rm = T),
            rush_touchdown = sum(rush_touchdown, na.rm = T),
            fumble_lost = sum(fumble_lost, na.rm = T),
            
            epa = round(mean(qb_epa), digits = 3),
            cpoe = round(mean(cpoe, na.rm = T), digits = 2)
  ) %>% 
  mutate(join =paste0(game_date,passer)) %>% 
  left_join(rushers[,6:10], by=c('join')) %>% 
  mutate(rush_attempt= sum(rush_attempt.x,rush_attempt.y, na.rm = T),
         rushing_yards= sum(rushing_yards.x,rushing_yards.y,na.rm = T),
         rush_touchdown = sum(rush_touchdown.x,rush_touchdown.y,na.rm = T),
         fumble_lost = sum(fumble_lost.x,fumble_lost.y,na.rm = T)) %>% 
  mutate(big_py = ifelse(passing_yards > 300, 1,0),
         big_ry = ifelse(rushing_yards >100, 1, 0),
         ypa = round(passing_yards/pass_attempt, digits = 1),
         fpts = 
           pass_touchdown * 4 +
           passing_yards * .04 +
           interception * -1 +
           rushing_yards * .1 +
           rush_touchdown * 6 +
           fumble_lost * -1 +
           big_py * 3 +
           big_ry * 3, 
         qbr = 
           round((((completions/pass_attempt-.3)*0.5 +
           (passing_yards/pass_attempt-3)*.25 +
           (pass_touchdown/pass_attempt*20) +
           (2.375-(interception/pass_attempt*25)))/6)*100,digits = 1)
         ) %>%
  filter(n>10) %>% 
  drop_na(passer) %>% 
  ungroup() %>% 
  arrange(-fpts) %>% 
  select(game_date, passer, posteam, defteam, epa, fpts, epa, cpoe, ypa, qbr)
         #pass_touchdown, passing_yards, interception, rushing_yards, rush_touchdown, fumble_lost, big_py, big_ry)

#qbr reference
#https://www.sports-king.com/how-passer-rating-qbr-calculated-3198/#:~:text=d%20%3D%202.375%20%2D%20(Interceptions%2FAttempts%20*%2025)&text=Let's%20run%20some%20real%20numbers%20to%20arrive%20at%20a%20real%20QBR.