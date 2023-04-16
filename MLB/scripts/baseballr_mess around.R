#lets mess around with bassballr

#libraries
library(tidyverse)
library(baseballr)

# 2.0 Baseball Reference -------------------------------------------------------------

bref_standings_on_date(Sys.Date(), "NL East", from = F)

# 2.1 BR bats ---------------------------------------------


#download all data from baseball reference
batters <- bref_daily_batter("2023-03-30", Sys.Date()) %>% 
  mutate(K_rate = SO/PA, 
         BB_rate = BB/PA, 
         fpts = (X1B*3) + (X2B*6) + (X3B*8) + (HR*10) + (BB*3) + (HBP*3) + (RBI*3) + (R*2) + (SB*2))

#filter for qualified batters
#based on fangraphs figure for number of qualifed bats
batters_q <- batters %>% 
  filter(PA > 0.6*max(PA)) 

#lets look at team level data
batters %>% 
  #drop_na() %>% 
  group_by(Team, Level) %>% 
  summarise(fpts = sum(fpts, na.rm=T), 
            BA = weighted.mean(BA, AB), 
            OBP = mean(OBP, na.rm = T), 
            SLG = mean(SLG, na.rm = T), 
            OPS = mean(SLG, na.rm = T), 
            HR = sum(HR), 
            SB = sum(SB)) %>% 
  arrange(-fpts) %>% 
  print(n=30)

#lets look at league averages
batters_q %>% 
  summarise(BA = mean(BA),
            OBP = mean(OBP), 
            SLG = mean(SLG), 
            OPS = mean(OPS), 
            H = mean(H), 
            HR = mean(HR),
            RBI = mean(RBI), 
            BB = mean(BB),
            SO = mean(SO), 
            K_rate = mean(K_rate, na.rm = T), 
            BB_rate = mean(BB_rate, na.rm = T)) %>% 
  ungroup()

batters_q %>% 
  arrange(-BA) %>% 
  select(Name, Team, PA, BA, OBP, SLG, OPS, K_rate, BB_rate, fpts)


# 2.2 BR Pitchers ------------------------------------------------------------

#load pitcher data from baseball reference
pitchers <- bref_daily_pitcher("2023-03-30", Sys.Date()) %>% 
  mutate(fpts = (W*5) + (SO*3) + (IP*3) + (ER*-3))
#pitchers <- bref_daily_pitcher(Sys.Date()-1, Sys.Date()0

#filter for qualified pitchers
pitchers_q <- pitchers %>% filter(IP > 0.65*max(IP))

pitchers_q %>% 
  select(Name, GS, IP, fpts, H, R, ER, SO, ERA, WHIP, BAbip, GB.FB, LD, PU) %>% 
  arrange(BAbip)


# 3.0 Fangraphs -----------------------------------------------------------


#fg batter ledaers
fg_batters <- fg_batter_leaders(2023, 2023, qual = "y", ind = 1, exc_p = T)

#park factors
fg_park <- fg_park(2022)

fg_park_hand <- fg_park_hand(2022)

fg_batter_game_logs <- fg_batter_game_logs(18568, 2023)

list <- lapply(fg_batters$playerid[1:10], function(id) fg_batter_game_logs(id, 2023))

list <- map(fg_batters$playerid[1:10], ~ fg_batter_game_logs(.x, 2023))

fg_pitcher <- fg_pitch_leaders(2023, 2023, league = "all", qual = "y", pitcher_type = "pit", ind = 1)
