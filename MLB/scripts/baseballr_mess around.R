#lets mess around with bassballr

#libraries
library(tidyverse)
library(baseballr)

# 1.0 Standings -----------------------------------------------------------


bref_standings_on_date(Sys.Date(), "NL East", from = F)

# 2.0 Batters -------------------------------------------------------------


#download all data from baseball reference
batters <- bref_daily_batter("2023-03-30", Sys.Date())

#filter for qualified batters
#based on fangraphs figure for number of qualifed bats
batters_q <- batters %>% 
  filter(PA > 0.6*max(PA)) %>% 
  mutate(K_rate = SO/PA, 
         BB_rate = BB/PA)

#lets look at team level data
batters %>% 
  #drop_na() %>% 
  group_by(Team, Level) %>% 
  summarise(BA = weighted.mean(BA, AB), 
            OBP = mean(OBP, na.rm = T), 
            SLG = mean(SLG, na.rm = T), 
            OPS = mean(SLG, na.rm = T), 
            HR = sum(HR), 
            SB = sum(SB)) %>% 
  arrange(-HR) %>% 
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
  select(Name, Team, PA, BA, OBP, SLG, OPS, K_rate, BB_rate)

#fg batter ledaers
fb_bats <- fg_batter_leaders(x = 2023, y = 2023, qual = 400)

# 3.0 Pitchers ------------------------------------------------------------

#load pitcher data from baseball reference
pitchers <- bref_daily_pitcher("2023-03-30", Sys.Date()) %>% 
  mutate(fpts = (W*5) + (SO*3) + (IP*3) + (ER*-3))
#pitchers <- bref_daily_pitcher(Sys.Date()-1, Sys.Date()0

#filter for qualified pitchers
pitchers_q <- pitchers %>% filter(IP > 0.65*max(IP))

pitchers_q %>% 
  select(Name, GS, IP, fpts, H, R, ER, SO, ERA, WHIP, BAbip, GB.FB, LD, PU) %>% 
  arrange(BAbip)

