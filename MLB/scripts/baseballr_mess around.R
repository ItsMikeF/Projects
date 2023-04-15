#lets mess around with bassballr

#libraries
library(tidyverse)
library(baseballr)

bref_standings_on_date("2023-04-08", "NL East", from = F)

batters <- bref_daily_batter("2023-03-30", Sys.Date())
#batters <- bref_daily_batter(Sys.Date()-1, Sys.Date())

batters %>% 
  #drop_na() %>% 
  group_by(Team, Level) %>% 
  summarise(BA = weighted.mean(BA, AB), 
            OBP = mean(OBP), 
            SLG = mean(SLG), 
            OPS = mean(SLG), 
            HR = sum(HR), 
            SB = sum(SB)) %>% 
  arrange(-BA) %>% 
  print(n=30)

batters %>% 
  filter(PA > 0.25*max(batters$PA)) %>% 
  mutate(K_rate = SO/PA, 
         BB_rate = BB/PA) %>% 
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

pitchers <- bref_daily_pitcher("2023-03-30", Sys.Date())
pitchers <- bref_daily_pitcher(Sys.Date()-1, Sys.Date())

pitchers %>% 
  arrange(-SO) %>% 
  slice_head(n=10)