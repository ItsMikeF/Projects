library(tidyverse)

kp <- read.csv("summary22.csv")
bart <- read.csv("Bart_projections.csv")
biffs <- read.csv("Roster_Strength.csv")

biffs$Roster_rank <- round(rank(-biffs$Roster.Strength), digits = 0)
bart$BARTHAG_rank <- round(rank(-bart$BARTHAG), digits = 0)
bart$TALENT_rank <- round(rank(-bart$TALENT), digits = 0)

kp %>%
  left_join(biffs, by = c('TeamName' = 'Team')) %>%
  left_join(bart, by = c('TeamName' = 'TEAM')) %>%
  select(TeamName, 
         AdjEM,
         RankAdjEM, 
         Roster.Strength,
         Roster_rank,
         BARTHAG, 
         BARTHAG_rank,
         RET.MINS, 
         TALENT, 
         TALENT_rank,
         AdjTempo,
         RankAdjTempo) %>%
  arrange(-AdjEM) %>%
  #filter(TeamName == "Duke" | TeamName == "Kentucky") %>%
  view(title = "Kenpom") %>%
  write.csv(kp, file = "test.csv")

kp %>%
  mutate_if(is.numeric(),round(digits = 2))