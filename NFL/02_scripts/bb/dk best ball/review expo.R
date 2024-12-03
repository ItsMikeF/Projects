# review nfl dk exposures

# load packages
library(tidyverse)

# load data
load("./01_data/exposures/dk/dk_exposures.RData")

# group by
write.csv(expo, file = "expo.csv")

# divide by position groups
pos <- expo %>% 
  filter(slotName == "TE")

# group by
teams <- expo %>% 
  group_by(teamName) %>% 
  summarise(m=sum(n)) %>% 
  arrange(-m)

write.csv(teams, file = "teams.csv")
