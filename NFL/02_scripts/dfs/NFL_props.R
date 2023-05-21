###Prop Tool###

props <- read.csv("nfl-best-bets.csv")

props$sideOneValue <- round(props$sideOneValue, digits = 3)
props$sideTwoValue <- round(props$sideTwoValue, digits = 3)

props %>% 
  select(player, 
         position, 
         line,
         propType,
         sideOneType, 
         sideOneOdds, 
         sideOneValue) %>% 
  arrange(-sideOneValue)
view(title = "Overs")

props %>% 
  select(player, 
         position, 
         line,
         propType,
         sideTwoType, 
         sideTwoOdds, 
         sideTwoValue) %>%
  arrange(-sideTwoValue)
view(title = "Unders")