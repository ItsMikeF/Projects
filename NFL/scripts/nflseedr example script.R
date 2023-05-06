#trying to learn this script from this link
#https://nflseedr.com/articles/nflsim.html

library(nflseedR)
library(dplyr)
options(digits = 3)

set.seed(4)
sims <- simulate_nfl(
  nfl_season = 2020, 
  fresh_season = T, 
  simulations = 100
)

sims$teams %>% 
  dplyr::filter(team == "CHI") %>% 
  select(sim, team, wins, seed, draft_order) %>% 
  head(6) %>% 
  knitr:: kable()

sims$games %>% filter(sim == 1, game_type != "REG") %>% knitr::kable()

summary(sims)
