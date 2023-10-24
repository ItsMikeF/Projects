#blitz rate from sportsref
sportsref <- function(week){
  #https://www.pro-football-reference.com/years/2023/opp.htm
  sportsref_download <- read.csv(glue("./01_data/contests/2023_w{week}/sportsref_download.csv"))
  
  sportsref_download$Bltz. <- round(as.numeric(sub("%","",sportsref_download$Bltz.))/100, digits = 3)
  sportsref_download$bltz_rank <- round(rank(-sportsref_download$Bltz.), digits = 0)
  
  team_blitz <- sportsref_download %>% 
    select(Tm,
           Bltz.,
           bltz_rank) %>% 
    left_join(read.csv("nfl_team_table.csv"), by = c('Tm' = 'Tm')) %>%
    arrange(bltz_rank) %>% 
    mutate(TeamAbbrev = gsub("JAC","JAX", TeamAbbrev))
}