#underdog winning draft anaylsis

#load packages
suppressMessages({
  library(tidyverse) #metapackage
  library(nflverse) #functions to efficiently access NFL pbp data
  library(fs) #Cross-Platform File System Operations Based on 'libuv'
  library(ggrepel) #Automatically Position Non-Overlapping Text Labels with 'ggplot2'
})

# 1.0 projections --------------------------------------------------------------

#load the projections
projections_pff <- read.csv("./season_projections/projections_pff.csv") %>% 
  select(playerName, position, fantasyPoints, fantasyPointsRank)

projections_udd <- read.csv("./season_projections/projections_underdog.csv") %>% 
  mutate(name = paste(firstName, lastName)) %>% 
  select(name, adp, projectedPoints, positionRank, slotName, teamName)


# 2.0 Draft position function ---------------------------------------------

#define function to determine pick positions based on first pick
draft_positions <- function(i) {
  
  #define constants
  league_size <- 12
  rounds <- 18
  picks <<- vector()
  
  print(i)
  picks[1] <<- i
  
  for (j in 1:ceiling(rounds/2)) {
    #assign the picks to a vector
    picks[j*2] <<- i+j*(2*length(seq(i,12,by=1))-1)+(j-1)*(2*i-1)
    picks[j*2+1] <<-i+j*(2*length(seq(i,12,by=1))-1)+j*(2*i-1)
    
    #print the values
    print(i+j*(2*length(seq(i,12,by=1))-1)+(j-1)*(2*i-1))
    print(i+j*(2*length(seq(i,12,by=1))-1)+j*(2*i-1))
    
  }
  #filters out picks beyond the draft size
  picks <<- picks[picks %in% 1:(league_size*rounds)]
}
draft_positions(11)

# 3.0 Winning Draft -------------------------------------------------------

winning_draft <- c('Jalen Hurts', 'Joe Burrow', 'Davis Mills', 
                   'Miles Sanders', 'Kenneth Walker', 'Rachaad White', 'Dameon Pierce', 'Khalil Herbert', 'Chris Evans',
                   'Davante Adams', 'A.J. Brown', 'Jaylen Waddle', 'Amon-Ra St. Brown', 'Garrett Wilson', 'Nico Collins', 'Zay Jones',
                   'Travis Kelce', 'Evan Engram')

wd <- projections_udd %>% filter(name %in% winning_draft)
wd <- cbind(wd, picks)

wd$adp <- as.numeric(wd$adp)

wd <- wd %>% 
  mutate(value = picks-adp) %>% 
  select(name, picks, adp, value) %>% 
  mutate(percent = round(value/adp, digits = 2))

sum(wd$percent)