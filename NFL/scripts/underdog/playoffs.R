#lets look at player projections
#run this script 3rd

#load packages
suppressMessages({
  library(tidyverse) #metapackage
  library(nflverse) #functions to efficiently access NFL pbp data
  library(fs) #Cross-Platform File System Operations Based on 'libuv'
  library(ggrepel) #Automatically Position Non-Overlapping Text Labels with 'ggplot2'
})

# 1.0 projections --------------------------------------------------------------

#load the projections
projections_udd <- read.csv("./projections_playoffs/underdog_playoff_projections_dec28.csv") %>% 
  mutate(name = paste(firstName, lastName)) %>% 
  select(name, adp, projectedPoints, positionRank, slotName, teamName)

draft_positions <- function(league_size, rounds, i) {
  
  #define constants
  league_size <- league_size
  rounds <- rounds
  picks <<- vector()
  
  picks[1] <<- i
  
  for (j in 1:ceiling(rounds/2)) {
    #assign the picks to a vector
    picks[j*2] <<- i+j*(2*length(seq(i,league_size,by=1))-1)+(j-1)*(2*i-1)
    picks[j*2+1] <<-i+j*(2*length(seq(i,league_size,by=1))-1)+j*(2*i-1)
  }
  #filters out picks beyond the draft size
  picks <<- picks[picks %in% 1:(league_size*rounds)]
}
draft_positions(6,10,3)
picks

draft_picks <- projections_udd[picks,]
draft_picks
