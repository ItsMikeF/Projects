#determine draft positions for snake draft

#load packages
suppressMessages({
  library(tidyverse) #ggplot2 dplyr tidyr readr stringr forcats purrr tibble
  library(nflverse) #nflfastr nflseedr nfl4th nflreadr nflplotr
  library(rvest) #easily harvest (scrape) web pages
  library(janitor) #simple little tools for examining and cleaning dirty data
  library(ggrepel) #automatically position non-overlapping text labels with ggplot2
  library(tictoc) #Functions for Timing R Scripts, as Well as Implementations of Stack and List Structures
})

#define function to determine pick positions based on first pick
draft_positions <- function(league_size, rounds, i) {
  
  #define constants
  league_size <- league_size
  rounds <- rounds
  picks <- vector()
  
  print(i)
  picks[1] <<- i
  
  for (j in 1:ceiling(rounds/2)) {
    #assign the picks to a vector
    picks[j*2] <<- i+j*(2*length(seq(i,league_size,by=1))-1)+(j-1)*(2*i-1)
    picks[j*2+1] <<-i+j*(2*length(seq(i,league_size,by=1))-1)+j*(2*i-1)
    
    #print the values
    print(i+j*(2*length(seq(i,league_size,by=1))-1)+(j-1)*(2*i-1))
    print(i+j*(2*length(seq(i,league_size,by=1))-1)+j*(2*i-1))
    
  }
  #filters out picks beyond the draft size
  picks <<- picks[picks %in% 1:(league_size*rounds)]
}

draft_positions(league_size,rounds,3)
picks
