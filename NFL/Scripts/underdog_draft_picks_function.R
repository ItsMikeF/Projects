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

#define constants
league_size <- 12
rounds <- 18
picks <- vector()

#define function to determine pick positions based on first pick
draft_positions <- function(i) {
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

draft_positions(5)
picks
