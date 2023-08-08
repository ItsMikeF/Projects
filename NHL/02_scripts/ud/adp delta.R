# best ball ranking analysis

library(tidyverse)
library(glue)
library(gt)
library(gtExtras)

ud_rankings <- function(x) {
  
  # define some dates
  first_date <- "july27"
  current_month <- tolower(as.character(month(Sys.Date(), label = T, abbr = T)))

  # Load the opening rankings
  rankings_udd_1 <- read.csv(glue("./01_data/projections/season/2023_2024/rankings_{first_date}.csv")) %>% 
    mutate(name = paste(firstName, lastName),
           adp = as.numeric(adp)) %>% 
    select(name, adp, projectedPoints, slotName, teamName)
  
  # Load the most current rankings by reading the last file in the directory
  rankings_udd_2 <- read.csv(paste0("./01_data/projections/season/2023_2024/", 
                                    list.files(path = "./01_data/projections/season/2023_2024/")
                                    [max(which(grepl(current_month,list.files(path = "./01_data/projections/season/2023_2024/"))))]
  )
  ) %>% 
    mutate(name = paste(firstName, lastName), 
           adp = as.numeric(adp)) %>% 
    select(name, adp, projectedPoints, slotName, teamName)
  
  # extract the date from the file name
  second_date <<- str_extract(list.files(path = "./01_data/projections/season/2023_2024/")
                              [max(which(grepl(current_month,list.files(path = "./01_data/projections/season/2023_2024/"))))], 
                              "(?<=_)[a-z]+[0-9]+")
  
  # Combine rankings in a single dataframe
  rankings <<- rankings_udd_1 %>% 
    left_join(rankings_udd_2 %>% select(name, adp), by=c("name")) %>% 
    mutate(delta = adp.x-adp.y, 
           percent_change = round(delta/adp.x*100,digits = 1)) %>% 
    select(name, slotName, adp.y, adp.x, delta, percent_change, projectedPoints, teamName) %>% 
    rename_with(~ first_date, adp.x) %>% 
    rename_with(~ second_date, adp.y) %>% 
    arrange(.[[3]])
  
  # create a GT table with the rankings
  rankings_gt <- rankings %>% 
    drop_na() %>% 
    gt() %>% 
    data_color(columns = percent_change, colors = scales::col_numeric(
      palette = c("red", "green"), 
      domain = c(min(rankings$percent_change, na.rm = T), max(rankings$percent_change, na.rm = T))
    )) %>% 
    gt_theme_dark() 
  
  gtsave(rankings_gt, filename = glue("./03_plots/{second_date} UD NHL Board.html"))
  
}
ud_rankings(x)


# 2.0 draft picks -------------------------------------------------------------

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
    picks[j*2] <<- i+j*(2*length(seq(i,league_size,by=1))-1)+(j-1)*(2*i-1)
    picks[j*2+1] <<-i+j*(2*length(seq(i,league_size,by=1))-1)+j*(2*i-1)
    
    #print the values
    print(i+j*(2*length(seq(i,league_size,by=1))-1)+(j-1)*(2*i-1))
    print(i+j*(2*length(seq(i,league_size,by=1))-1)+j*(2*i-1))
    
  }
  #filters out picks beyond the draft size
  picks <<- picks[picks %in% 1:(league_size*rounds)]
}
draft_positions(3)

rankings[picks,] %>% select(-c(8:10))