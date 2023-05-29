#lets scrape the depth charts 

#load packages
suppressMessages({
  library(tidyverse) #ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, forcats
  library(glue) #interpreted string literals
  library(rvest) #easily harvest (scrape) web pages
  library(httr) #tools for working with URLs and HTTP
  library(cfbfastR) #access cfb pbp data
  library(ggimage) #use image in ggplot2
  library(tictoc) #functions for timing r scripts
})


# 1.0 Create list of all depth charts -------------------------------------

#create table of urls

cfb_depth_charts <- function(url) {
  tic()
  #ourlads_urls <- read.csv("./data/ourlads_urls.csv")
  
  #url <- ourlads_urls$url
  
  #start timer
  tic(msg = "Time to scrape all 112 depth charts")
  
  #combine all depth charts to a list
  depth_charts <- url %>% 
    map(function(url){
      
      #define all values
      css1 <- "td:nth-child(3) a" #starters
      css2 <- "td:nth-child(5) a" #second strings
      css3 <- "td:nth-child(7) a" #third strings
      css4 <- "td:nth-child(9) a" #fourth strings
      css5 <- ".row-dc-wht .row-dc-wht" #positions from white lines
      css6 <- ".row-dc-grey .row-dc-grey" #positions from grey lines
      
      #define the types of classman
      classmen <- c("FR", "RS FR", "FR/TR", "RS FR/TR", 
                    "SO", "RS SO", "SO/TR", "RS SO/TR",
                    "JR","RS JR", "JR/TR", "RS JR/TR",
                    "SR", "RS SR", "SR/TR", "RS SR/TR",
                    "GR/TR")
      
      webpage <- read_html(url)
      
      #scrape the positions
      pos1 <- html_text(html_elements(webpage, css5))
      pos2 <- html_text(html_elements(webpage, css6)) 
      
      pos_limit <- if_else(length(pos1)>length(pos2), length(pos2),length(pos1))
      
      pos <- c(rbind(pos1[1:pos_limit],pos2[1:pos_limit]))[1:11]
      
      #scrape the players
      player1 <- html_text(html_elements(webpage, css1)) 
      player1 <- player1[1:11]
      
      player2 <- html_text(html_elements(webpage, css2))
      player2 <- player2[1:11]
      
      #create a df of the depth chart
      lads_depth_chart <<- cbind(pos, player1, player2)
    })
  
  depth_charts <- setNames(depth_charts, ourlads_urls$team)
  toc()
}

ourlads_urls <- read.csv("./data/ourlads_urls.csv")
url <- ourlads_urls$url

cfb_depth_charts(url)

#start timer
tic(msg = "Time to scrape all 112 depth charts")

#combine all depth charts to a list
depth_charts <- url %>% 
  map(function(url){
    
    #define all values
    css1 <- "td:nth-child(3) a" #starters
    css2 <- "td:nth-child(5) a" #second strings
    css3 <- "td:nth-child(7) a" #third strings
    css4 <- "td:nth-child(9) a" #fourth strings
    css5 <- ".row-dc-wht .row-dc-wht" #positions from white lines
    css6 <- ".row-dc-grey .row-dc-grey" #positions from grey lines
    
    #define the types of classman
    classmen <- c("FR", "RS FR", "FR/TR", "RS FR/TR", 
                  "SO", "RS SO", "SO/TR", "RS SO/TR",
                  "JR","RS JR", "JR/TR", "RS JR/TR",
                  "SR", "RS SR", "SR/TR", "RS SR/TR",
                  "GR/TR")
    
    webpage <- read_html(url)
    
    #scrape the positions
    pos1 <- html_text(html_elements(webpage, css5))
    pos2 <- html_text(html_elements(webpage, css6)) 
    
    pos_limit <- if_else(length(pos1)>length(pos2), length(pos2),length(pos1))
    
    pos <- c(rbind(pos1[1:pos_limit],pos2[1:pos_limit]))[1:11]
    
    #scrape the players
    player1 <- html_text(html_elements(webpage, css1)) 
    player1 <- player1[1:11]
    
    player2 <- html_text(html_elements(webpage, css2))
    player2 <- player2[1:11]
    
    #create a df of the depth chart
    lads_depth_chart <- cbind(pos, player1, player2)
  })

depth_charts <- setNames(depth_charts, ourlads_urls$team)

# 2.0 testing area for data cleaning --------------------------------------

#work on cleaning up these depth charts
test <- as.data.frame(depth_charts$`central-florida`)

test %>% 
  str_replace("FR","")

#define the types of classman
classmen <- c("FR", "RS FR", "FR/TR", "RS FR/TR", 
              "SO", "RS SO", "SO/TR", "RS SO/TR",
              "JR","RS JR", "JR/TR", "[RS JR/TR]",
              "SR", "RS SR", "SR/TR", "[RS SR/TR]",
              "GR/TR")

for (j in 1:17) {
  for (i in 1:11) {
    test[i,2] <- test[i,2] %>% 
      str_remove(classmen[j]) %>% 
      trimws()
  }
}

###scrape one team for testing 
{
#scrape depth charts from team pages
url <- "https://www.ourlads.com/ncaa-football-depth-charts/depth-chart/lsu/90981"
webpage <- read_html(url)

#scrape the positions
css5 <- ".row-dc-wht .row-dc-wht"
css6 <- ".row-dc-grey .row-dc-grey"

pos1 <- html_text(html_elements(webpage, css5))
pos2 <- html_text(html_elements(webpage, css6)) 

pos_limit <- if_else(length(pos1)>length(pos2), length(pos2),length(pos1))

pos <- c(rbind(pos1[1:pos_limit],pos2[1:pos_limit]))[1:11]

#scrape the players
css1 <- "td:nth-child(3) a" #starters
css2 <- "td:nth-child(5) a" #second strings
css3 <- "td:nth-child(7) a" #third strings
css4 <- "td:nth-child(9) a" #fourth strings

player1 <- html_text(html_elements(webpage, css1)) 
player1 <- player1[1:11]

player2 <- html_text(html_elements(webpage, css2))
player2 <- player2[1:11]

player3 <- html_text(html_elements(webpage, css3)) 
player3 <- player3[1:11]

player4 <- html_text(html_elements(webpage, css4))
player4 <- player4[1:11]

#create a df of the depth chart
lads_depth_chart <- as.data.frame(cbind(pos, player1, player2, player3, player4))

#define the types of classman
classmen <- c("FR", "RS FR", "FR/TR", "RS FR/TR", 
              "SO", "RS SO", "SO/TR", "RS SO/TR",
              "JR","RS JR", "JR/TR", "[RS JR/TR]",
              "SR", "RS SR", "SR/TR", "[RS SR/TR]",
              "GR/TR")

for (j in 1:17) {
  for (i in 1:11) {
    lads_depth_chart[i,2] <- lads_depth_chart[i,2] %>% 
      str_remove(classmen[j]) %>% 
      trimws()
  }
}

}