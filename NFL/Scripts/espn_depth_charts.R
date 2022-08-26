#web scrape depth charts from espn
#made via https://stackoverflow.com/questions/56739863/web-scraping-image-url-for-a-series-of-events-in-espn-play-by-play

#load packages
suppressMessages({
  library(tidyverse) #ggplot2 dplyr tidyr readr stringr forcats purrr tibble
  library(nflverse) #nflfastr nflseedr nfl4th nflreadr nflplotr
  library(rvest) #easily harvest (scrape) web pages
})

#css tags to be used
css1 <- ".fw-medium .AnchorLink" #starters
css2 <- ".Table__TD:nth-child(2) .AnchorLink" #second string
css3 <- ".Table--fixed-left span" #positions

#write css tags to a vector
css <- c(css1, css2, css3)

#nfl team abbrev table
teams <- teams_colors_logos %>% 
  select(team_abbr, team_name, team_nick)

#replace spaces with dashes
teams$team_name <- gsub(" ", "-", teams$team_name)

#remove old team names
teams <- teams[-c(17,27,30,33),]

#fix washington names
teams$team_abbr[32] <- "WSH"
teams$team_name[32] <- "Washington-Commanders"

#scrape one team
{
url <- "https://www.espn.com/nfl/team/depth/_/name/buf/buffalo-bills" 

#scrape webpage with rvest
webpage <- read_html(url)

#make depth chart as a list
depth_chart <- css %>% 
  map(function (css){
    html <- html_nodes(webpage,css)
    text <- trimws(html_text(html)) %>% as_tibble()
  })

len <- length(depth_chart[[3]][[1]])

positions <- depth_chart[[3]][[1]][seq(2, len, 2)]
positions <- tibble(positions[1:12])

off <- depth_chart[[1]][c(1:12),]

depth_off <- cbind(positions,off)
}

#scrape every team into a list with for loop
{
depth_off <- list()

for (i in 1:32) {
  print(teams$team_abbr[i])
  url <- paste0("https://www.espn.com/nfl/team/depth/_/name/",teams$team_abbr[i],"/",teams$team_name[i])
  
  #scrape webpage with rvest
  webpage <- read_html(url)
  
  depth_chart <- css %>% 
    map(function (css){
      html <- html_nodes(webpage,css)
      text <- trimws(html_text(html)) %>% as_tibble()
    })
  
  len <- length(depth_chart[[3]][[1]])
  
  positions <- depth_chart[[3]][[1]][seq(2, len, 2)]
  positions <- tibble(positions[1:12])
  
  off <- depth_chart[[1]][c(1:12),]
  
  depth_off[[i]] <- cbind(positions,off)
}
}

#purrr map every offensive roster into a list
i <- 1:32

depth_off <- i %>% 
  map(function (i) {
    print(teams$team_abbr[i])
    url <- paste0("https://www.espn.com/nfl/team/depth/_/name/",teams$team_abbr[i],"/",teams$team_name[i])
    
    #scrape webpage with rvest
    webpage <- read_html(url)
    
    depth_chart <- css %>% 
      map(function (css){
        html <- html_nodes(webpage,css)
        text <- trimws(html_text(html)) %>% as_tibble()
      })
    
    len <- length(depth_chart[[3]][[1]])
    
    positions <- depth_chart[[3]][[1]][seq(2, len, 2)]
    positions <- tibble(positions[1:12])
    
    off <- depth_chart[[1]][c(1:12),]
    
    depth_off[[i]] <- cbind(positions,off)
 } )

#sort to positon groups
nfl_depth <- Reduce(full_join,depth_off)
qb1 <- nfl_depth[seq(1,dim(nfl_depth)[1],12),]
rb1 <- nfl_depth[seq(2,dim(nfl_depth)[1],12),]
wr1 <- nfl_depth[seq(3,dim(nfl_depth)[1],12),]

t <- as.tibble(t(nfl_depth))
