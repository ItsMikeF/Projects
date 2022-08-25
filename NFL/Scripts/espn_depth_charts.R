#web scrape depth charts from espn
#made via https://stackoverflow.com/questions/56739863/web-scraping-image-url-for-a-series-of-events-in-espn-play-by-play

#load packages
suppressMessages({
  library(tidyverse)
  library(rvest)
})

#css table to be used
css1 <- ".fw-medium .AnchorLink" #starters
css2 <- ".Table__TD:nth-child(2) .AnchorLink" #second string
css3 <- ".Table--fixed-left span" #positions

#write css tags to a vector
css <- c(css1, css2, css3)

#url for the depth chart
url <- "https://www.espn.com/nfl/team/depth/_/name/buf/buffalo-bills" #bills

#scrape webpage with rvest
webpage <- read_html(url)

#make depth chart as a list
depth_chart <- css %>% 
  map(function (css){
    html <- html_nodes(webpage,css)
    text <- html_text(html) %>% as.tibble()
  })

depth_chart[[3]][[1]]
len <- length(depth_chart[[3]][[1]])

n <- trimws(depth_chart[[3]][[1]][seq(2, len, 2)])
m <- tibble(n[1:12])

p <- depth_chart[[1]][c(1:12),]

q <- cbind(m,p)
