#web scrape depth charts from espn
#made via https://stackoverflow.com/questions/56739863/web-scraping-image-url-for-a-series-of-events-in-espn-play-by-play
#run this script 2nd

#load packages
suppressMessages({
  library(dplyr) #ggplot2 dplyr tidyr readr stringr forcats purrr tibble
  library(purrr)
  library(stringr)
  library(nflverse) #nflfastr nflseedr nfl4th nflreadr nflplotr
  library(rvest) #easily harvest (scrape) web pages
  library(janitor) #simple little tools for examining and cleaning dirty data
  library(ggrepel) #automatically position non-overlapping text labels with ggplot2
})

# 1.0 scrape and clean depth charts -------------------------------------------------

#css tags to be used
css1 <- ".nfl-depth-table div:nth-child(2) .flex .AnchorLink" #first and second string defenders

#write css tags to a vector
css <- c(css1)

#nfl team abbrev table
teams <- teams_colors_logos %>% 
  select(team_abbr, team_name) %>% 
  filter(!team_abbr %in% c("LA", "OAK", "SD", "STL"))

#replace spaces with dashes
teams$team_name <- gsub(" ", "-", teams$team_name)

#fix Washington names
teams$team_abbr[32] <- "WSH"
teams$team_name[32] <- "Washington-Commanders"

#purrr map every offensive roster into a list
i <- 1:32

depth_def <- i %>% 
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
    
    # define starters
    def1 <- tibble(unlist(depth_chart)[seq(1,22,2)])
    
    # define second strings
    def2 <- tibble(unlist(depth_chart)[seq(2,22,2)])
    
    #define espn positions
    positions_espn <- tibble(c("LDE", "LDT", "RDT", "RDE", "WLB", "MLB", "SLB", "LCB", "SS", "FS", "RCB"))

    # column bind all
    cbind(positions_espn, def1, def2, teams$team_abbr[i])
    })

depth_def <- setNames(depth_def, teams$team_abbr)

# Create a data frame of the rosters from the list
depth_def <- bind_rows(depth_def)
names(depth_def) <- c("pos", "player1", "player2", "team")

