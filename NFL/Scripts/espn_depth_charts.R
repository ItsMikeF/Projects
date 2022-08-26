#web scrape depth charts from espn
#made via https://stackoverflow.com/questions/56739863/web-scraping-image-url-for-a-series-of-events-in-espn-play-by-play

#load packages
suppressMessages({
  library(tidyverse) #ggplot2 dplyr tidyr readr stringr forcats purrr tibble
  library(nflverse) #nflfastr nflseedr nfl4th nflreadr nflplotr
  library(rvest) #easily harvest (scrape) web pages
  library(janitor) #simple little tools for examining and cleaning dirty data
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

#fix Washington names
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
    cbind(positions,off, teams$team_abbr[i])
 } )

rm(depth_off)

#create a data frame of the rosters from the list 
nfl_depth <- Reduce(full_join,depth_off)
names(nfl_depth) <- c("pos", "player", "team")

#separate into position groups
qb1 <- nfl_depth[seq(1,dim(nfl_depth)[1],12),]
rb1 <- nfl_depth[seq(2,dim(nfl_depth)[1],12),]
wr1 <- nfl_depth[seq(3,dim(nfl_depth)[1],12),]

iol <- nfl_depth %>% 
  filter(pos == "LG" | pos == "C" | pos == "RG")

ot <- nfl_depth %>% 
  filter(pos == "LT" | pos == "RT")

#load pff offensive line data
pff_ol <- read_csv("./Training_Data/position_groups/ols.csv") %>% 
  filter(year == max(year) & week == max(week))

#left join pff ol data to iol
iol_grades <- iol %>% 
  left_join(pff_ol, by = c("player"))

#summarise the iol data
iol_team <- iol_grades %>% 
  group_by(team) %>% 
  summarise(
    rblk = round(weighted.mean(grades_run_block.x, non_spike_pass_block.x, na.rm=T), digits = 1),
    rblk_snaps = sum(snap_counts_run_block.x, na.rm=T), 
    
    gap_snaps = sum(gap_snap_counts_run_block, na.rm=T),
    gap_rblk = round(weighted.mean(gap_grades_run_block, gap_snap_counts_run_block, na.rm=T), digits = 1),
    
    zone_snaps = sum(zone_snap_counts_run_block, na.rm=T),
    zone_rblk = round(weighted.mean(zone_grades_run_block, zone_snap_counts_run_block, na.rm=T), digits = 1),
    
    pass_snaps = sum(snap_counts_pass_block.x, na.rm = T),
    pbe = round(weighted.mean(pbe.x, non_spike_pass_block.x, na.rm=T), digits = 1),
    hurries = sum(hurries_allowed.x, na.rm=T),
    
    tps_snaps = sum(true_pass_set_non_spike_pass_block, na.rm=T), 
    tps_pbe = round(weighted.mean(true_pass_set_pbe, true_pass_set_non_spike_pass_block, na.rm=T), digits = 1),
    tps_hurries = sum(true_pass_set_hurries_allowed, na.rm=T)
  )

#lets scrape pro football reference for team advanced rushing stats
url <- "https://www.pro-football-reference.com/years/2021/advanced.htm"

webpage <- read_html(url)
tables <- webpage %>% html_table(fill = T)
pfr_rush <- tables[[5]] %>% 
  clean_names()

#clean the pfr data frame
pfr_rush$tm <- gsub(" ", "-", pfr_rush$tm)
pfr_rush$tm[32] <- "Washington-Commanders"

#join rb and iol dfs
rb_team <- rb1 %>% 
  left_join(iol_team, by=c("team"))

#load rb data
pff_rb <- read_csv("./Training_Data/position_groups/rbs.csv") %>% 
  filter(year == max(year) & week == max(week))

#sort for elu
#The PFF "Elusive Rating" distills the success and impact of a runner with the ball independently of the blocking in front of him by looking at how hard he was to bring down.
pff_rb_select <- pff_rb %>% 
  select(player, attempts, player_game_count, designed_yards, elusive_rating)

#left join rb_team with pff_rb_select
plot_rb <- rb_team %>% 
  left_join(pff_rb_select, by=c("player")) %>% 
  drop_na()

#change player full name in pff df to pbp name format
plot_rb <- plot_rb %>% 
  mutate(name = paste(substr(player,1,1),str_extract(player, '[^ ]+$'),sep = ".")) 

#load pbp data for player ids
pbp <- nflreadr::load_pbp(2021) %>% 
  dplyr::filter(season_type == "REG") %>%
  dplyr::filter(!is.na(posteam) & (rush == 1 | pass == 1))

pbp_rbs <- pbp %>%
  filter(rush == 1) %>%
  filter(down %in% 1:4) %>%
  group_by(id) %>%
  summarise(
    name = first(name),
    team = last(posteam),
    plays = n(),
    rushing_yards = sum(rushing_yards, na.rm = T),
    rush_att = sum(rush_attempt, na.rm = T)
  ) %>% 
  arrange(-rushing_yards)

#join plot_rb with pbp_rbs for ids and stats
plot_rb <- plot_rb %>% 
  left_join(pbp_rbs, by=c("name")) 

#lets plot it
ggplot2::ggplot(plot_rb, aes(x = reorder(player, -rblk), y = elusive_rating)) +
  ggplot2::geom_col(aes(color = team.y, fill = team.y), width = 0.5) +
  nflplotR::geom_nfl_headshots(aes(player_gsis = id), width = 0.075, vjust = 0.45) +
  nflplotR::scale_color_nfl(type = "secondary") +
  nflplotR::scale_fill_nfl(alpha = 0.4) +
  ggplot2::labs(
    title = "2022 RB",
    y = "Elusiveness Rating", 
    caption = "Data from PFF"
  ) +
  ggplot2::ylim(0, 0.4) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    plot.title.position = "plot",
    # it's obvious what the x-axis is so we remove the title
    axis.title.x = ggplot2::element_blank(),
    # this line triggers the replacement of team abbreviations with logos
    axis.text.x = element_nfl_logo(size = 1)
  )
