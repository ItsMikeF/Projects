#web scrape depth charts from espn
#made via https://stackoverflow.com/questions/56739863/web-scraping-image-url-for-a-series-of-events-in-espn-play-by-play
#run this script 2nd

#load packages
suppressMessages({
  library(tidyverse) #ggplot2 dplyr tidyr readr stringr forcats purrr tibble
  library(nflverse) #nflfastr nflseedr nfl4th nflreadr nflplotr
  library(rvest) #easily harvest (scrape) web pages
  library(janitor) #simple little tools for examining and cleaning dirty data
  library(ggrepel) #automatically position non-overlapping text labels with ggplot2
  library(tictoc) #Functions for Timing R Scripts, as Well as Implementations of Stack and List Structures
})

# 1.0 scrape and clean depth charts -------------------------------------------------

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

depth_off <- setNames(depth_off, teams$team_abbr)

#create a data frame of the rosters from the list 
nfl_depth <- Reduce(full_join,depth_off)
names(nfl_depth) <- c("pos", "player", "team")

depth_off2 <- i %>% 
  map(function (i) {
    print(teams$team_abbr[i])
    url <- paste0("https://www.espn.com/nfl/team/depth/_/name/",teams$team_abbr[i],"/",teams$team_name[i])
    
    #scrape webpage with rvest
    webpage <- read_html(url)
    
    depth_chart <- css %>% 
      map(function (css){
        html <- html_nodes(webpage,css2)
        text <- trimws(html_text(html)) %>% as_tibble()
      })
    
    len <- length(depth_chart[[3]][[1]])
    
    positions <- depth_chart[[3]][[1]][seq(2, len, 2)]
    positions <- tibble(positions[1:12])
    
    off <- depth_chart[[1]][c(1:12),]
    cbind(positions,off, teams$team_abbr[i])
  } )
depth_off2 <- setNames(depth_off2, teams$team_abbr)

#create a data frame of the rosters from the list 
nfl_depth2 <- Reduce(full_join,depth_off2)
names(nfl_depth2) <- c("pos", "player", "team")

# 2.0 add injury report to starters ---------------------------------------

nfl_depth_inj <- nfl_depth %>% 
  left_join(injuries %>% select(name, status, comment), by=c("player"="name"))

# 3.0 separate into positions and load data -----------------------------------

#separate into position groups
qb1 <- nfl_depth[seq(1,dim(nfl_depth)[1],12),]
rb1 <- nfl_depth[seq(2,dim(nfl_depth)[1],12),]
wr1 <- nfl_depth[seq(3,dim(nfl_depth)[1],12),]

iol <- nfl_depth %>% 
  filter(pos == "LG" | pos == "C" | pos == "RG")

ot <- nfl_depth %>% 
  filter(pos == "LT" | pos == "RT")

#load pff offensive line data
pff_ol <- read.csv("./Training_Data/position_groups/ols.csv") %>% 
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
url <- "https://www.pro-football-reference.com/years/2022/advanced.htm"

webpage <- read_html(url)
tables <- webpage %>% html_table(fill = T)
pfr_rush <- tables[[5]] %>% 
  clean_names()

#clean the pfr data frame
pfr_rush$tm <- gsub(" ", "-", pfr_rush$tm)
pfr_rush$tm[32] <- "Washington-Commanders"

# 4.0 qb eda ---------------------------------------------------------

#lets look at the OT groups

#left join pff ol data to iol
ot_grades <- ot %>% 
  left_join(pff_ol, by = c("player"))

#summarise the iol data
ot_team <- ot_grades %>% 
  group_by(team) %>% 
  summarise(
    grades_pass_block.x = round(weighted.mean(grades_pass_block.x, non_spike_pass_block.x, na.rm=T), digits = 1),
    
    pass_snaps = sum(snap_counts_pass_block.x, na.rm = T),
    pbe = round(weighted.mean(pbe.x, non_spike_pass_block.x, na.rm=T), digits = 1),
    pressures_allowed = sum(true_pass_set_pressures_allowed, na.rm=T),
    
    tps_snaps = sum(true_pass_set_non_spike_pass_block, na.rm=T), 
    tps_pbe = round(weighted.mean(true_pass_set_pbe, true_pass_set_non_spike_pass_block, na.rm=T), digits = 1),
    tps_pressures_allowed = sum(true_pass_set_pressures_allowed, na.rm=T)
  )

#join qb and ot dfs
qb_team <- qb1 %>% 
  left_join(ot_team, by=c("team"))

#load qb data
pff_qb <- read.csv("./Training_Data/position_groups/qbs.csv") %>% 
  filter(year == max(year) & week == max(week)) 

pff_qb <- read.csv("./Training_Data/2022/passing_summary (17).csv")

#sort for elu
pff_qb_select <- pff_qb %>% 
  select(player, grades_pass, btt_rate, twp_rate, avg_depth_of_target, 
         avg_time_to_throw, grades_run, pressure_to_sack_rate, player_game_count, attempts, yards, ypa) %>% 
  mutate(ttt_run_grade = round(avg_time_to_throw*grades_run/pressure_to_sack_rate, digits = 1))

#left join rb_team with pff_rb_select
plot_qb <- qb_team %>% 
  left_join(pff_qb_select, by=c("player")) 

#change player full name in pff df to pbp name format
plot_qb <- plot_qb %>% 
  mutate(name = paste(substr(player,1,1),str_extract(player, '[^ ]+$'),sep = "."), 
         cat = paste0(name, team))

#load pbp data for player ids
pbp <- nflreadr::load_pbp(2021) %>% 
  dplyr::filter(season_type == "REG") %>%
  dplyr::filter(!is.na(posteam) & (rush == 1 | pass == 1))

#load pbp data for player ids
pbp_qbs <- pbp %>%
  filter(pass == 1) %>%
  filter(down %in% 1:4) %>%
  group_by(id) %>%
  summarise(
    name = first(name),
    team = last(posteam),
    plays = n(),
    passing_yards = sum(passing_yards, na.rm = T),
    pass_attempt = sum(pass_attempt, na.rm = T)
  ) %>% 
  arrange(-passing_yards) %>% 
  mutate(cat = paste0(name, team))

#join plot_rb with pbp_rbs for ids and stats
plot_qb_pbp <- plot_qb %>% 
  left_join(pbp_qbs, by=c("cat")) %>% 
  mutate(btt_twp = round(btt_rate / twp_rate, digits = 1)) #%>% drop_na() 

#qb plot
plot_qb_pbp %>% 
  ggplot(aes(x = grades_pass_block.x, y = grades_pass)) +
  geom_vline(xintercept = mean(plot_qb_pbp$grades_pass_block.x, na.rm=T), color="red",linetype="dashed", alpha=0.5) +
  geom_hline(yintercept = mean(plot_qb_pbp$grades_pass, na.rm=T), color="red",linetype="dashed", alpha=0.5) +
  #geom_nfl_logos(aes(team_abbr=team.x), width=0.065, alpha=0.7) +
  geom_nfl_headshots(aes(player_gsis = id), width = 0.075, vjust = 0.45) +
  geom_label_repel(aes(label = player)) +
  scale_y_continuous(limits = c(NA, 100)) +
  labs(
    title = "2023 QB Review",
    caption = "2022 OT Grade based on starting LT and RT on 2022 ESPN Depth Chart.",
    x = "Avg OT 2022 PBLK Grades",
    y = "2022 QB Passing Grade"
  ) + 
  theme(axis.title.x = element_text(size = 18), 
        axis.title.y = element_text(size = 18)) +
  ggsave("2022 QB Grades vs OT Grades.png", width = 1920/72, height = 1080/72, dpi = 72)

# 5.0 rb eda ---------------------------------------------------------

#join rb and iol dfs
rb_team <- rb1 %>% 
  left_join(iol_team, by=c("team"))

#load rb data
pff_rb <- read.csv("./Training_Data/position_groups/rbs.csv") %>% 
  filter(year == max(year) & week == max(week))

#manually add in missing data
plot_rb[which(plot_rb=="Breece Hall",arr.ind = T)[1],19] <- 88.8
plot_rb[which(plot_rb=="J.K. Dobbins",arr.ind = T)[1],19] <- 77.8
plot_rb[which(plot_rb=="Travis Etienne Jr.",arr.ind = T)[1],19] <- 86.9
plot_rb[which(plot_rb=="Cam Akers",arr.ind = T)[1],19] <- 45.5
plot_rb[which(plot_rb=="Dameon Pierce",arr.ind = T)[1],19] <- 83.4

#sort for elu
#The PFF "Elusive Rating" distills the success and impact of a runner with the ball independently of the blocking in front of him by looking at how hard he was to bring down.
pff_rb_select <- pff_rb %>% 
  select(player, attempts, player_game_count, designed_yards, elusive_rating)

#left join rb_team with pff_rb_select
plot_rb <- rb_team %>% 
  left_join(pff_rb_select, by=c("player")) 

#change player full name in pff df to pbp name format
plot_rb <- plot_rb %>% 
  mutate(name = paste(substr(player,1,1),str_extract(player, '[^ ]+$'),sep = "."), 
         cat = paste0(name, team)) 

#filter pbp data for rbs
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
  arrange(-rushing_yards) %>% 
  mutate(cat = paste0(name, team))

#join plot_rb with pbp_rbs for ids and stats
plot_rb_pbp <- plot_rb %>% 
  left_join(pbp_rbs, by=c("cat")) #%>% drop_na()

#rb plot
plot_rb_pbp %>% 
  ggplot(aes(x = rblk, y = elusive_rating)) +
  geom_vline(xintercept = mean(plot_rb_pbp$rblk), color="red",linetype="dashed", alpha=0.5) +
  geom_hline(yintercept = mean(plot_rb_pbp$elusive_rating, na.rm = T), color="red",linetype="dashed", alpha=0.5) +
  #geom_nfl_logos(aes(team_abbr=team.x), width=0.065, alpha=0.7) +
  geom_nfl_headshots(aes(player_gsis = id), width = 0.075, vjust = 0.45) +
  geom_label_repel(aes(label = player)) +
  labs(
    title = "2023 RB Elu vs IOL Grades",
    caption = "2022 IOL Grade based on starting LG, C, RG on 2022 ESPN Depth Chart. \n 
    The PFF Elusive Rating distills the success and impact of a runner with the ball independently of the blocking in front of him by looking at how hard he was to bring down.",
    x = "Avg IOL 2022 RBLK Grades", 
    y = "Elusiveness Rating"
  ) +
  scale_y_continuous(limits = c(NA, 130)) +
  theme(plot.title = element_text(size = 24), 
        axis.title = element_text(size = 18), 
        axis.text = element_text(size = 16), 
        plot.caption = element_text(size = 12)) +
  ggsave("2023 RB Elu vs IOL Grades.png", width = 1920/72, height = 1080/72, dpi = 72)


# 6.0 wr eda --------------------------------------------------------------


# 7.0 te eda --------------------------------------------------------------


# 8.0 def eda -------------------------------------------------------------

