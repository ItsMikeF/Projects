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
projections_pff <- read.csv("./projections_season/2023/projections_pff.csv") %>% 
  select(playerName, position, fantasyPoints, fantasyPointsRank)

projections_udd <- read.csv("./projections_season/2023/rankings_may05.csv") %>% 
  mutate(name = paste(firstName, lastName)) %>% 
  select(name, adp, projectedPoints, positionRank, slotName, teamName)

#aggregate projections
projections <- projections_udd

projections <- projections_pff %>% 
  left_join(projections_udd, by=c("playerName"="name")) %>% 
  rename(pos=position, 
         pff=fantasyPoints, 
         underdog=projectedPoints) %>% 
    mutate(avg = round(((pff + underdog)/2),digits=1),
           adp = as.numeric(adp)) %>% 
  select(playerName, pos, adp, avg, pff, underdog, teamName) %>% 
  arrange(adp)

#filter by the 6 positions
pos <- unique(projections$pos)

#create dataframes for drafting
#create qb dataframe
qbs <- projections %>% filter(pos == "qb") %>% 
  left_join(plot_qb_pbp, by=c("playerName"="player")) %>% 
  select(playerName, teamName, adp, avg, pff, underdog, grades_pass, btt_twp, tps_pbe, ttt_run_grade, avg_depth_of_target, avg_time_to_throw, ttt_run_grade)

rbs <- projections %>% filter(pos == "rb") 
wrs <- projections %>% filter(pos == "wr")
tes <- projections %>% filter(pos == "te")

#sleeper only
#ks <- projections %>% filter(pos == "k") 
#dst <- projections %>% filter(pos == "dst")

# 2.0 picks -------------------------------------------------------------------

#define function to determine pick positions based on first pick
draft_positions <- function(i) {
  
  #define constants
  league_size <- 12
  rounds <- 18
  picks <<- vector()
  
  picks[1] <<- i
  
  for (j in 1:ceiling(rounds/2)) {
    #assign the picks to a vector
    picks[j*2] <<- i+j*(2*length(seq(i,12,by=1))-1)+(j-1)*(2*i-1)
    picks[j*2+1] <<-i+j*(2*length(seq(i,12,by=1))-1)+j*(2*i-1)
  }
  #filters out picks beyond the draft size
  picks <<- picks[picks %in% 1:(league_size*rounds)]
}
draft_positions(11)
picks
draft_picks <- projections[picks,]
draft_picks
sum(draft_picks$avg)

#create a table of the picks and compare all sums of the avg proj points
k <- 1:12

picks_table <- k %>% 
  map(function (k) {
    draft_positions(k)
  } )

sum(picks_table[[1]]$avg)

# 3.0 plots -------------------------------------------------------------------

#plot it
ggplot(qbs, aes(x=adp, y=avg)) +
  geom_point() + 
  geom_text_repel(aes(label=playerName)) +
  geom_smooth(method = "loess", se=F) + 
  theme_dark() +
  labs(
    title = "2022/2023 Fantasy Draft Value", 
    caption = "Twitter: Its_MikeF \n Data from Pro Football Focus"
  )

#plot it
ggplot(rbs, aes(x=adp, y=avg)) +
  geom_point() + 
  geom_text_repel(aes(label=playerName)) +
  geom_smooth(method = "loess", se=F) + 
  theme_dark() +
  labs(
    title = "2022/2023 Fantasy Draft Value", 
    caption = "Twitter: Its_MikeF \n Data from Pro Football Focus"
  )

#plot it
ggplot(wrs, aes(x=adp, y=avg)) +
  geom_point() + 
  geom_text_repel(aes(label=playerName)) +
  geom_smooth(method = "loess", se=F) + 
  theme_dark() +
  labs(
    title = "2022/2023 Fantasy Draft Value", 
    caption = "Twitter: Its_MikeF \n Data from Pro Football Focus"
  )

#facet warp plot
ggplot(projections, aes(x=adp, y=avg)) +
  geom_point() + 
  geom_text_repel(aes(label=playerName)) +
  facet_wrap(vars(pos)) +
  geom_smooth(method = "loess", se=F) + 
  theme_dark() +
  labs(
    title = "2022/2023 Fantasy Draft Value", 
    caption = "Twitter: Its_MikeF \n Data from Pro Football Focus"
  )
