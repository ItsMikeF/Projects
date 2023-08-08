#lets look at player projections
#run this script 3rd

#load packages
suppressMessages({
  library(tidyverse) #metapackage
  library(hockeyR) #functions to efficiently access NFL pbp data
  library(ggrepel) #Automatically Position Non-Overlapping Text Labels with 'ggplot2'
  library(ggimage) #use image in ggplot2
  library(ggtext) #improve text rendering support for ggplot2
})

# 1.0 Load pbp data -------------------------------------------------------

#hockeyR
pbp <- load_pbp(2022:2023)
names(pbp)

xg_leaders <- pbp %>% 
  filter(event_type %in% c("SHOT", "MISSED_SHOT", "GOAL")) %>% 
  group_by(player = event_player_1_name, id= event_player_1_id) %>% 
  summarize(
    team = last(event_team_abbr), 
    goals = sum(event_type == "GOAL"), 
    xg = round(sum(xg, na.rm = T),1), 
    .groups = "drop"
  ) %>% 
  arrange(-xg) %>% 
  mutate(rank = row_number(), 
         player = str_replace(player, "[.]"," "))

# 2.0 projections --------------------------------------------------------------

#load the first projections
projections_udd <- read.csv("./01_data/season_projections/2023_2024/rankings_july27.csv") %>% 
  mutate(name = paste(firstName, lastName)) %>% 
  select(name, adp, projectedPoints, positionRank, slotName, teamName)

projections_udd <- projections_udd %>% 
  replace(., projections_udd =='Mitch Marner','Mitchell Marner')

# 2.1 Projections change --------------------------------------------------

#load the latest projections
projections_udd_1 <- read.csv("./01_data/season_projections/2023_2024/rankings_july28.csv") %>% 
  mutate(name = paste(firstName, lastName)) %>% 
  select(name, adp, projectedPoints, positionRank, slotName, teamName)

projections_udd_1 <- projections_udd_1 %>% 
  replace(., projections_udd =='Mitch Marner','Mitchell Marner')

#check adp movement
projections_udd_delta <- projections_udd_1 %>% 
  left_join(projections_udd %>% select(name, adp), by=c("name"))

projections_udd_delta <- projections_udd_delta %>% 
  select(name, adp.x, adp.y, slotName, teamName, projectedPoints) %>% 
  mutate(adp.x = as.numeric(adp.x), 
         adp.y = as.numeric(adp.y)) %>% 
  mutate(adp_delta = adp.y-adp.x, 
         rel_adp_mvmt = round(adp_delta/adp.y, digits = 2)) %>% 
  select(name, slotName, teamName, projectedPoints, adp.x, adp.y, adp_delta, rel_adp_mvmt)

adp_movement <- projections_udd_delta %>% 
  group_by(teamName) %>% 
  summarize(
    adp_movement = sum(adp_delta, na.rm = T)
  ) %>% 
  filter(teamName != "") %>%  #%>% left_join(team_logos_colors, by=c("teamName"="full_team_name"))
  arrange(-adp_movement)

ggplot(adp_movement, aes(x=teamName, y=adp_movement)) +
  #geom_point() +
  #geom_image(aes(image=team_logo_espn))
  
  scale_x_discrete(name=NULL, labels = adp_movement$team_logo_espn) +
  theme(
    axis.text.x = element_markdown(color = "black", size = 11)
  )

  #geom_col() +
  #scale_x_discrete(name=NULL, labels = teamName)
  
  geom_col(aes(color = teamName, fill = teamName)) +
  theme(
    axis.title.x = ggplot2::element_blank(),
    axis.text.x = geom_image(aes(image=team_logo_espn))
  )

ggplot2::ggplot(offense, aes(x = team, y = off_epa)) +
  ggplot2::geom_col(aes(color = team, fill = team), width = 0.5) +
  nflplotR::scale_color_nfl(type = "secondary") +
  nflplotR::scale_fill_nfl(alpha = 0.4) +
  ggplot2::labs(
    title = "2020 NFL Offensive EPA per Play",
    y = "Offense EPA/play"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    plot.title.position = "plot",
    # it's obvious what the x-axis is so we remove the title
    axis.title.x = ggplot2::element_blank(),
    # this line triggers the replacement of team abbreviations with logos
    axis.text.x = element_nfl_logo(size = 1)
  )

# 2.2 Combine projections with pbp ----------------------------------------

#combine projection with pbp xg
projections <- projections_udd %>% 
  left_join(xg_leaders, by=c("name"="player")) %>% 
  mutate(adp = as.numeric(adp)) %>% 
  select(name, teamName, slotName, positionRank, adp, xg, goals, projectedPoints) %>% 
  arrange(adp)

#create dataframes for drafting
centers <- projections %>% filter(slotName == "C") 
wingers <- projections %>% filter(slotName == "W") 
defenders <- projections %>% filter(slotName == "D")
goalies <- projections %>% filter(slotName == "G")

# 2.0 picks -------------------------------------------------------------------

#define function to determine pick positions based on first pick
draft_positions <- function(league_size, rounds, i) {
  league_size <- league_size
  rounds <- rounds
  picks <<- vector()
  
  picks[1] <<- i
  
  for (j in 1:ceiling(rounds/2)) {
    #assign the picks to a vector
    picks[j*2] <<- i+j*(2*length(seq(i,12,by=1))-1)+(j-1)*(2*i-1)
    picks[j*2+1] <<-i+j*(2*length(seq(i,12,by=1))-1)+j*(2*i-1)
  }
  #filters out picks beyond the draft size
  picks <<- picks[picks %in% 1:(league_size*rounds)]
  print(picks)
  }

draft_positions(12,16,12)
draft_picks <- projections[picks,]
draft_picks
sum(draft_picks$projectedPoints)

#create a table of the picks and compare all sums of the avg proj points
k <- 1:12

picks_table <- k %>% 
  map(function (k) {
    draft_positions(12,16,k)
  } )

sum(picks_table[[1]]$projectedPoints)

# 3.0 plots -------------------------------------------------------------------

#plot it
ggplot(centers, aes(x=adp, y=projectedPoints)) +
  geom_point() + 
  geom_text_repel(aes(label=name)) +
  geom_smooth(method = "loess", se=F) + 
  theme_dark() +
  labs(
    title = "2022/2023 Fantasy Draft Value", 
    caption = "Twitter: Its_MikeF \n Data from Pro Football Focus"
  )

#plot it
ggplot(wingers, aes(x=adp, y=name)) +
  geom_point() + 
  geom_text_repel(aes(label=playerName)) +
  geom_smooth(method = "loess", se=F) + 
  theme_dark() +
  labs(
    title = "2022/2023 Fantasy Draft Value", 
    caption = "Twitter: Its_MikeF \n Data from Pro Football Focus"
  )

#plot it
ggplot(defenders, aes(x=adp, y=projectedPoints)) +
  geom_point() + 
  geom_text_repel(aes(label=name)) +
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