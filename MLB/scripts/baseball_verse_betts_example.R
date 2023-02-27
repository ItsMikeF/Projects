#Load packages
library(baseballr) #acquiring and analyzing baseball data
library(tidyverse) #metapackage
library(reshape2) #flexibly reshape data
library(zoo) #S3 infrastructure for regular and irregular time series

#Find Mookie Betts"s MLBAMID
betts_id <- playerid_lookup("Betts") %>%
  filter(first_name == "Mookie") %>%
  select(mlbam_id, first_name, last_name)

#scrape Betts’ Statcast data, by pitch removing those with a batted ball speed of 0
betts_15 <- statcast_search("2015-03-31", "2015-10-31", playerid = betts_id[1,1], player_type = 'batter')
betts_16 <- statcast_search("2016-03-31", "2016-10-31", playerid = betts_id[1,1], player_type = 'batter') 
betts_17 <- statcast_search("2017-03-31", "2017-10-31", playerid = betts_id[1,1], player_type = 'batter')
betts_18 <- statcast_search("2018-03-31", "2018-10-31", playerid = betts_id[1,1], player_type = 'batter') 
betts_19 <- statcast_search("2019-03-31", "2019-10-31", playerid = betts_id[1,1], player_type = 'batter')
betts_20 <- statcast_search("2020-03-31", "2020-10-31", playerid = betts_id[1,1], player_type = 'batter') 
betts_21 <- statcast_search("2021-03-31", "2021-10-31", playerid = betts_id[1,1], player_type = 'batter')
betts_22 <- statcast_search("2022-03-31", "2022-10-31", playerid = betts_id[1,1], player_type = 'batter') 

#list of player years
betts_years <- list()

for (i in 15:22) {
  betts_years[[i-14]] <- paste0("betts_",i)
}

betts_years <- unlist(betts_years)
betts_years

get(betts_years[1])

#bind rows
betts <- bind_rows(betts_15,betts_16,betts_17,betts_18,betts_19,betts_20,betts_21,betts_22) %>%
  mutate(Year = as.factor(substr(game_date,1,4))) %>%
  filter(type == "X") %>%
  filter(launch_speed != 0)

#calculate average launch angles and batted ball speeds by game
betts_grpd <- betts %>%
  group_by(game_date) %>%
  summarise(
    `Average Launch Angle` = mean(launch_angle, na.rm = TRUE), 
    `Average Batted Ball Speed` = mean(launch_speed, na.rm = TRUE)) %>%
  ungroup() %>%
  melt(id=c("game_date")) %>%
  mutate(Year = as.factor(substr(game_date,1,4)))

#calculate Betts’ average launch angle and batted ball speed by year
betts_avg_speed_yr <- betts %>%
  group_by(Year) %>%
  summarise(
    speed = round(mean(launch_speed, na.rm = TRUE),1), 
    angle = round(mean(launch_angle, na.rm = TRUE),1))

#plot the data
betts_grpd %>%
  ggplot(aes(game_date, value)) +
  geom_point() +
  stat_smooth(aes(group = Year, color = Year)) +
  facet_wrap(~variable, scales = "free_y") + 
  ggtitle("\nMookie Betts: 2015 vs. 2016\n") + 
  labs(subtitle = paste0("Betts has lowered his launch angle in 2016, from ", betts_avg_speed_yr[1,3], " degrees in 2015 to ", betts_avg_speed_yr[2,3], " degrees this year.\n\n"), 
       caption = "@BillPetti\nData from baseballsavant.mlb.com\nData acquired with the baseballr package") +
  ylab("Angle = Degrees, Speed = MPH\n") +
  xlab("\nDate") +
  #theme_bp_grey() + 
  theme(legend.position = "bottom", strip.text.x = element_text(face = "bold", size = 14), plot.subtitle = element_text(hjust=-.12)) +
  scale_color_manual(values = c("#5F9ED1", "#FF800E"))

ggsave("betts_angle_speed_year.png", scale = 1.2, width = 14, height = 8.5, units = "in")
