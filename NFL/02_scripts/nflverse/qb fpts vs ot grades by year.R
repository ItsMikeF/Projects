#nfl qb passing grade vs ot historical

# load packages
suppressMessages({
  library(tidyr)
  library(dplyr)
  library(nflfastR)
  library(nflplotR)
  library(lubridate)
  library(ggplot2)
  library(ggrepel)
})


# 1.0 Load OL data --------------------------------------------------------


# load position groups
ol <- read.csv("./01_data/Training_Data/position_groups/ols.csv")

names(ol)

# filter for tps pass block by team and year
ol_filtered <- ol %>% 
  filter(week == 17) %>%
  filter(position == "T") %>% 
  group_by(team_name, year) %>% 
  summarise(true_pass_set_grades_pass_block = round(weighted.mean(true_pass_set_grades_pass_block, true_pass_set_snap_counts_pass_block, na.rm = T),digits = 1)) %>% 
  ungroup() %>% 
  mutate(join = paste0(year, team_name))


# 2.0 Load and process qb data --------------------------------------------

# get qb fpts from pbp
pbp <- load_pbp(2022)
names(pbp)
pbp$fumble_lost
rm(qb_list)

# create list of qb data
qb_list <- lapply(2014:2022, function(year){
  
  print(paste("Loading NFL Season:", year))
  
  # Load regular season data
  pbp <- load_pbp(year) %>% 
    filter(season_type == "REG") %>% 
    mutate(year = ymd(game_date)) %>% 
    group_by(passer, passer_id, posteam) %>% 
    summarize(
      
      passing_yards = sum(passing_yards, na.rm = T), 
      pass_attempt = sum(pass_attempt, na.rm = T), 
      pass_touchdown = sum(pass_touchdown, na.rm = T), 
      interception = sum(interception, na.rm = T), 
      
      rush_attempt = sum(rush_attempt, na.rm = T),
      rushing_yards = sum(rushing_yards, na.rm = T),
      rush_touchdown = sum(rush_touchdown, na.rm = T),
      
      fumble_lost = sum(fumble_lost, na.rm = T), 
      year = last(year(game_date))
      ) %>% 
    drop_na() %>% 
    ungroup() %>% 
    mutate(
      fpts = 
        pass_touchdown * 4 +
        passing_yards * .04 +
        interception * -1 +
        rushing_yards * .1 +
        rush_touchdown * 6 +
        fumble_lost * -1, 
      fpts_ntile = ntile(fpts, 100), 
      join = paste0(year, posteam)
    ) %>% 
    arrange(-fpts)
})

# bind qb data to a dataframe
qbs <- bind_rows(qb_list)
median(qbs$fpts)
mean(qbs$fpts)

# 3.0 Analyze Data --------------------------------------------------------


# load team logos
teams_colors_logos <- teams_colors_logos %>% 
  filter(!team_abbr %in% c("LA","OAK","SD","STL")) %>% 
  select(team_name, team_abbr, team_logo_espn)

# join qbs and ot data
data <- qbs %>%
  left_join(ol_filtered %>% select(join, true_pass_set_grades_pass_block), by=c("join")) %>% 
  left_join(teams_colors_logos, by=c("posteam"="team_abbr")) %>% 
  drop_na() %>% # have some clean up work to do 
  filter(fpts > 200)



cor(data$fpts, data$true_pass_set_grades_pass_block)


# 4.0 Plot data -----------------------------------------------------------

# plot the data
data %>% 
  ggplot(aes(x=true_pass_set_grades_pass_block, y=fpts)) +
  geom_nfl_logos(aes(team_abbr = posteam), width = 0.02, alpha = 0.7) +
  geom_text_repel(aes(label = join)) +
  geom_smooth() +
  ggtitle(label = "OT Grades vs QB Fpts", 
          subtitle = "2014 - 2022") +
  labs(caption = "Data from PFF & nflfastR | Twitter: @Its_MikeF") +
  ggsave(filename = "./03_plots/OT Grades vs QB Fpts.png", 
         width = 20, 
         height = 11, 
         units = "in")
