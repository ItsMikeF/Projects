#lets look at rb weight vs fpts

# Load packages
suppressMessages({
  library(nflreadr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(nflfastR)
  library(nflplotR)
  library(lubridate)
})


# 1.0 Last 5 years of RB data--------------------------------------------------

rb_list <- list()

for (year in 2018:2022) {
  #Load rosters
  rosters <- load_rosters(year)
  rbs <- rosters %>% filter(position=="RB")
  mean(rbs$weight)
  
  # Load regular season data
  pbp <- load_pbp(year) %>% 
    filter(season_type == "REG")
  
  # Get rushing stats
  rb_pbp <- pbp %>% 
    group_by(rusher, rusher_id) %>% 
    summarize(
      
      rush_attempt = sum(rush_attempt, na.rm = T),
      rushing_yards = sum(rushing_yards, na.rm = T),
      rush_touchdown = sum(rush_touchdown, na.rm = T),
      
      fumble = sum(fumble, na.rm = T), 
      
      epa_run = round(sum(epa)/sum(rush_attempt, na.rm = T), digits = 2)) %>% 
    drop_na() %>% 
    ungroup() %>% 
    arrange(-rushing_yards)
  
  # Get receiving stats
  wr_pbp <- pbp %>% 
    group_by(receiver, receiver_id) %>% 
    summarize(
      fumble = sum(fumble, na.rm = T), 
      
      receptions = sum(complete_pass, na.rm = T),
      receiving_yards = sum(receiving_yards, na.rm = T), 
      rec_touchdown = sum(pass_touchdown, na.rm = T),
      
      epa_pass = round(sum(epa)/sum(receiving_yards, na.rm = T), digits = 2)) %>% 
    drop_na() %>% 
    ungroup() %>% 
    arrange(-receiving_yards)
  
  rbs_fpts <- rb_pbp %>% 
    left_join(wr_pbp %>% select(receiver_id, receptions, receiving_yards, rec_touchdown, epa_pass), 
                                   by=c("rusher_id"="receiver_id")) %>% 
    replace(is.na(.),0) %>% 
    mutate(
      #big_rush = ifelse(rushing_yards > 100, 1,0), 
      #big_rec = ifelse(receiving_yards > 100, 1,0), 
      fpts = 
        #big_rush * 3 +
        #big_rec * 3
        rushing_yards * .1 +
        rush_touchdown * 6 +
        fumble * -1 +
        
        receptions * 0.5 +
        rec_touchdown * 6 +
        receiving_yards * .1, 
      fpts_ntile = ntile(fpts, 100)
    ) %>% 
    arrange(-fpts)
  
  # Join the weight data
  rb_pbp_join <- rbs_fpts %>% 
    left_join(rosters %>% select(season, gsis_id, position, height, weight),
              by=c("rusher_id"="gsis_id")) %>% 
    relocate(c("height", "weight"), .after = rusher) %>% 
    filter(position == "RB")

  #slice top 25
  #switch between slicing top 25 and filtering > 10 pts
  rb_pbp_join_slice <- rb_pbp_join %>% 
    #filter(fpts > 10) %>% 
    slice_head(n=25) %>% 
    select(rusher, rusher_id, season ,fpts, weight) %>% 
    drop_na()
  
  rb_list[[year-2017]] <- rb_pbp_join_slice
  
}

# Combine list into 1 dataframe
rbs_combined <- bind_rows(rb_list)

# Group rb data by season
rbs_combined %>% 
  group_by(season) %>% 
  summarise(avg_weight = as.integer(mean(weight)),
            median = median(weight),
            min = min(weight), 
            max = max(weight)
            )

# check correlation between weight and rush fpts
cor(rbs_combined$weight, rbs_combined$fpts)

# 2.0 Below here, i tried to do it without the list---------------------------


# Load rosters
rosters <- load_rosters(2018:2022)
rbs <- rosters %>% filter(position=="RB")

# Load 2022 regular season data
pbp <- load_pbp(2018:2022) %>% 
  filter(season_type == "REG") %>% 
  mutate(year = year(game_date))
  
# Get rushing stats
rb_pbp <- pbp %>% 
  group_by(rusher, rusher_id, year) %>% 
  summarize(
    
    rush_attempt = sum(rush_attempt, na.rm = T),
    rushing_yards = sum(rushing_yards, na.rm = T),
    rush_touchdown = sum(rush_touchdown, na.rm = T),
    
    fumble = sum(fumble, na.rm = T), 
    
    epa_run = round(sum(epa)/sum(rush_attempt, na.rm = T), digits = 2)) %>% 
  drop_na() %>% 
  ungroup() %>% 
  arrange(-rushing_yards)

# Get receiving stats
wr_pbp <- pbp %>% 
  group_by(receiver, receiver_id, year) %>% 
  summarize(
    fumble = sum(fumble, na.rm = T), 
    
    receptions = sum(complete_pass, na.rm = T),
    receiving_yards = sum(receiving_yards, na.rm = T), 
    rec_touchdown = sum(pass_touchdown, na.rm = T),
    
    epa_pass = round(sum(epa)/sum(receiving_yards, na.rm = T), digits = 2)) %>% 
  drop_na() %>% 
  ungroup() %>% 
  arrange(-receiving_yards)

rbs_fpts <- rb_pbp %>% left_join(wr_pbp %>% select(receiver_id, receptions, receiving_yards, rec_touchdown, epa_pass), 
                            by=c("rusher_id"="receiver_id")) %>% 
  replace(is.na(.),0) %>% 
  mutate(
    #big_rush = ifelse(rushing_yards > 100, 1,0), 
    #big_rec = ifelse(receiving_yards > 100, 1,0), 
    fpts = 
      #big_rush * 3 +
      #big_rec * 3
      rushing_yards * .1 +
      rush_touchdown * 6 +
      fumble * -1 +
      
      receptions * 0.5 +
      rec_touchdown * 6 +
      receiving_yards * .1, 
    fpts_ntile = ntile(fpts, 100)
  ) %>% 
  arrange(-fpts)

# Join the weight data
rb_pbp_join <- rbs_fpts %>% 
  left_join(rosters %>% select(gsis_id, position, height, weight),
            by=c("rusher_id"="gsis_id")) %>% 
  relocate(c("height", "weight"), .after = rusher) %>% 
  filter(position == "RB")


# 3.0 Top 25 RBs ----------------------------------------------------------

#slice top 25
#switch between slicing top 25 and filtering > 10 pts
rb_pbp_join_slice <- rb_pbp_join %>% 
  #filter(fpts > 10) %>% 
  slice_head(n=25) %>% 
  select(rusher, rusher_id, fpts, weight) %>% 
  drop_na()

# 3.1 Plot ----------------------------------------------------------------


# Plot data with nfl headshots
# need to pipe in team colors
rb_pbp_join_slice %>% 
  ggplot(aes(x=weight, y=fpts)) + 
  geom_point() +
  geom_nfl_headshots(aes(player_gsis = rusher_id), width = 0.075, vjust =0.45) +
  theme_minimal() 

# 3.3 Correlation ---------------------------------------------------------


# check correlation between weight and rush fpts
cor(rb_pbp_join_slice$weight, rb_pbp_join_slice$fpts)


# 3.4 Predict Values ------------------------------------------------------

# Find second order polynomial that describes relationship
fit <- lm(rb_pbp_join_slice$fpts ~ poly(rb_pbp_join_slice$weight, 2, raw = T))

# Generate predicted values
weight_pred <- seq(min(rb_pbp_join_slice$weight), max(rb_pbp_join_slice$weight), length.out=length(fpts_pred))
fpts_pred <- predict(fit, newdata = list(x=weight_pred))

# ggplot the data
df <- data.frame(weight_pred, fpts_pred)
ggplot(df, aes(x=weight_pred, fpts_pred)) +
  geom_point() +
  geom_line(aes(x=weight_pred, y=fpts_pred), color = "red") +
  theme_dark()
