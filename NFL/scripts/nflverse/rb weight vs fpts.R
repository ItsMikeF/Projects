#lets look at draft prospects

# Load packages
library(nflreadr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(nflfastR)
library(nflplotR)

# rosters

rosters <- load_rosters(2022)
rbs <- rosters %>% filter(position=="RB")
mean(rbs$weight)

# Load 2022 regular season data
pbp <- load_pbp(2022) %>% filter(season_type == "REG")

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

#slice top 25
#switch between slicing top 25 and filtering > 10 pts
rb_pbp_join_slice <- rb_pbp_join %>% 
  #filter(fpts > 10) %>% 
  slice_head(n=25) %>% 
  select(rusher, rusher_id, fpts, weight) %>% 
  drop_na()

# check correlation between weight and rush fpts
cor(rb_pbp_join_slice$weight, rb_pbp_join_slice$fpts)

# Find second order polynomial that describes relationship
fit <- lm(rb_pbp_join_slice$fpts ~ poly(rb_pbp_join_slice$weight, 2, raw = T))
fit

# Generate predicted values
weight_pred <- seq(min(rb_pbp_join_slice$weight), max(rb_pbp_join_slice$weight), length.out=101)
fpts_pred <- predict(fit, newdata = list(x=weight_pred))

# ggplot the data
df <- data.frame(weight_pred, fpts_pred)
ggplot(df, aes(x=weight_pred, fpts_pred)) +
  geom_point() +
  geom_line(aes(x=weight_pred, y=fpts_pred), color = "red") +
  theme_dark()

# Plot data with nfl headshots
# need to pipe in team colors
rb_pbp_join_slice %>% 
  ggplot(aes(x=reorder(rusher_id, -weight), y=fpts)) + 
  geom_col() +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(), 
    axis.text.x = element_nfl_headshot(size=1)
  )

min(rb_pbp_join_slice$weight, na.rm = T)
mean(rb_pbp_join_slice$weight, na.rm = T)
