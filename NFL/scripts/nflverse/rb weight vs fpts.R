#lets look at draft prospects

# Load packages
library(nflreadr)
library(dplyr)
library(ggplot2)

# rosters

rosters <- load_rosters(2022)
rbs <- rosters %>% filter(position=="RB")
mean(rbs$weight)

library(nflfastR)
pbp <- load_pbp(2022)

pbp %>% filter(receiver == rusher)

#rb historical stats and fpts
rb_pbp <- pbp %>% 
  group_by(rusher, rusher_id, posteam) %>% 
  summarize(
    
    rushing_yards = sum(rushing_yards, na.rm = T),
    rush_attempt = sum(rush_attempt, na.rm = T),
    rush_touchdown = sum(rush_touchdown, na.rm = T),
    fumble = sum(fumble, na.rm = T), 
    
    receptions = sum(complete_pass, na.rm = T),
    receiving_yards = sum(receiving_yards, na.rm = T), 
    rec_touchdown = sum(pass_touchdown, na.rm = T),
    
    epa = round(mean(epa), digits = 2)) %>% 
  mutate(big_rush = ifelse(rushing_yards > 100, 1,0), 
         big_rec = ifelse(receiving_yards > 100, 1,0), 
         fpts = 
           rushing_yards * .1 +
           rush_touchdown * 6 +
           fumble * -1 +
           
           receptions * 0.5 +
           rec_touchdown * 6 +
           receiving_yards * .1 +
           
           big_rush * 3 +
           big_rec * 3) %>% 
  drop_na() %>% 
  ungroup() %>% 
  arrange(-fpts)

# Join the weight data
rb_pbp_join <- rb_pbp %>% left_join(rbs, by=c("rusher_id"="gsis_id"))

#slice top 25
#switch between slicing top 25 and filtering > 10 pts
rb_pbp_join_slice <- rb_pbp_join %>% 
  filter(fpts > 10) %>% 
  #slice_head(n=25) %>% 
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
