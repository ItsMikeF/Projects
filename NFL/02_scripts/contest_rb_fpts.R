
pbp <- load_pbp(2022:2023)

# Get rushing stats
rb_pbp <- pbp %>% 
  group_by(rusher, rusher_id, posteam, week, season) %>% 
  summarise(
    
    rush_attempt = sum(rush_attempt, na.rm = T),
    rushing_yards = sum(rushing_yards, na.rm = T),
    rush_touchdown = sum(rush_touchdown, na.rm = T),
    
    fumble = sum(fumble, na.rm = T)) %>% 
  drop_na() %>% 
  ungroup() %>% 
  mutate(join = paste(season, week, rusher_id, sep = "_"))

# Get receiving stats
wr_pbp <- pbp %>% 
  group_by(receiver, receiver_id, posteam, week, season) %>% 
  summarize(
    fumble = sum(fumble, na.rm = T), 
    
    receptions = sum(complete_pass, na.rm = T),
    receiving_yards = sum(receiving_yards, na.rm = T), 
    rec_touchdown = sum(pass_touchdown, na.rm = T)) %>% 
  drop_na() %>% 
  ungroup() %>% 
  mutate(join = paste(season, week, receiver_id, sep = "_"))

rbs_fpts <- rb_pbp %>% 
  left_join(wr_pbp %>% select(receptions, receiving_yards, rec_touchdown, join), 
            by=c("join")) %>% 
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
