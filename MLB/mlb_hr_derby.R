#load packages
suppressMessages({
  library(baseballr) #acquiring and analyzing baseball data
  library(tidyverse) #metapackage
  library(reshape2) #flexibly reshape data
  library(zoo) #S3 infrastructure for regular and irregular time series
  library(stringi) #fast and portable chartacter string processing
  library(rjson) #converts JSON objects into R objects 
})

#list batters
batters <- c("Ronald Acuña Jr.", "Kyle Schwarber", "Juan Soto", "Pete Alonso", "Julio Rodríguez", "Corey Seager", "Albert Pujols", "José Ramírez")
batters_clean <- stri_trans_general(batters, id = "Latin-ASCII")

#get fg stats
fg_batter_leaders <- fg_batter_leaders(2022, 2022, league = "all", qual = "n", ind = 1, exc_p = TRUE)
names <- tibble(names(fg_batter_leaders))

fg_batter_leaders_filter <- fg_batter_leaders %>% 
  arrange(-WAR) %>% 
  select(Season, playerid, Name, Age, Team, PA, AVG, OBP, SLG, OPS, wOBA, WAR, wRC_plus, WPA_plus, LD_pct, GB_pct) %>% 
  filter(Name %in% batters_clean)

#get statcast stats
statcast_leaderboards <- statcast_leaderboards(leaderboard = "exit_velocity_barrels", year = "2022", min_pa = "100") %>% 
  unite(name, first_name,last_name, sep = " ") %>% 
  filter(name %in% batters) %>% 
  mutate(name = stri_trans_general(name, id = "Latin-ASCII"))

#combine fg and sc
derby_stats <- fg_batter_leaders_filter %>% 
  left_join(statcast_leaderboards, by=c("Name" = "name"))

#load derby stats
mlb_game_pks <- mlb_game_pks("2021-07-10", level_ids = c(1))

#trying to load derby stats with game pk
mlb_homerun_derby <- mlb_homerun_derby(game_pk = 511101)
get_game_pks_mlb("2017-07-10")
str(mlb_homerun_derby)

#data wrangling for 2017 test data
test <- mlb_homerun_derby_bracket(game_pk = 511101)
test <- mlb_homerun_derby(game_pk = 511101)

test$alpha <- if_else(test$batter == test$top_seed_player_full_name, 
                      (test$top_seed_top_derby_hit_data_launch_speed), 
                      (test$bottom_seed_top_derby_hit_data_launch_speed))

test$bravo <- if_else(test$batter == test$top_seed_player_full_name, 
                      (test$top_seed_top_derby_hit_data_total_distance), 
                      (test$bottom_seed_top_derby_hit_data_total_distance))

test$year <- as.numeric(substr(test$event_date,1,4))

test2 <- test %>% 
  select(batter, hit_data_launch_speed, hit_data_launch_angle, hit_data_total_distance, top_seed_top_derby_hit_data_launch_speed, bottom_seed_top_derby_hit_data_launch_speed, alpha, 
         top_seed_top_derby_hit_data_total_distance, bottom_seed_top_derby_hit_data_total_distance, bravo)

test_2017 <- test %>% 
  group_by(batter) %>% 
  summarise(
    homeruns = sum(home_run),
    hit_data_launch_speed = round(mean(hit_data_launch_speed, na.rm = T), digits = 1), 
    hit_data_launch_angle = round(mean(hit_data_launch_angle, na.rm = T), digits = 1), 
    hit_data_total_distance = round(mean(hit_data_total_distance, na.rm = T), digits = 1), 
    top_derby_hit_data_launch_speed = max(alpha),
    top_derby_hit_data_total_distance = max(bravo)
  )

#load derby stats with json
year <- c(2015:2021)
df_stats <- list(list())

for (i in 1:length(year)) {
  print(paste("Start",year[i]))
  derby <- fromJSON(file = paste0("https://baseballsavant.mlb.com/derby-data?year=",year[i]))
  
  for (j in 1:length(derby[[1]])) {
    df_stats[[i]][[1]] <- derby[[1]][[j]]$id
    df_stats[[i]][[2]] <- derby[[1]][[j]]$fullName
  }
  print(paste("End",year[i]))
}

df <- data.frame()
for (i in 1:length(derby_2021[[1]])) {
  df[i,1] <- length(derby_2021[[1]][[i]])
}

list_min_names <- names(derby_2021[[1]][[index(which(df == min(df)))]])

df_stats[[2]][[1]] <- derby[[1]][[j]]$id

derby_2021_df <- as_tibble(derby_2021[[3]])
str