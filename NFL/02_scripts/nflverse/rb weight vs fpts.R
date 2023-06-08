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

# create a list of the top 
rb_list <- lapply(2018:2022, function(year){
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
  
})

# Combine list into 1 dataframe
rbs_combined <- bind_rows(rb_list) %>% arrange(-fpts)

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

# 2.0 Top 25 RBs ----------------------------------------------------------

# look at last years top 25 rbs
rb_pbp_join_slice <- rbs_combined %>% 
  filter(season == 2022) %>% 
  select(rusher, rusher_id, fpts, weight) %>% 
  drop_na()

# check correlation between weight and rush fpts
cor(rb_pbp_join_slice$weight, rb_pbp_join_slice$fpts)

# check model
model_rb_weight_fpts <- MASS::rlm(fpts ~ weight, data = rb_pbp_join_slice)
model_rb_weight_fpts
saveRDS(model, file = "./04_models/rb_weight_to_fpts.rds")
readRDS("./04_models/rb_weight_to_fpts.rds")

# run model
new_data <- data.frame(rusher = "Bijan",weight = 220, fpts = NA)
predict(model_rb_weight_fpts, new_data)


# 3.0 Plot the data -------------------------------------------------------


# Function to save the plot
my_ggsave <- function(my_plot) {
  
  # Plot data with nfl headshots
  # need to pipe in team colors
  gg <- rb_pbp_join_slice %>% 
    ggplot(aes(x=weight, y=fpts)) + 
    geom_point() +
    ylim(NA, max(rb_pbp_join_slice$fpts)+20) +
    geom_nfl_headshots(aes(player_gsis = rusher_id), width = 0.075, vjust =0.45) +
    geom_smooth(method = MASS::rlm, se = F, color = "red", linetype = "dashed") +
    annotate("text", x = 240, y = 200, size = 6, 
             label = paste("Correlation:", 
                           round(cor(rb_pbp_join_slice$weight, rb_pbp_join_slice$fpts), digits = 2))) +
    labs(title = "Top 25 2023 Rb: Weight vs Fpts",
         subtitle = "0.5 ppr scoring",
         caption = "Data from nflfastR | Twitter: @Its_MikeF") +
    theme_dark() +
    theme(plot.title = element_text(size = 20, face = "bold"), 
          plot.subtitle = element_text(size = 12), 
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 12), 
          plot.caption = element_text(size = 12)) 
  
  ggsave(filename = "./03_plots/RB fpts vs weight.png",
         plot = gg,
         height = 11,
         width = 20,
         units = "in")
}
my_ggsave(gg)
