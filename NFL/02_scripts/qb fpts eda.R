# qb eda

#load packages
suppressMessages({
  options(nflreadr.verbose = FALSE)
  library(nflfastR) # pbp data
  library(nflreadr) # nfl schedule and cleaning
  library(nflplotR) # nfl headshot
  library(tidyverse) # ggplot2 dplyr tibble tidyr purrr forecats 
  library(glue) # interpreted literal strings
  library(caret) # data partition
  library(randomForest) # rf model
  library(xgboost) # xgb model
  library(ggrepel) #
})

# define years
start_year <- 2020
nfl_year <- year(Sys.Date())

# load pbp
pbp <- load_pbp(start_year:nfl_year)

# load schedule
schedule <- load_schedules(start_year:nfl_year)

# calc rb fpts by game week
qb_fpts_pbp <- function(){
  
  # Load regular season data
  qb_pbp <- pbp %>% 
    group_by(game_id, passer, passer_id, posteam) %>% 
    summarize(
      
      epa = round(sum(qb_epa, na.rm = T), digits = 2),
      snaps = n(),
      epa_per_play = round(epa/snaps, digits = 2),
      
      passing_yards = sum(passing_yards, na.rm = T), 
      pass_attempt = sum(pass_attempt, na.rm = T), 
      pass_touchdown = sum(pass_touchdown, na.rm = T), 
      interception = sum(interception, na.rm = T), 
      
      # use to get last non-missing values
      season_type = last(season_type), 
      temp = last(temp), 
      spread_line = last(spread_line), 
      total_line = last(total_line), 
      posteam = last(posteam), 
      week = last(week), 
      season = last(season)
      
    ) %>% 
    ungroup() %>% 
    mutate(join = paste(game_id, passer_id, sep = "_"))
  
  # Get passer rushing stats
  passer_pbp <- pbp %>% 
    group_by(game_id, passer, passer_id, posteam) %>% 
    summarise(
      
      qb_scramble = sum(qb_scramble, na.rm = T),
      rush_attempt = sum(rush_attempt, na.rm = T),
      rushing_yards = sum(rushing_yards, na.rm = T),
      rush_touchdown = sum(rush_touchdown, na.rm = T),
      
      fumble = sum(fumble, na.rm = T), 
      
      ) %>% 
    drop_na() %>% 
    ungroup() %>% 
    mutate(join = paste(game_id, passer_id, sep = "_"))
  
  # Get rusher rushing stats
  rusher_pbp <- pbp %>% 
    group_by(game_id, season_type, temp, spread_line, total_line, 
             rusher, rusher_id, posteam, week, season) %>% 
    summarise(
      
      rush_attempt = sum(rush_attempt, na.rm = T),
      rushing_yards = sum(rushing_yards, na.rm = T),
      rush_touchdown = sum(rush_touchdown, na.rm = T),
      
      fumble = sum(fumble, na.rm = T)) %>% 
    drop_na() %>% 
    ungroup() %>% 
    mutate(join = paste(game_id, rusher_id, sep = "_"))
  
  qb_rush <- passer_pbp %>%
    left_join(rusher_pbp, by = "join") %>%
    
    # add zeros so that the columns can be added
    mutate(across(c(rush_attempt.x, rush_attempt.y, 
                    rushing_yards.x, rushing_yards.y, 
                    rush_touchdown.x, rush_touchdown.y, 
                    fumble.x, fumble.y), 
                  ~ replace_na(., 0))) %>% 
    
    # add the columns from scrambles + designed rushes
    mutate(rush_attempt = rush_attempt.x + rush_attempt.y, 
           rushing_yards = rushing_yards.x + rushing_yards.y,
           rush_touchdown = rush_touchdown.x + rush_touchdown.y,
           fumble = fumble.x + fumble.y) %>% 
    select(rush_attempt, rushing_yards, rush_touchdown, fumble, join)
  
  # join stats and calc fpts, dk scoring
  qb_fpts <<- qb_pbp %>% 
    left_join(qb_rush, by=c("join")) %>% # add passer + rusher stat 
    select(-c("join")) %>% # drop the join column
    drop_na(passer) %>% 
    replace(is.na(.),0) %>% 
    mutate(
      big_rush = ifelse(rushing_yards > 100, 1,0), 
      big_pass = ifelse(passing_yards > 300, 1,0), 
      fpts = 
        
        big_pass * 3 +
        big_rush * 3 +
        
        pass_touchdown * 4 +
        passing_yards * .04 +
        interception * -1 +
        
        rushing_yards * .1 +
        rush_touchdown * 6 +
        fumble * -1, 
      
      fpts_ntile = ntile(fpts, 100)
    ) %>% 
    arrange(-fpts) %>% 
    mutate(join = paste(season, week, posteam, passer, sep = "_")) %>% 
    left_join(schedule %>% select(game_id, roof), by=c("game_id")) %>% 
    relocate(c("fpts", "fpts_ntile", "spread_line", "total_line"), .after = posteam)
  
}
qb_fpts_pbp()

# plot
ggplot(qb_fpts, aes(x = temp, y = fpts)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", se=F)

# generalcorrelation
cor(qb_fpts$temp, qb_fpts$fpts)

# look for specific qb
qb <- "T.Tagovailoa"
qb <- "J.Love"
qb <- "P.Mahomes"
qb <- "K.Murray"
qb <- "A.Richardson"
qb <- "L.Jackson"
qb <- "J.Goff"

# filter data set
qb_data <- qb_fpts %>% 
  filter(passer == qb) %>% 
  filter(temp != 0)

# fit linear model
model <- lm(fpts ~ temp, data = qb_data)
slope <- round(model$coefficients[2], digits = 4)
y_int <- round(model$coefficients[1], digits = 1)
equation <- paste0("y = ", slope, "x + ", y_int)
equation

# plot
ggplot(qb_data %>% filter(season_type == "REG"), 
       aes(x = temp, y = fpts)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", se=F) +
  geom_label_repel(aes(label = game_id), 
                   box.padding = 0.5, 
                   point.padding = 0.5) +
  annotate("text", 
           x = 40, 
           y = 0, 
           label = equation)

# qb specific correlation
cor(qb_data$temp, qb_data$fpts)

# correlation by player
correlation_by_player <- qb_fpts %>% 
  filter(temp != 0) %>% 
  filter(snaps > 10) %>% 
  mutate(games_below_32 = if_else(temp<32,1,0)) %>% 
         #dome_games = if_else(roof == "dome" | roof == "closed",1,0)
  group_by(passer, passer_id) %>% 
  summarize(
    total_fpts = sum(fpts),
    total_epa = sum(epa),
    temp_correlation = round(cor(temp, fpts), digits = 2), 
    games_below_32 = sum(games_below_32),
    #dome_games = sum(dome_games),
    games = n()) %>% 
  arrange(-total_fpts) %>% 
  ungroup() # can not slice while data is grouped

correlation_by_player %>% slice_head(n=10)

# Plot visual table
temp_plot <- ggplot(correlation_by_player %>% slice_head(n=10), 
       aes(x = 1, y = reorder(passer, total_fpts))) +
  # Add NFL headshots
  geom_nfl_headshots(aes(player_gsis = passer_id.x), width = 0.1) +
  
  # Add player names
  geom_text(aes(label = passer), hjust = -0.5, size = 5) +
  
  # Add total fantasy points
  geom_text(aes(x = 2, label = round(total_fpts, 1)), hjust = 0, size = 5) +
  
  # Add correlation
  geom_text(aes(x = 3, label = round(correlation, 2)), hjust = 0, size = 5) +
  # Adjust x-axis labels and titles
  scale_x_continuous(
    breaks = c(1, 2, 3),
    labels = c("Player", "Total Fantasy Points", "Correlation"),
    expand = expansion(mult = c(0.1, 0.5))
  ) +
  # Theme adjustments for a clean look
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank()
  ) +
  labs(title = "Player Statistics with Headshots") + 
  theme_dark()

ggsave(filename = "./03_plots/qbs/temperature correlation.png", 
       plot = temp_plot, 
       width = 8, height = 6, dpi = 300)
