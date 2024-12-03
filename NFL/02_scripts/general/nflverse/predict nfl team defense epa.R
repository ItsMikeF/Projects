# predict team epa of nfl defenses based on pff grade of espn depth chart

# Load packages
suppressMessages({
  library(nflfastR)
  library(dplyr)
  library(tidyr)
  library(caret)
  library(gt)
  library(gtExtras)
})

# run dfs nfl defense script tp get pbp_def

# run depth chart script for nfl defense

# need to project every player

# load def

# 0.0 Define Inputs -------------------------------------------------------

week = 17
pbp <- load_pbp(2022)
wp_lower = 0.1
wp_upper = 0.9
half_seconds_remaining = 120

# 1.0 defense epa table -------------------------------------------------------

epa_def_team <- lapply(2014:2022, function(year){
  #define the values
  print(paste("Load pbp of the", year, "season"))
  pbp <- load_pbp(year)
  week = 17
  wp_lower = 0.1
  wp_upper = 0.9
  half_seconds_remaining = 120
  print_plot = "no"
  
  #def pass epa
  pbp_def_pass <- pbp %>% 
    filter(pass == 1 &
             wp > 0.1 &
             wp < 0.9 &
             half_seconds_remaining > 120) %>% 
    group_by(defteam) %>% 
    summarize(def_pass_epa = round(mean(epa), digits = 3),
              n_plays = n()) %>% 
    arrange(def_pass_epa) %>% 
    mutate(def_pass_epa_rank = round(rank(def_pass_epa), digits = 0), 
           def_pass_epa_sd = round((def_pass_epa - weighted.mean(def_pass_epa, n_plays, na.rm=T)) / sd(def_pass_epa, na.rm = T), digits = 2))
  
  #def rush epa
  pbp_def_rush <- pbp %>% 
    filter(rush == 1 &
             wp > 0.1 &
             wp < 0.9 &
             half_seconds_remaining > 120) %>% 
    group_by(defteam) %>% 
    summarize(def_rush_epa = round(mean(epa), digits = 3),
              n_plays = n()) %>% 
    arrange(def_rush_epa) %>% 
    mutate(def_rush_epa_rank = round(rank(def_rush_epa), digits = 0), 
           def_rush_epa_sd = round((def_rush_epa - weighted.mean(def_rush_epa, n_plays, na.rm=T)) / sd(def_rush_epa, na.rm = T), digits = 2))
  
  pbp_def <- pbp_def_pass %>% 
    left_join(pbp_def_rush, by = c('defteam')) %>% 
    mutate(total_plays = n_plays.x + n_plays.y,
           #defteam = gsub('JAX','JAC', defteam), 
           defteam = gsub('LA','LAR', defteam), 
           defteam = gsub('LARC','LAC', defteam), 
           year = year)
  
})

# bind data to a dataframe
epa_def_team_df <- bind_rows(epa_def_team) %>% 
  mutate(join = paste0(year, defteam))


# 2.0 load pff defense data -----------------------------------------------

# load pff defense data
pff <- read.csv("./01_data/training_data/position_groups/def.csv")

#group by team by year
pff_team <- pff %>% 
  filter(week == 17) %>% 
  group_by(team_name, year) %>% 
  summarise(rdef = round(weighted.mean(grades_run_defense.x,snap_counts_run_defense, na.rm = T), digits = 1),
            tack = round(weighted.mean(grades_tackle.x, tackles.x, na.rm = T), digits = 1),
            prsh = round(weighted.mean(true_pass_set_grades_pass_rush_defense, true_pass_set_snap_counts_pass_rush, na.rm = T), digits = 1),
            cov = round(weighted.mean(grades_coverage_defense.x, snap_counts_coverage.x, na.rm = T), digits = 1)) %>% 
  ungroup() %>% 
  mutate(team_name = case_when(
           team_name == "LA" ~ "LAR",
           team_name == "LARC" ~ "LAC", 
           team_name == "HST" ~ "HOU", 
           team_name == "ARZ" ~ "ARI", 
           team_name == "BLT" ~ "BAL", 
           team_name == "CLV" ~ "CLE", 
           T ~ team_name
           ),
         join = paste0(year, team_name)
         )


# 3.0 Join pff and pbp dataframes -----------------------------------------



# join dataframes
def <- epa_def_team_df %>% 
  left_join(pff_team %>% select(-c(team_name, year)), by=c("join")) %>% 
  drop_na()

# 4.0 PFF grades of 2023 starting defense ------------------------------------------

# Run espn depth chart defense script

# aggregate pff ratings for 2023 rosters
depth_def_join <- depth_def %>% 
  select(player1, team) %>% 
  left_join(pff %>% filter(week == 17), by=c("player1" = "player")) %>% 
  group_by(team) %>% 
  summarise(
    rdef = round(weighted.mean(grades_run_defense.x,snap_counts_run_defense, na.rm = T), digits = 1),
    tack = round(weighted.mean(grades_tackle.x, tackles.x, na.rm = T), digits = 1),
    prsh = round(weighted.mean(true_pass_set_grades_pass_rush_defense, true_pass_set_snap_counts_pass_rush, na.rm = T), digits = 1),
    cov = round(weighted.mean(grades_coverage_defense.x, snap_counts_coverage.x, na.rm = T), digits = 1) 
  )


# 5,0 Add 2023 pff grades to team dataframe to bind to -----------------------

# add blank df for 2023
teams <- data.frame(matrix(ncol = dim(def)[2]-4, nrow = 32)) 
teams$X11 <- 2023
teams <- cbind(teams, depth_def_join[,2:5])
teams$X1 <- unique(epa_def_team_df$defteam)

names(teams) <- names(def)
teams <- replace(teams, is.na(teams), 0)

def <- rbind(def, teams)


# 6.0 Build model ---------------------------------------------------------


# Step 3: Split the data
set.seed(1)
train_set <- def[which(def$year < 2022),]
test_set  <- def[which(def$year == 2022),]
new_data <- def[which(def$year == 2023),]

# Step 4 Choose a regression model
model <- train(def_pass_epa ~ prsh + cov, data = train_set, method = "lm")

# Step 5 Train the regression model
trained_model <- model$finalModel
trained_model

# Step 6 Evaluate the model
predictions <- predict(trained_model, newdata = new_data )
predictions

# add predictions to def dataframe
new_data$def_pass_epa <- predictions
new_data$def_pass_epa_rank <- rank(new_data$def_pass_epa)


# build run defense model -------------------------------------------------

model_run <- train(def_rush_epa ~ rdef, data = train_set, method = "lm")

trained_model_run <- model_run$finalModel

predictions_run <- predict(trained_model_run, newdata = new_data)

new_data$def_rush_epa <- predictions_run
new_data$def_rush_epa_rank <- rank(new_data$def_rush_epa)

View(new_data)


# build gt table ----------------------------------------------------------

names(teams_colors_logos)
names(new_data)

new_data %>% 
  left_join(teams_colors_logos %>% select(team_abbr, team_logo_espn), by = c("defteam"="team_abbr")) %>% 
  relocate(team_logo_espn, .after = defteam) %>% 
  select(defteam, team_logo_espn, def_pass_epa, def_pass_epa_rank, def_rush_epa, def_rush_epa_rank) %>% 
  arrange(def_pass_epa) %>% 
  gt() %>% 
  gt_img_rows(columns = team_logo_espn) %>% 
  gtsave(filename = "./03_plots/2023 defense rankings.png")


