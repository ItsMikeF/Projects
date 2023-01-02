#lets predict a rbs next game grade based on past game grades and team epa data

#need game grades from every game

# 1.0 Load packages, values, and functions --------------------------------------------

#load packages
suppressMessages({
  library(nflfastR) #nflfastr nflseedr nflplotr
  library(nflreadr) #
  library(tidyverse) #ggplot2 dplyr tibble tidyr purrr forecats 
  library(glue) #interpreted literal strings
  library(xgboost) #extreme gradient boosting
  library(caret) #classification and regression training
})

#define folder to read files from
folder <- "./game_grades/2022_rb"

#load nflfastr pbp data
pbp <- load_pbp(2022)

#create vector of modern team abbreviations
teams <- pull(teams_colors_logos %>% select(team_abbr)) 
teams <- teams[! teams %in% c("LA","OAK","SD","STL")]

#load normalize function
normalize <- function(x){
  return( round((x - min(x,na.rm = T))/( max(x, na.rm = T) - min(x, na.rm = T)), digits=3))
}

#write passing grade csvs to list
combine_csv <- function(end_week) {
  rbs_list <- list()
  
  for (i in 1:16) {
    rushing_summary <- read.csv(glue("{folder}/rushing_summary ({i}).csv")) %>% 
      select(player, team_name, grades_run, attempts, avoided_tackles, gap_attempts, zone_attempts, run_plays, elu_recv_mtf, elu_rush_mtf, yco_attempt, explosive, first_downs, ypa, 
             receptions, routes, rec_yards, targets, yprr, total_touches)
    
    rbs_list[[i]] <- list(rushing_summary %>% mutate(week=i, join=paste0(team_name,week))) 
  }
  
  temp <- bind_rows(rbs_list)
  write.csv(temp, file = "./game_grades/rbs.csv", row.names = F)
  
}
combine_csv

# 1.1 Define Game week ----------------------------------------------------

week = 17
#week <- as.numeric(max(pbp$week))

# 2.0 PFF Def Table -------------------------------------------------------

passing_reports <- c("passing_grades", "passing_depth", "passing_pressure", "passing_concept", "time_in_pocket", "allowed_pressure")
receiving_reports <- c("receiving_grades", "receiving_depth", "receiving_concept", "receiving_scheme")
rushing_reports <- c("rushing_grades")
blocking_reports <- c("blocking_grades", "pass_blocking", "run_blocking", "ol_pbe")
defense_reports <- c("grades_def","grades_prsh", "grades_rdef", "grades_cov", "scheme_cov", "slot_cov", "prp")

def_list <- list()

for (i in 1:(week-1)) {
  nfl_pff_def <- read.csv(glue("./contests/2022_w{i}/pff/defense_summary.csv"))
  
  nfl_pff_def_table <- nfl_pff_def %>%
    group_by(team_name) %>%
    summarise(def = round(weighted.mean(grades_defense, snap_counts_defense), digits = 1),
              rdef = round(weighted.mean(grades_run_defense, snap_counts_run_defense), digits = 1),
              tack = round(weighted.mean(grades_tackle, snap_counts_defense, na.rm = TRUE), digits = 1),
              prsh = round(weighted.mean(grades_pass_rush_defense, snap_counts_pass_rush), digits = 1),
              cov = round(weighted.mean(grades_coverage_defense, snap_counts_coverage), digits =1))
  
  def_list[[i]] <- nfl_pff_def_table %>% 
    mutate(week = i,
           team_name = gsub('ARZ','ARI', team_name), 
           team_name = gsub('BLT','BAL', team_name), 
           team_name = gsub('CLV','CLE', team_name), 
           team_name = gsub('HST','HOU', team_name), 
           #team_name = gsub('JAX','JAC', team_name), 
           team_name = gsub('LA','LAR', team_name), 
           team_name = gsub('LARC','LAC', team_name))
}

pff_def <- bind_rows(def_list) %>% 
  mutate(join_def_week_join = paste0(team_name, week))

# 2.1 pff defense coverage scheme ---------------------------------------------

rdef_list <- list()

for (i in 1:(week-1)) {
  nfl_pff_rdef <- read.csv(glue("./contests/2022_w{i}/pff/run_defense_summary.csv")) %>% 
    mutate(week = i, 
           team_name = gsub('ARZ','ARI', team_name), 
           team_name = gsub('BLT','BAL', team_name), 
           team_name = gsub('CLV','CLE', team_name), 
           team_name = gsub('HST','HOU', team_name), 
           team_name = gsub('JAX','JAC', team_name), 
           team_name = gsub('LA','LAR', team_name),
           team_name = gsub('LARC','LAC', team_name)) 
  
  rdef_list[[i]] <- nfl_pff_rdef %>% 
    select(player,
           team_name,
           week,
           avg_depth_of_tackle, 
           forced_fumbles,
           grades_run_defense,
           grades_tackle, 
           missed_tackles, 
           stops, 
           snap_counts_run) %>% 
    group_by(team_name, week) %>% 
    summarize(avg_depth_of_tackle = round(weighted.mean(avg_depth_of_tackle, snap_counts_run, na.rm = T),digits = 2), 
              forced_fumbles = round(weighted.mean(forced_fumbles, snap_counts_run, na.rm = T),digits = 2),
              grades_run_defense = round(weighted.mean(grades_run_defense, snap_counts_run, na.rm = T), digits = 1),
              grades_tackle = round(weighted.mean(grades_tackle, snap_counts_run, na.rm = T), digits = 1),
              missed_tackles = round(weighted.mean(missed_tackles, snap_counts_run, na.rm = T), digits = 1),
              stops = round(weighted.mean(stops, snap_counts_run, na.rm = T),digits = 1))
}

rdef <- bind_rows(rdef_list) %>% 
  mutate(join = paste0(team_name, week))

# 2.3 Team Defense EPA table -------------------------------------------------

#load defense data grouped by defteam and week
def <- pbp %>% 
  filter(rush == 1 & wp > 0.1 & wp < 0.9 & half_seconds_remaining > 120) %>% 
  group_by(defteam, week) %>% 
  summarize(def_rush_epa = round(sum(epa), digits = 3),
            n_plays = n()) %>% 
  arrange(def_rush_epa) %>% 
  ungroup() %>% 
  mutate(week_join = week)

#create list of team pass def with cumsum per week
d_list <- list()

for (i in seq_along(teams)) {
  d_list[[i]] <- def %>% 
    filter(defteam==teams[i]) %>% 
    arrange(week) %>% 
    mutate(cumsum_d=cumsum(def_rush_epa), 
           cumsum_plays=cumsum(n_plays),
           cumsum_epa_d=round(cumsum(def_rush_epa)/cumsum(n_plays), digits = 3))
}

d_list <- set_names(d_list, teams)
def_rush_epa <- bind_rows(d_list) %>% 
  mutate(join_def_week_join = paste0(defteam,week_join), 
         join_def_week = paste0(defteam,week))

# 3.0 Team Offense EPA table -------------------------------------------------

#load defense data grouped by defteam and week
off <- pbp %>% 
  filter(rush == 1 & wp > 0.1 & wp < 0.9 & half_seconds_remaining > 120) %>% 
  group_by(posteam, week) %>% 
  summarize(off_rush_epa = round(sum(epa), digits = 3),
            n_plays = n()) %>% 
  arrange(off_rush_epa) %>% 
  ungroup() %>% 
  mutate(week_join = week)

#create list of team pass def with cumsum per week
o_list <- list()

for (i in seq_along(teams)) {
  o_list[[i]] <- off %>% 
    filter(posteam==teams[i]) %>% 
    arrange(week) %>% 
    mutate(cumsum_o= cumsum(off_rush_epa), 
           cumsum_plays=cumsum(n_plays),
           cumsum_epa_o=round(cumsum(off_rush_epa)/cumsum(n_plays), digits = 3))
}

o_list <- set_names(o_list, teams)
off_rush_epa <- bind_rows(o_list) %>% 
  mutate(join_off_week_join = paste0(posteam,week_join), 
         join_off_week = paste0(posteam,week))


# 4.0 Collect all RBs on slate --------------------------------------------

slate_rbs <- read.csv(glue("./contests/2022_w{17}/DKSalaries.csv")) %>% 
  filter(Position == "RB" & Salary > 4000) %>% 
  select(Name, Salary) %>% 
  mutate(player = Name) %>% 
  separate(Name, into = c("first_name", "last_name"), sep=" ") %>% 
  mutate(first = substr(first_name, 1, 1), 
         name = paste(first, last_name, sep = ".")) %>% 
  select(player, name, Salary) %>% 
  mutate(fpts = 0)

slate_rbs$player[1]
slate_rbs$name[1]

# 4.1 Single Player analysis ----------------------------------------------

i = as.numeric(1)

for (i in 1:dim(slate_rbs)[1]) {
  
  #start with a single player
  player <- pbp %>% 
    filter(rush == 1 & rusher == slate_rbs$name[i] & week < 17) %>% 
    group_by(rusher, posteam, defteam, week) %>% 
    summarize(pass_attempt = sum(pass_attempt, na.rm = T),
              passing_yards = sum(passing_yards, na.rm = T),
              pass_touchdown = sum(pass_touchdown, na.rm = T),
              interception = sum(interception, na.rm = T),
              
              rushing_yards = sum(rushing_yards, na.rm = T),
              rush_attempt = sum(rush_attempt, na.rm = T),
              rush_touchdown = sum(rush_touchdown, na.rm = T),
              fumble_lost = sum(fumble_lost, na.rm = T),
              
              epa = round(mean(epa), digits = 3),
    ) %>% 
    mutate(big_ry = ifelse(rushing_yards > 99.5, 1,0), 
           fpts = 
             pass_touchdown * 4 +
             passing_yards * .04 +
             interception * -1 +
             rushing_yards * .1 +
             rush_touchdown * 6 +
             fumble_lost * -1 +
             big_ry * 3) %>% 
    select(rusher, epa, fpts, defteam, week, posteam) %>% 
    ungroup() %>% 
    mutate(week_join = week-1,
           join_def_week = paste0(defteam, week), 
           join_off_week = paste0(posteam, week),
           join_def_week_join = paste0(defteam, week_join), 
           join_off_week_join = paste0(posteam, week_join), 
           join = paste0(posteam, week)) %>% 
    arrange(week)
  
  posteam = player$posteam[1]
  
  schedule <- load_schedules(2022) %>% 
    filter(away_team == player$posteam[1] | home_team == player$posteam[1]) %>% 
    select(week, away_team, away_score, home_team, home_score, result, total, spread_line, total_line, roof, surface) %>% 
    mutate(join = paste0(player$posteam[1], week))
  
  # 5.0 Load pff data -------------------------------------------------------
  
  #i = as.numeric(1)
  
  #read csv with rb grades and filter for player
  pff_game_log <- read.csv(glue("game_grades/rbs.csv")) %>% 
    filter(player == slate_rbs$player[i]) %>% 
    separate(player, into = c("first_name", "last_name"), sep=" ") %>% 
    mutate(first = substr(first_name, 1, 1), 
           name = paste(first, last_name, sep = ".")) %>% 
    select(name, team_name, grades_run, run_plays, week, join) %>% 
    mutate(csc = grades_run*run_plays, 
           csa=cumsum(run_plays), 
           csb=cumsum(csc), 
           cumsum_grades_run=round(csb/csa, digits = 1))
  
  # 6.0 Combine it all ------------------------------------------------------
  
  }
  rb <- schedule %>% 
    left_join(player, by=c('join')) %>% 
    left_join(def_rush_epa %>% select(cumsum_epa_d, join_def_week_join), by="join_def_week_join")  %>% 
    
    left_join(off_rush_epa %>% select(off_rush_epa, join_off_week), by="join_off_week") %>% 
    left_join(off_rush_epa %>% select(cumsum_epa_o, join_off_week_join), by="join_off_week_join") %>% 
    
    left_join(pff_def %>% select(def, rdef, tack, rdef, join_def_week_join),by="join_def_week_join") %>% 
    
    left_join(pff_game_log %>% select(join, grades_run, cumsum_grades_run), by=c('join_off_week'='join')) %>% 
    
    select(week, spread_line, total_line, roof, surface, rusher, grades_run, fpts, posteam, week, defteam, def, rdef, tack, cumsum_epa_d, cumsum_epa_o)
  
  #determine next week opponent
  next_week <- load_schedules(2022) %>% filter(week==16) %>% filter(away_team == posteam | home_team == posteam)
  opp <- if_else(next_week$away_team == posteam, next_week$home_team, next_week$away_team)
  
  rb$rusher[week-1] = rb$rusher[1]
  rb$defteam[week-1] = opp
  #rb$week[week-1] = week+1
  
  rb$def[week-1] <- nfl_pff_def_table[nfl_pff_def_table$team_name == opp,][2]
  rb$rdef[week-1] <- nfl_pff_def_table[nfl_pff_def_table$team_name == opp,][5]
  rb$tack[week-1] <- nfl_pff_def_table[nfl_pff_def_table$team_name == opp,][6]
  
  rb$cumsum_epa_d[week-1] <- round(def_rush_epa[def_rush_epa$join_def_week==paste0(opp,week-1) ,][8], digits = 3)
  
  rb$posteam[week-1] <- rb$posteam[week-2]
  rb$cumsum_epa_o[week-1] <- off_rush_epa[off_rush_epa$join_off_week==paste0(rb$posteam[1],week-1) ,][8]
  rb$cumsum_grades_run[week-1] <- rb$cumsum_grades_run[week-2]
  
  # 7.0 Generate Fpts -------------------------------------------------------
  
  #split into training (80%) and testing set (20%)
  data <- rb %>% select(fpts, cumsum_grades_rush, def, rdef, cumsum_epa_d, cumsum_epa_o)
  data$fpts[week-1] <- 0
  
  train = data[1:(week-2), ] %>% drop_na() %>% filter(!grepl("NA", cumsum_epa_o))
  test = data[week-1, ]
  
  data <- data %>% drop_na() %>% filter(!grepl("NA", def)) %>% filter(!grepl("NA", cumsum_epa_o))
  
  #define predictor and response variables in training set
  train_x <- data.matrix(train[,-c(which(colnames(train)=="fpts"))])
  train_y <- train$fpts
  
  #define predictor and response variables in testing set
  test_x <- data.matrix(test[,-c(which(colnames(test)=="fpts"))])
  test_y <- test$fpts
  
  #define final training and testing sets
  xgb_train = xgb.DMatrix(data = train_x, label = train_y)
  xgb_test = xgb.DMatrix(data = test_x, label = test_y)
  
  #define watchlist
  watchlist = list(train=xgb_train, test=xgb_test)
  
  #fit XGBoost model and display training and testing data at each round
  model <- xgb.train(data = xgb_train, max.depth = 3, watchlist=watchlist, nrounds = 10, print_every_n = 1)
  
  #define final model
  final <- xgboost(data = xgb_train, max.depth = 3, nrounds = 10, print_every_n = 1)
  
  #use model to make predictions on test data
  pred_y <- round(predict(final, xgb_test), digits = 2)
  
  importance <- xgb.importance(feature_names = unlist(dimnames(train_x)[2]),
                               model = final)
  #xgb.plot.importance(importance)
  head(importance)
  
  mean((test_y - pred_y)^2) #mse
  caret::MAE(test_y, pred_y) #mae
  caret::RMSE(test_y, pred_y) #rmse
  
  test$fpts[1] <- pred_y
  rb$fpts[week-1] <- pred_y
  
  #add projection to player
  slate_rbs$fpts[which(slate_rbs$name == player$passer[1])] <- pred_y
  
}

view(slate_rbs)
