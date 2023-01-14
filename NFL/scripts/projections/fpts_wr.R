#lets predict a wrs next game grade based on past game grades and team epa data

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
folder <- "./game_grades/2022_wr"

#load nflfastr pbp data
pbp <- load_pbp(2022)

#create vector of modern team abbreviations
teams <- pull(teams_colors_logos %>% select(team_abbr)) 
teams <- teams[! teams %in% c("LA","OAK","SD","STL")]

#load normalize function
normalize <- function(x){
  return( round((x - min(x,na.rm = T))/( max(x, na.rm = T) - min(x, na.rm = T)), digits=3))
}

#write rushing grade csvs to list
combine_csv <- function(end_week) {
  wrs_list <- list()
  
  for (i in 1:16) {
    receiving_summary <- read.csv(glue("{folder}/receiving_summary ({i}).csv")) %>% 
      select(player, team_name, avg_depth_of_target, contested_receptions, contested_targets, drops, grades_offense, grades_pass_route, 
             inline_snaps, pass_plays, receptions, routes, slot_snaps, targeted_qb_rating, targets, touchdowns, 
             wide_snaps, yards, yards_after_catch, yards_after_catch_per_reception, yards_per_reception, yprr
             
)
    
    wrs_list[[i]] <- list(receiving_summary %>% mutate(week=i, join=paste0(team_name,week))) 
  }
  
  temp <- bind_rows(wrs_list)
  write.csv(temp, file = "./game_grades/wrs.csv", row.names = F)
  
}
combine_csv

# 1.1 Define Game week ----------------------------------------------------

week = 17
#week <- as.numeric(max(pbp$week))

# 2.0 PFF Def Table -------------------------------------------------------

rushing_reports <- c("rushing_grades", "rushing_depth", "rushing_pressure", "rushing_concept", "time_in_pocket", "allowed_pressure")
receiving_reports <- c("receiving_grades", "receiving_depth", "receiving_concept", "receiving_scheme")
receiving_reports <- c("receiving_grades")
blocking_reports <- c("blocking_grades", "rush_blocking", "pass_blocking", "ol_pbe")
defense_reports <- c("grades_def","grades_prsh", "grades_rdef", "grades_cov", "scheme_cov", "slot_cov", "prp")

def_list <- list()

for (i in 1:(week-1)) {
  nfl_pff_def <- read.csv(glue("./contests/2022_w{i}/pff/defense_summary.csv"))
  
  nfl_pff_def_table <- nfl_pff_def %>%
    group_by(team_name) %>%
    summarise(def = round(weighted.mean(grades_defense, snap_counts_defense), digits = 1),
              rdef = round(weighted.mean(grades_run_defense, snap_counts_run_defense), digits = 1),
              tack = round(weighted.mean(grades_tackle, snap_counts_defense, na.rm = TRUE), digits = 1),
              prsh = round(weighted.mean(grades_run_defense, snap_counts_run_defense), digits = 1),
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

cov_list <- list()

for (i in 1:(week-1)) {
  nfl_pff_defense_coverage_scheme <- read.csv(glue("./contests/2022_w{i}/pff/defense_coverage_scheme.csv")) %>% 
    mutate(week = i, 
           team_name = gsub('ARZ','ARI', team_name), 
           team_name = gsub('BLT','BAL', team_name), 
           team_name = gsub('CLV','CLE', team_name), 
           team_name = gsub('HST','HOU', team_name), 
           team_name = gsub('JAX','JAC', team_name), 
           team_name = gsub('LA','LAR', team_name),
           team_name = gsub('LARC','LAC', team_name)) 
  
  cov_list[[i]] <- nfl_pff_defense_coverage_scheme %>% 
    select(player,
           team_name,
           week,
           man_snap_counts_coverage, 
           man_snap_counts_coverage_percent,
           man_grades_coverage_defense,
           zone_snap_counts_coverage,
           zone_snap_counts_coverage_percent,
           zone_grades_coverage_defense) %>% 
    group_by(team_name, week) %>% 
    summarize(man_snaps = sum(man_snap_counts_coverage), 
              zone_snaps = sum(zone_snap_counts_coverage),
              def_man_grade = weighted.mean(man_grades_coverage_defense, man_snap_counts_coverage), 
              def_zone_grade = weighted.mean(zone_grades_coverage_defense, zone_snap_counts_coverage)) %>% 
    mutate(man_percentage = round(man_snaps / (man_snaps + zone_snaps), digits = 3), 
           def_man_grade = round(def_man_grade, digits = 1), 
           zone_percentage = 1 - man_percentage, 
           def_zone_grade = round(def_zone_grade, digits = 1))
}

cov_scheme <- bind_rows(cov_list) %>% 
  mutate(join = paste0(team_name, week))

# 2.3 Team Defense EPA table -------------------------------------------------

#load defense data grouped by defteam and week
def <- pbp %>% 
  filter(pass == 1 & wp > 0.1 & wp < 0.9 & half_seconds_remaining > 120) %>% 
  group_by(defteam, week) %>% 
  summarize(def_pass_epa = round(sum(epa), digits = 3),
            n_plays = n()) %>% 
  arrange(def_pass_epa) %>% 
  ungroup() %>% 
  mutate(week_join = week)

#create list of team pass def with cumsum per week
d_list <- list()

for (i in seq_along(teams)) {
  d_list[[i]] <- def %>% 
    filter(defteam==teams[i]) %>% 
    arrange(week) %>% 
    mutate(cumsum_d=cumsum(def_pass_epa), 
           cumsum_plays=cumsum(n_plays),
           cumsum_epa_d=round(cumsum(def_pass_epa)/cumsum(n_plays), digits = 3))
}

d_list <- set_names(d_list, teams)
def_pass_epa <- bind_rows(d_list) %>% 
  mutate(join_def_week_join = paste0(defteam,week_join), 
         join_def_week = paste0(defteam,week))

# 3.0 Team Offense EPA table -------------------------------------------------

#load defense data grouped by defteam and week
off <- pbp %>% 
  filter(pass == 1 & wp > 0.1 & wp < 0.9 & half_seconds_remaining > 120) %>% 
  group_by(posteam, week) %>% 
  summarize(off_pass_epa = round(sum(epa), digits = 3),
            n_plays = n()) %>% 
  arrange(off_pass_epa) %>% 
  ungroup() %>% 
  mutate(week_join = week)

#create list of team pass def with cumsum per week
o_list <- list()

for (i in seq_along(teams)) {
  o_list[[i]] <- off %>% 
    filter(posteam==teams[i]) %>% 
    arrange(week) %>% 
    mutate(cumsum_o= cumsum(off_pass_epa), 
           cumsum_plays=cumsum(n_plays),
           cumsum_epa_o=round(cumsum(off_pass_epa)/cumsum(n_plays), digits = 3))
}

o_list <- set_names(o_list, teams)
off_pass_epa <- bind_rows(o_list) %>% 
  mutate(join_off_week_join = paste0(posteam,week_join), 
         join_off_week = paste0(posteam,week))

# 4.0 Collect all wrs on slate --------------------------------------------

slate_wrs <- read.csv(glue("./contests/2022_w{17}/DKSalaries.csv")) %>% 
  filter(Position == "WR" & Salary > 5000) %>% 
  select(Name, Salary) %>% 
  mutate(player = Name) %>% 
  separate(Name, into = c("first_name", "mid","last_name"), sep=" ") %>% 
  mutate(first = substr(first_name, 1, 1), 
         test = if_else(is.na(last_name), mid, last_name),
         name = paste(first, test, sep = ".")) %>% 
  select(player, name, Salary) %>% 
  mutate(fpts = 0)

slate_wrs$player[1]
slate_wrs$name[1]

# 4.1 Single Player analysis ----------------------------------------------

i = as.numeric(1)

for (i in 1:dim(slate_wrs)[1]) {
  
  #start with a single player
  player <- pbp %>% 
    filter(pass == 1 & receiver == slate_wrs$name[i] & week < 17) %>% 
    group_by(receiver, posteam, defteam, week) %>% 
    summarize(rush_attempt = sum(rush_attempt, na.rm = T),
              rushing_yards = sum(rushing_yards, na.rm = T),
              rush_touchdown = sum(rush_touchdown, na.rm = T),
              interception = sum(interception, na.rm = T),
              
              receiving_yards = sum(receiving_yards, na.rm = T),
              rush_attempt = sum(rush_attempt, na.rm = T),
              rush_touchdown = sum(rush_touchdown, na.rm = T),
              fumble_lost = sum(fumble_lost, na.rm = T),
              
              epa = round(mean(epa), digits = 3),
    ) %>% 
    mutate(big_ry = ifelse(receiving_yards > 99.5, 1,0), 
           fpts = 
             rush_touchdown * 4 +
             rushing_yards * .04 +
             interception * -1 +
             receiving_yards * .1 +
             rush_touchdown * 6 +
             fumble_lost * -1 +
             big_ry * 3) %>% 
    select(receiver, epa, fpts, defteam, week, posteam) %>% 
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
  
  #read csv with wr grades and filter for player
  pff_game_log <- read.csv(glue("game_grades/wrs.csv")) %>% 
    filter(player == slate_wrs$player[i]) %>% 
    separate(player, into = c("first_name", "last_name"), sep=" ") %>% 
    mutate(first = substr(first_name, 1, 1), 
           name = paste(first, last_name, sep = ".")) %>% 
    select(name, team_name, grades_pass_route, routes, week, join) %>% 
    mutate(csc = grades_pass_route*routes, 
           csa=cumsum(routes), 
           csb=cumsum(csc), 
           cumsum_grades_pass=round(csb/csa, digits = 1))
  
  # 6.0 Combine it all ------------------------------------------------------
  
  
  wr <- schedule %>% 
    left_join(player, by=c('join')) %>% 
    left_join(def_pass_epa %>% select(cumsum_epa_d, join_def_week_join), by="join_def_week_join")  %>% 
    
    left_join(off_pass_epa %>% select(off_pass_epa, join_off_week), by="join_off_week") %>% 
    left_join(off_pass_epa %>% select(cumsum_epa_o, join_off_week_join), by="join_off_week_join") %>% 
    
    left_join(pff_def %>% select(def, prsh, cov, join_def_week_join),by="join_def_week_join") %>% 
    
    left_join(pff_game_log %>% select(join, grades_pass_route, cumsum_grades_pass), by=c('join_off_week'='join')) %>% 
    
    select(week, spread_line, total_line, roof, surface, receiver, grades_pass_route, cumsum_grades_pass, fpts, posteam, week, defteam, def, prsh, cov, cumsum_epa_d, cumsum_epa_o)
  
  #determine next week opponent
  next_week <- load_schedules(2022) %>% filter(week==17) %>% filter(away_team == posteam | home_team == posteam)
  opp <- if_else(next_week$away_team == posteam, next_week$home_team, next_week$away_team)
  
  wr$receiver[week-1] = wr$receiver[1]
  wr$defteam[week-1] = opp
  #wr$week[week-1] = week+1
  
  wr$def[week-1] <- nfl_pff_def_table[nfl_pff_def_table$team_name == opp,][2]
  wr$prsh[week-1] <- nfl_pff_def_table[nfl_pff_def_table$team_name == opp,][5]
  wr$cov[week-1] <- nfl_pff_def_table[nfl_pff_def_table$team_name == opp,][6]
  
  wr$cumsum_epa_d[week-1] <- round(def_pass_epa[def_pass_epa$join_def_week==paste0(opp,week-1) ,][8], digits = 3)
  
  wr$posteam[week-1] <- wr$posteam[week-2]
  wr$cumsum_epa_o[week-1] <- off_pass_epa[off_pass_epa$join_off_week==paste0(wr$posteam[1],week-1) ,][8]
  wr$cumsum_grades_pass[week-1] <- wr$cumsum_grades_pass[week-2]
  wr$week.y[week-1] = week
  
  # 7.0 Generate Fpts -------------------------------------------------------
  
  #split into training (80%) and testing set (20%)
  data <- wr %>% select(fpts, cumsum_grades_pass, def, cov, cumsum_epa_d, cumsum_epa_o)
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
  wr$fpts[week-1] <- pred_y
  
  #add projection to player
  slate_wrs$fpts[which(slate_wrs$name == player$receiver[1])] <- pred_y
  
}

view(slate_wrs)