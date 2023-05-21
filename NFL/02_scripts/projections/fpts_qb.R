#lets predict a qb's next game grade based on past game grades and team epa data

# Need game grades from every game

# 1.0 Load packages, values, and functions --------------------------------------------

# Load packages
suppressMessages({
  library(nflfastR) #nflfastr nflseedr nflplotr
  library(nflreadr) #
  library(nflplotR) 
  library(tidyverse) #ggplot2 dplyr tibble tidyr purrr forecats 
  library(glue) #interpreted literal strings
  library(xgboost) #extreme gradient boosting
  library(caret) #classification and regression training
  library(gt)
})

# Load nflfastr pbp data
pbp <- load_pbp(2022)

#create vector of modern team abbreviations
teams <- pull(teams_colors_logos %>% select(team_abbr)) 
teams <- teams[! teams %in% c("LA","OAK","SD","STL")]

# Load normalize function
normalize <- function(x){
  return( round((x - min(x,na.rm = T))/( max(x, na.rm = T) - min(x, na.rm = T)), digits=3))
}

# Write passing grade csvs to list
combine_csv <- function(end_week) {
  qbs_list <- list()
  
  for (i in 1:week) {
    passing_summary <- read.csv(glue("./game_grades/2022_qb/passing_summary ({i}).csv")) %>% 
      select(player, team_name, grades_pass, passing_snaps)
    
    qbs_list[[i]] <- list(passing_summary %>% mutate(week=i, join=paste0(team_name,week))) 
  }
  
  temp <- bind_rows(qbs_list)
  write.csv(temp, file = "./game_grades/qbs.csv", row.names = F)
  
}
#combine_csv(14)


# 1.1 Define Game week ----------------------------------------------------

week = 15
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

# 2.1 pff defense coverage scheme -------------------------------------------------

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


# 4.0 Collect all QBs on slate --------------------------------------------

slate_qbs <- read.csv(glue("./contests/2022_w{18}/DKSalaries.csv")) %>% 
  filter(Position == "QB" & Salary > 4500) %>% 
  select(Name, Salary) %>% 
  mutate(player = Name) %>% 
  separate(Name, into = c("first_name", "last_name"), sep=" ") %>% 
  mutate(first = substr(first_name, 1, 1), 
         name = paste(first, last_name, sep = ".")) %>% 
  select(player, name, Salary) %>% 
  mutate(fpts = 0)

slate_qbs$player[1]
slate_qbs$name[1]

# 4.1 Single Player analysis ----------------------------------------------

i = as.numeric(1)

for (i in 1:dim(slate_qbs)[1]) {
  
#start with a single player
player <- pbp %>% 
  filter(pass == 1 & passer == slate_qbs$name[i] & week < 18) %>% 
  group_by(passer, posteam, defteam, week) %>% 
  summarize(pass_attempt = sum(pass_attempt, na.rm = T),
            passing_yards = sum(passing_yards, na.rm = T),
            pass_touchdown = sum(pass_touchdown, na.rm = T),
            interception = sum(interception, na.rm = T),
            
            rushing_yards = sum(rushing_yards, na.rm = T),
            rush_attempt = sum(rush_attempt, na.rm = T),
            rush_touchdown = sum(rush_touchdown, na.rm = T),
            fumble_lost = sum(fumble_lost, na.rm = T),
            
            epa = round(mean(qb_epa), digits = 3),
            cpoe = round(mean(cpoe, na.rm = T), digits = 2)
            ) %>% 
  mutate(big_py = ifelse(passing_yards > 300, 1,0), 
         fpts = 
           pass_touchdown * 4 +
           passing_yards * .04 +
           interception * -1 +
           rushing_yards * .1 +
           rush_touchdown * 6 +
           fumble_lost * -1 +
           big_py * 3) %>% 
  select(passer, epa, fpts, defteam, week, posteam) %>% 
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

#read csv with qb grades and filter for player
pff_game_log <- read.csv(glue("game_grades/qbs.csv")) %>% 
  filter(player == slate_qbs$player[i]) %>% 
  separate(player, into = c("first_name", "last_name"), sep=" ") %>% 
  mutate(first = substr(first_name, 1, 1), 
         name = paste(first, last_name, sep = ".")) %>% 
  select(name, team_name, grades_pass, passing_snaps, week, join) %>% 
  mutate(csc = grades_pass*passing_snaps, 
         csa=cumsum(passing_snaps), 
         csb=cumsum(csc), 
         cumsum_grades_pass=round(csb/csa, digits = 1))

# 6.0 Combine it all ------------------------------------------------------

{#nfl dfs for reference
#nfl_qb <- nfl_salaries %>%
  #left_join(nfl_pff_qb, by = c('Name' = 'player')) %>% 
  #left_join(rg, by=c("Name" = "name")) %>% 
  #left_join(nfl_pff_dk_own, by = c('Name' = 'player')) %>%
  #filter(pos == "QB" & proj_own > 0) %>% 
  #left_join(nfl_pff_pblk, by = c('TeamAbbrev' = 'team_name')) %>% 
  #left_join(nfl_pff_passing_concept, by = c('Name' = 'player')) %>% 
  #left_join(nfl_reciever_salary, by = c('TeamAbbrev' = 'TeamAbbrev'))
}
qb <- schedule %>% 
  left_join(player, by=c('join')) %>% 
  #left_join(def_pass_epa %>% select(def_pass_epa, join_def_week), by="join_def_week")  %>% 
  left_join(def_pass_epa %>% select(cumsum_epa_d, join_def_week_join), by="join_def_week_join")  %>% 
  
  left_join(off_pass_epa %>% select(off_pass_epa, join_off_week), by="join_off_week") %>% 
  left_join(off_pass_epa %>% select(cumsum_epa_o, join_off_week_join), by="join_off_week_join") %>% 
  
  left_join(pff_def %>% select(def, rdef, tack, prsh, cov, join_def_week_join),by="join_def_week_join") %>% 
  
  left_join(pff_game_log %>% select(join, grades_pass, cumsum_grades_pass), by=c('join_off_week'='join')) %>% 
  
  select(week, spread_line, total_line, roof, surface, passer, grades_pass, cumsum_grades_pass, fpts, posteam, week, defteam, def, prsh, cov, cumsum_epa_d, cumsum_epa_o)

#determine next week opponent
next_week <- load_schedules(2022) %>% filter(week==18) %>% filter(away_team == posteam | home_team == posteam)
opp <- if_else(next_week$away_team == posteam, next_week$home_team, next_week$away_team)

qb$posteam[dim(qb)[1]] = qb$posteam[1]
qb$passer[dim(qb)[1]] = qb$passer[1]
qb$defteam[dim(qb)[1]] = opp
#qb$week[week-1] = week+1

qb$def[dim(qb)[1]] <- nfl_pff_def_table[nfl_pff_def_table$team_name == opp,][2]
qb$prsh[dim(qb)[1]] <- nfl_pff_def_table[nfl_pff_def_table$team_name == opp,][5]
qb$cov[dim(qb)[1]] <- nfl_pff_def_table[nfl_pff_def_table$team_name == opp,][6]

qb$cumsum_epa_d[dim(qb)[1]] <- round(def_pass_epa[def_pass_epa$join_def_week==paste0(opp,dim(qb)[1]) ,][8], digits = 3)

qb$posteam[dim(qb)[1]] <- qb$posteam[week-2]
qb$cumsum_epa_o[dim(qb)[1]] <- off_pass_epa[off_pass_epa$join_off_week==paste0(qb$posteam[1],dim(qb)[1]) ,][8]
qb$cumsum_grades_pass[dim(qb)[1]] <- qb$cumsum_grades_pass[dim(qb)[2]]

# 7.0 Generate Fpts -------------------------------------------------------

#split into training (80%) and testing set (20%)
data <- qb %>% select(fpts, cumsum_grades_pass, def, prsh, cov, cumsum_epa_d, cumsum_epa_o)
data$fpts[dim(qb)[1]] <- 0

train = data[1:(dim(data)[1]-1), ] %>% drop_na() %>% filter(!grepl("NA", cumsum_epa_o))
test = data[dim(data)[1], ]

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
qb$fpts[dim(qb)[1]] <- pred_y

#add projection to player
slate_qbs$fpts[which(slate_qbs$name == player$passer[1])] <- pred_y

}

view(slate_qbs)

game_week = week

slate_qbs %>% 
  left_join(pbp %>% 
              filter(week < game_week) %>% 
              group_by(passer, id, posteam) %>% 
              summarise(yards=sum(passing_yards,na.rm = T)), 
            by=c('name'='passer')) %>% 
  knitr::kable()
  

ggplot2::ggplot(slate_qbs, aes(x = reorder(player, -fpts), y = fpts)) +
  nflplotR::geom_nfl_headshots(aes(player_gsis = passer_id), width = 0.075, vjust = 0.45)
