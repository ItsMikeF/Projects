#lets analyze the nfl dfs slate

#load packages
suppressMessages({
  library(nflfastR) #nflfastr nflseedr nflplotr
  library(tidyverse) #ggplot2 dplyr tibble tidyr purrr forecats 
  library(ggrepel) #automatically position non-overlapping text labels
  library(glue) #interpreted literal strings
  library(gt)
})

week = 8
folder = glue("./01_data/contests/2023_w{sprintf(\"%02d\", week)}")

# nfl dfs salaries

salaries <- read.csv(glue("{folder}/DKSalaries.csv")) %>% 
  select(1,3,6:8) %>% 
  rename_with(~c("pos", "name", "salary", "game_info", "team")) %>% 
  separate(game_info, sep = "@", into = c("alpha", "bravo")) %>% 
  separate(bravo, sep = " ", into = c("charlie", "delta"), extra = "drop") %>% 
  mutate(opp = if_else(team == alpha, charlie, alpha)) %>% 
  select(pos, name, salary, team, opp)

#load the wr matchup table
chart_wr_cb_matchup <- read.csv(glue("{folder}/pff/wr_cb_matchup_chart.csv")) %>% 
  filter(defPlayer == 'All Defenders')

chart_wr_cb_matchup <- chart_wr_cb_matchup %>% 
  replace(., chart_wr_cb_matchup =='ARZ','ARI') %>% 
  replace(., chart_wr_cb_matchup =='BLT','BAL') %>% 
  replace(., chart_wr_cb_matchup =='CLV','CLE') %>% 
  replace(., chart_wr_cb_matchup =='HST','HOU') %>% 
  replace(., chart_wr_cb_matchup =='JAX','JAC') %>% 
  replace(., chart_wr_cb_matchup =='LA','LAR') %>% 
  mutate(advantage = round(chart_wr_cb_matchup$advantage, digits = 1), 
         expectedSnaps = round(chart_wr_cb_matchup$expectedSnaps, digits = 1))

chart_wr_cb_matchup <- replace(chart_wr_cb_matchup, chart_wr_cb_matchup =='D.K. Metcalf','DK Metcalf')

# load recieving summary
receiving_summary <- read.csv(glue("{folder}/pff/receiving_summary.csv"))
receiving_summary <- replace(receiving_summary, receiving_summary =='D.K. Metcalf','DK Metcalf')

# load scheme
receiving_scheme <- read.csv(glue("{folder}/pff/receiving_scheme.csv"))
receiving_scheme <- replace(receiving_scheme, receiving_scheme =='D.K. Metcalf','DK Metcalf')


wr <- salaries %>% filter(pos=="WR") %>%
  left_join(receiving_summary, by = c('name' = 'player')) %>% 
  #left_join(rg, by=c("Name" = "name")) %>% 
  #filter(pos == "WR" & proj_own >= 0) %>% 
  left_join(chart_wr_cb_matchup, by = c('name' = 'offPlayer')) %>% 
  left_join(pbp_def, by = c('opp' = 'defteam')) %>% 
  left_join(defense_coverage_scheme, by = c('opp' = 'team_name')) %>% 
  left_join(receiving_scheme,
            by = c('name' = 'player')) %>% 
  left_join(def_table, by = c('opp' = 'team_name')) %>% 
  left_join(slot, by=c('opp'='team_name'))

wr <- wr %>% 
  mutate(name_salary = paste(name, salary), 
         #name_salary_own = paste(name, salary, proj_own), 
         yprr_sd = round((yprr - mean(yprr, na.rm=T)) / sd(yprr, na.rm = T), digits = 2), 
         advantage_sd = round((advantage - mean(advantage, na.rm=T)) / sd(advantage, na.rm = T), digits = 2), 
         man_grade_yprr_man_cov = round((man_grades_pass_route * man_yprr * man_percentage), digits = 1), 
         man_grade_yprr_man_cov_sd = round((man_grade_yprr_man_cov - mean(man_grade_yprr_man_cov, na.rm=T)) / sd(man_grade_yprr_man_cov, na.rm = T), digits = 2), 
         targets_per_game = round(targets / player_game_count.x, digits = 1),
         targets_per_game_sd = round((targets_per_game - mean(targets_per_game, na.rm=T)) / sd(targets_per_game, na.rm = T), digits = 2), 
         man_zone_yprr_split = man_yprr - zone_yprr)
         #slot_rate_cov = round((slot_rate/100)*slot +(1-slot_rate/100)*wide,digits=1), 
         #adv = grades_pass_route-slot_rate_cov)


wr$sum_sd <- 
  (0.20 * wr$yprr_sd) + 
  (0.20 * wr$targets_per_game_sd) +
  (0.20 * wr$advantage_sd) - 
  (0.10 * wr$def_pass_epa_sd) -
  (0.10 * wr$cov_sd) + 
  (0.20 * wr$man_grade_yprr_man_cov_sd)

wr %>%
  #filter(proj_own != 0) %>% 
  select(name,
         team,
         salary,
         grades_pass_route, 
         advantage,
         targets_per_game,
         yprr,
         sum_sd,
         opp,
         def_pass_epa_rank,
         cov_rank,
         man_zone_yprr_split,
         man_yprr,
         zone_yprr,
         man_rank,
         zone_rank,
         man_pass_plays, 
         man_percentage,
         def_man_grade_rank,
         zone_pass_plays,
         zone_percentage,
         route_rate, 
         slot_rate, 
         slot,
         wide, 
         def_zone_grade_rank,
         def_pass_epa,
         touchdowns,
         yards_after_catch_per_reception) %>%
  arrange(-sum_sd) %>%
  view(title = "WRs")

#Salary Table
wr_salary_table <- wr %>%
  group_by(team) %>%
  summarise(wr_sum_salary = round(mean(salary), digits = 0))

wr_count <- table(wr$team)

te_salary_table <- te %>% 
  group_by(team) %>% 
  summarise(te_sum_salary = round(mean(salary), digits = 0))

names(te_salary_table)[2] <- 'te_sum_salary'

reciever_salary <- wr_salary_table %>% 
  left_join(te_salary_table, by = c('team' = 'team'))

reciever_salary$total_rec_salary <- reciever_salary$wr_sum_salary + reciever_salary$te_sum_salary

reciever_salary$total_rec_salary <- rowSums(reciever_salary[,c("wr_sum_salary", "te_sum_salary")], na.rm=TRUE)

reciever_salary$total_rec_salary_sd <- round((reciever_salary$total_rec_salary - mean(reciever_salary$total_rec_salary, na.rm=T)) / sd(reciever_salary$total_rec_salary, na.rm = T), digits = 2)

wr %>% 
  group_by(team) %>% 
  summarize(total_sum_sd = sum(sum_sd, na.rm = T)) %>% 
  arrange(-total_sum_sd)
