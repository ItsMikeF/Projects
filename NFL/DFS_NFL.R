#load packages
library(nflfastR)
library(tidyverse)
library(ggrepel)

#csv Imports
nfl_salaries <- read.csv("DKSalaries.csv")

nfl_pff_dk_own <- read.csv("dk-ownership.csv")
nfl_pff_projections <- read.csv("projections.csv")

nfl_pff_qb <- read.csv("passing_summary.csv")
nfl_pff_passing_concept <- read.csv("passing_concept.csv")
nfl_pff_passing_pressure_blitz <- read.csv("passing_pressure_blitz.csv")

nfl_pff_wr <- read.csv("receiving_summary.csv")
nfl_pff_receiving_scheme <- read.csv("receiving_scheme.csv")

nfl_pff_rb <- read.csv("rushing_summary.csv")

nfl_pff_pblk <- read.csv("line_pass_blocking_efficiency.csv")

nfl_pff_chart_wr_cb_matchup <- read.csv("wr_cb_matchup_chart.csv")
nfl_pff_chart_te_matchup <- read.csv("te_matchup_chart.csv")
nfl_pff_chart_oline_dline_matchup <- read.csv("oline_dline_matchup_chart.csv")

#Chart Adjustments
nfl_pff_chart_wr_cb_matchup <- nfl_pff_chart_wr_cb_matchup %>% 
  filter(defPlayer == 'All Defenders')

nfl_salaries <- replace(nfl_salaries, nfl_salaries == 'Eli Mitchell', 'Elijah Mitchell')

nfl_pff_pblk <- replace(nfl_pff_pblk, nfl_pff_pblk == 'ARZ', 'ARI')
nfl_pff_pblk <- replace(nfl_pff_pblk, nfl_pff_pblk == 'BLT', 'BAL')
nfl_pff_pblk <- replace(nfl_pff_pblk, nfl_pff_pblk == 'CLV', 'CLE')
nfl_pff_pblk <- replace(nfl_pff_pblk, nfl_pff_pblk == 'HST', 'HOU')
nfl_pff_pblk <- replace(nfl_pff_pblk, nfl_pff_pblk == 'JAX', 'JAC')
nfl_pff_pblk <- replace(nfl_pff_pblk, nfl_pff_pblk == 'LA', 'LAR')
nfl_pff_pblk$pbe_rank <- round(rank(-nfl_pff_pblk$pbe), digits = 0)
nfl_pff_pblk$pbe_sd <- round((nfl_pff_pblk$pbe - mean(nfl_pff_pblk$pbe, na.rm=T)) / sd(nfl_pff_pblk$pbe, na.rm = T), digits = 2)
nfl_pff_pblk <- replace(nfl_pff_pblk, nfl_pff_pblk =='JAC','JAX')

nfl_pff_chart_oline_dline_matchup <- replace(nfl_pff_chart_oline_dline_matchup, nfl_pff_chart_oline_dline_matchup == 'ARZ', 'ARI')
nfl_pff_chart_oline_dline_matchup <- replace(nfl_pff_chart_oline_dline_matchup, nfl_pff_chart_oline_dline_matchup == 'BLT', 'BAL')
nfl_pff_chart_oline_dline_matchup <- replace(nfl_pff_chart_oline_dline_matchup, nfl_pff_chart_oline_dline_matchup == 'CLV', 'CLE')
nfl_pff_chart_oline_dline_matchup <- replace(nfl_pff_chart_oline_dline_matchup, nfl_pff_chart_oline_dline_matchup == 'HST', 'HOU')
nfl_pff_chart_oline_dline_matchup <- replace(nfl_pff_chart_oline_dline_matchup, nfl_pff_chart_oline_dline_matchup == 'LA', 'LAR')

nfl_pff_chart_wr_cb_matchup <- replace(nfl_pff_chart_wr_cb_matchup, nfl_pff_chart_wr_cb_matchup =='ARZ','ARI')
nfl_pff_chart_wr_cb_matchup <- replace(nfl_pff_chart_wr_cb_matchup, nfl_pff_chart_wr_cb_matchup =='BLT','BAL')
nfl_pff_chart_wr_cb_matchup <- replace(nfl_pff_chart_wr_cb_matchup, nfl_pff_chart_wr_cb_matchup =='CLV','CLE')
nfl_pff_chart_wr_cb_matchup <- replace(nfl_pff_chart_wr_cb_matchup, nfl_pff_chart_wr_cb_matchup =='HST','HOU')
nfl_pff_chart_wr_cb_matchup <- replace(nfl_pff_chart_wr_cb_matchup, nfl_pff_chart_wr_cb_matchup =='JAX','JAC')
nfl_pff_chart_wr_cb_matchup <- replace(nfl_pff_chart_wr_cb_matchup, nfl_pff_chart_wr_cb_matchup =='LA','LAR')

nfl_pff_chart_wr_cb_matchup <- replace(nfl_pff_chart_wr_cb_matchup, nfl_pff_chart_wr_cb_matchup =='D.K. Metcalf','DK Metcalf')
nfl_pff_projections <- replace(nfl_pff_projections, nfl_pff_projections =='D.K. Metcalf','DK Metcalf')
nfl_pff_receiving_scheme <- replace(nfl_pff_receiving_scheme, nfl_pff_receiving_scheme =='D.K. Metcalf','DK Metcalf')
nfl_pff_wr <- replace(nfl_pff_wr, nfl_pff_wr =='D.K. Metcalf','DK Metcalf')

nfl_pff_chart_wr_cb_matchup$advantage <- round(nfl_pff_chart_wr_cb_matchup$advantage, digits = 1)
nfl_pff_chart_wr_cb_matchup$expectedSnaps <- round(nfl_pff_chart_wr_cb_matchup$expectedSnaps, digits = 1)

nfl_salaries$week <- max(nfl_2021$week)+1

#QB
nfl_qb <- nfl_salaries %>%
  left_join(nfl_pff_qb, by = c('Name' = 'player'))
         
nfl_qb <- nfl_qb %>% 
  left_join(nfl_pff_dk_own, by = c('Name' = 'player')) 

nfl_qb <- nfl_qb %>%
  filter(Position == "QB" &
           ownership > 0)

nfl_qb <- nfl_qb %>% 
  left_join(nfl_pff_pblk, by = c('TeamAbbrev' = 'team_name'))

nfl_qb <- nfl_qb %>% 
  left_join(nfl_pff_passing_concept, by = c('Name' = 'player'))

player <- nfl_pff_passing_pressure_blitz$player
blitz_dropbacks_percent <- nfl_pff_passing_pressure_blitz$blitz_dropbacks_percent / 100
blitz_grades_pass <- nfl_pff_passing_pressure_blitz$blitz_grades_pass

nfl_qb_blitz <- tibble(player,blitz_dropbacks_percent,blitz_grades_pass)

nfl_qb <- nfl_qb %>% 
  left_join(nfl_qb_blitz, by = c('Name' = 'player'))

nfl_qb <- nfl_qb %>% 
  left_join(nfl_2021_def, by = c('opponent' = 'defteam'))

nfl_qb <- nfl_qb %>% 
  left_join(nfl_pff_def_table, by = c('opponent' = 'team_name'))

nfl_qb <- nfl_qb %>% 
  left_join(team_blitz, by = c('opponent' = 'TeamAbbrev'))

nfl_qb <- nfl_qb %>% 
  left_join(nfl_pff_projections, by = c('Name' = 'playerName'))

nfl_qb$points_per_dollar <- round(nfl_qb$fantasyPoints / (nfl_qb$Salary/100), digits = 3)

nfl_qb$name_salary <- paste(nfl_qb$Name, nfl_qb$Salary)
nfl_qb$name_salary_own <- paste(nfl_qb$Name, nfl_qb$Salary, nfl_qb$ownership)

nfl_qb$btt_twp_ratio <- round(nfl_qb$btt_rate / nfl_qb$twp_rate, digits = 1)
nfl_qb$btt_twp_ratio_sd <- round((nfl_qb$btt_twp_ratio - mean(nfl_qb$btt_twp_ratio, na.rm=T)) / sd(nfl_qb$btt_twp_ratio, na.rm = T), digits = 2)

nfl_qb$dropbacks_game <- round(nfl_qb$dropbacks.x / nfl_qb$player_game_count, digits = 1)

nfl_qb$expected_blitz_rate <- round(rowMeans(nfl_qb[ , c("blitz_dropbacks_percent","blitz_rate")], na.rm=TRUE), digits = 3)
nfl_qb$blitz_grades_pass_sq_exp_blitz_rate <- round(nfl_qb$blitz_grades_pass^2 * nfl_qb$expected_blitz_rate, digits = -1)
nfl_qb$blitz_grades_pass_sq_exp_blitz_rate_sd <- round((nfl_qb$blitz_grades_pass_sq_exp_blitz_rate - mean(nfl_qb$blitz_grades_pass_sq_exp_blitz_rate, na.rm=T)) / sd(nfl_qb$blitz_grades_pass_sq_exp_blitz_rate, na.rm = T), digits = 2)

nfl_qb$blitz_grades_pass_sq_blitz_rate <- round(nfl_qb$blitz_grades_pass^2 * nfl_qb$blitz_rate, digits = -1)
nfl_qb$blitz_grades_pass_sq_blitz_rate_sd <- round((nfl_qb$blitz_grades_pass_sq_blitz_rate - mean(nfl_qb$blitz_grades_pass_sq_blitz_rate, na.rm=T)) / sd(nfl_qb$blitz_grades_pass_sq_blitz_rate, na.rm = T), digits = 2)

nfl_qb$grades_pass_sd <- round((nfl_qb$grades_pass - weighted.mean(nfl_qb$grades_pass, nfl_qb$dropbacks.x, na.rm=T)) / sd(nfl_qb$grades_pass, na.rm = T), digits = 2)
nfl_qb$grades_pass_multiplier <- round(nfl_qb$grades_pass / weighted.mean(nfl_qb$grades_pass, nfl_qb$dropbacks.x, na.rm=T), digits = 2)

nfl_qb$sum_sd <- round(
  (0.35 * nfl_qb$grades_pass_sd) + 
  (0.05 * nfl_qb$btt_twp_ratio_sd) +
  (0.20 * nfl_qb$def_pass_epa_sd) + 
  (0.20 * (nfl_qb$total_rec_salary_sd - nfl_qb$cov_sd)) +
  (0.15 * nfl_qb$blitz_grades_pass_sq_blitz_rate_sd) +
  (0.05 * (nfl_qb$pbe_sd - nfl_qb$prsh_sd)) , digits =3)

nfl_qb$adj_fpts <- round(nfl_qb$fantasyPoints * nfl_qb$grades_pass_multiplier / (nfl_qb$cov_multiplier), digits = 1)

nfl_qb %>%
  select(Name,
         TeamAbbrev,
         Salary,
         ownership,
         fantasyPoints,
         adj_fpts,
         sum_sd,
         points_per_dollar,
         opponent, 
         def_pass_epa,
         def_pass_epa_rank,
         def_rank, 
         cov_rank,
         grades_pass,
         grades_pass_sd,
         def_pass_epa_sd,
         dropbacks.x,
         pbe_rank,
         prsh_rank,
         btt_twp_ratio,
         dropbacks_game,
         blitz_dropbacks_percent,
         blitz_grades_pass,
         blitz_rate,
         bltz_rank,
         expected_blitz_rate,
         blitz_grades_pass_sq_blitz_rate,
         blitz_grades_pass_sq_blitz_rate_sd,
         blitz_grades_pass_sq_exp_blitz_rate,
         blitz_grades_pass_sq_exp_blitz_rate_sd,
         total_rec_salary,
         total_rec_salary_sd,
         cov_sd,
         name_salary_own) %>%
  arrange(-sum_sd) %>% 
  view(title = "NFL QBs")

#QB Chart
nfl_qb <- nfl_qb %>% 
  rename(blitz_rate = Bltz.)

nfl_qb_chart <- nfl_qb %>% 
  filter(Salary >4500)

nfl_qb_chart %>%
  ggplot(aes(x = btt_rate , y = twp_rate)) +
  geom_hline(yintercept = mean(nfl_qb_chart$twp_rate), color = "red", linetype = "dashed", alpha=0.5) +
  geom_vline(xintercept =  mean(nfl_qb_chart$btt_rate), color = "red", linetype = "dashed", alpha=0.5) +
  geom_point(color = nfl_qb_chart$team_color, cex = 5, alpha = .6) +
  geom_smooth(method = "lm", se = F) +
  geom_text_repel(aes(label=name_salary)) +
  labs(x = "Big Time Throw %",
       y = "Turnover Worthy Play %",
       title = paste("QBs, NFL Weeks 1-",max(nfl_2021$week)),
       caption = "Twitter: Its_MikeF | Data: PFF") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold")) +
  #scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_reverse(n.breaks = 10)

nfl_qb_chart %>%
  ggplot(aes(x = sum_sd , y = fantasyPoints)) +
  geom_hline(yintercept = mean(nfl_qb_chart$fantasyPoints, na.rm = T), color = "red", linetype = "dashed", alpha=0.5) +
  geom_vline(xintercept =  mean(nfl_qb_chart$sum_sd, na.rm = T), color = "red", linetype = "dashed", alpha=0.5) +
  geom_point(color = nfl_qb_chart$team_color, cex = 5, alpha = .6) +
  geom_smooth(method = "lm") +
  geom_text_repel(aes(label=name_salary_own)) +
  labs(x = "sum_sd",
       y = "fantasyPoints",
       title = paste("QBs, NFL Weeks 1-",max(nfl_2021$week)),
       caption = "Twitter: Its_MikeF | Data: PFF") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

#RB
nfl_rb <- nfl_salaries %>%
  left_join(nfl_pff_rb, by = c('Name' = 'player'))

nfl_rb <- nfl_rb %>% 
  left_join(nfl_pff_dk_own, by = c('Name' = 'player'))

nfl_rb <- nfl_rb %>%
  filter(Position == "RB" &
           ownership > 0)

nfl_rb <- nfl_rb %>% 
  left_join(nfl_2021_def, by = c('opponent' = 'defteam'))

nfl_rb <- nfl_rb %>% 
  left_join(nfl_2021_off, by = c('opponent' = 'posteam'))

nfl_rb <- nfl_rb %>%
  left_join(nfl_pff_chart_oline_dline_matchup, by = c('TeamAbbrev' = 'offTeam'))

nfl_rb <- nfl_rb %>% 
  left_join(nfl_pff_projections, by = c('Name' = 'playerName'))

nfl_rb <- nfl_rb %>% 
  left_join(nfl_pff_def_table, by = c('opponent' = 'team_name'))

nfl_rb$name_salary <- paste(nfl_rb$Name, nfl_rb$Salary)
nfl_rb$name_salary_own <- paste(nfl_rb$Name, nfl_rb$Salary, nfl_rb$ownership)


nfl_rb$touches_game <- round(nfl_rb$total_touches / nfl_rb$player_game_count, digits = 1)
nfl_rb$mtf_per_attempt <- round(nfl_rb$elu_rush_mtf / nfl_rb$rushAtt, digits = 1)

nfl_rb$runBlockAdv_sd <- round((nfl_rb$runBlockAdv - mean(nfl_rb$runBlockAdv, na.rm=T)) / sd(nfl_rb$runBlockAdv, na.rm = T), digits = 2)
nfl_rb$yco_attempt_sd <- round((nfl_rb$yco_attempt - mean(nfl_rb$yco_attempt, na.rm=T)) / sd(nfl_rb$yco_attempt, na.rm = T), digits = 2)
nfl_rb$touches_game_sd <- round((nfl_rb$touches_game - mean(nfl_rb$touches_game, na.rm=T)) / sd(nfl_rb$touches_game, na.rm = T), digits = 2)

nfl_rb$sum_sd <- round(
  (0.10 * nfl_rb$runBlockAdv_sd) +
  (0.10 * nfl_rb$off_rush_epa_sd) -
  (0.15 * nfl_rb$def_rush_epa_sd) - 
  (0.15 * nfl_rb$rdef_sd) + 
  (0.10 * (nfl_rb$yco_attempt_sd - nfl_rb$tack_sd)) +
  (0.40 * nfl_rb$touches_game_sd), digits = 3)

nfl_rb %>% 
  select(Name,
         TeamAbbrev,
         Salary,
         ownership,
         fantasyPoints,
         sum_sd,
         runBlockAdv,
         opponent,
         def_rush_epa,
         def_rush_epa_rank,
         rdef_rank,
         mtf_per_attempt,
         breakaway_percent,
         elusive_rating,
         tack_rank,
         grades_offense,
         touches_game,
         yco_attempt,
         yprr, 
         name_salary) %>%
  arrange(-sum_sd) %>%
  view(title = "NFL RBs")

#RB Chart
nfl_rb %>%
  ggplot(aes(x = sum_sd , y = fantasyPoints)) +
  geom_hline(yintercept = mean(nfl_rb$fantasyPoints, na.rm = T), color = "red", linetype = "dashed", alpha=0.5) +
  geom_vline(xintercept =  mean(nfl_rb$sum_sd, na.rm = T), color = "red", linetype = "dashed", alpha=0.5) +
  geom_point(color = nfl_rb$team_color.x, cex = 5, alpha = .6) +
  geom_smooth(method = "lm") +
  geom_text_repel(aes(label=name_salary_own)) +
  labs(x = "sum_sd",
       y = "fantasyPoints",
       title = paste("RBs, NFL Weeks 1-",max(nfl_2021$week)),
       caption = "Twitter: Its_MikeF | Data: PFF") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

#WR
nfl_wr <- nfl_salaries %>%
  left_join(nfl_pff_wr, by = c('Name' = 'player'))

nfl_wr <- nfl_wr %>% 
  left_join(nfl_pff_dk_own, by = c('Name' = 'player'))

nfl_wr <- nfl_wr %>%
  filter(Position == "WR" &
           ownership > 0)

nfl_wr <- nfl_wr %>% 
  left_join(nfl_pff_chart_wr_cb_matchup, by = c('Name' = 'offPlayer'))

nfl_wr <- nfl_wr %>% 
  left_join(nfl_2021_def, by = c('opponent' = 'defteam'))

nfl_wr <- nfl_wr %>% 
  left_join(defense_coverage_scheme, by = c('opponent' = 'team_name'))

nfl_wr <- nfl_wr %>% 
  left_join(nfl_pff_receiving_scheme, by = c('Name' = 'player'))

nfl_wr <- nfl_wr %>% 
  left_join(nfl_pff_projections, by = c('Name' = 'playerName'))

nfl_wr <- nfl_wr %>% 
  left_join(nfl_pff_def_table, by = c('opponent' = 'team_name'))

nfl_wr$name_salary <- paste(nfl_wr$Name, nfl_wr$Salary)
nfl_wr$name_salary_own <- paste(nfl_wr$Name, nfl_wr$Salary, nfl_wr$ownership)

nfl_wr$yprr_sd <- round((nfl_wr$yprr - mean(nfl_wr$yprr, na.rm=T)) / sd(nfl_wr$yprr, na.rm = T), digits = 2)
nfl_wr$advantage_sd <- round((nfl_wr$advantage - mean(nfl_wr$advantage, na.rm=T)) / sd(nfl_wr$advantage, na.rm = T), digits = 2)

nfl_wr$man_grade_yprr_man_cov <- round((nfl_wr$man_grades_pass_route * nfl_wr$man_yprr * nfl_wr$man_percentage), digits = 1)
nfl_wr$man_grade_yprr_man_cov_sd <- round((nfl_wr$man_grade_yprr_man_cov - mean(nfl_wr$man_grade_yprr_man_cov, na.rm=T)) / sd(nfl_wr$man_grade_yprr_man_cov, na.rm = T), digits = 2)

nfl_wr$sum_sd <- 
  (0.20 * nfl_wr$yprr_sd) + 
  (0.40 * nfl_wr$advantage_sd) - 
  (0.10 * nfl_wr$def_pass_epa_sd) -
  (0.10 * nfl_wr$cov_sd) + 
  (0.20 * nfl_wr$man_grade_yprr_man_cov_sd)

nfl_wr %>%
  select(Name,
         TeamAbbrev,
         Salary,
         ownership,
         fantasyPoints,
         sum_sd,
         yprr,
         advantage,
         def_pass_epa_rank,
         cov_rank,
         opponent,
         man_grade_yprr_man_cov,
         man_pass_plays,
         man_grades_pass_route,
         man_yprr,
         man_percentage,
         man_rank,
         def_man_grade_rank,
         zone_grades_pass_route,
         zone_yprr,
         zone_percentage,
         zone_rank,
         def_zone_grade_rank,
         def_pass_epa,
         AvgPointsPerGame,
         yards,
         avg_depth_of_target,
         targeted_qb_rating,
         targets,
         touchdowns,
         yards_after_catch_per_reception) %>%
  arrange(-sum_sd) %>%
  view(title = "NFL WRs")

#WR Chart
nfl_wr %>%
  ggplot(aes(x = sum_sd , y = fantasyPoints)) +
  geom_hline(yintercept = mean(nfl_wr$fantasyPoints, na.rm = T), color = "red", linetype = "dashed", alpha=0.5) +
  geom_vline(xintercept =  mean(nfl_wr$sum_sd, na.rm = T), color = "red", linetype = "dashed", alpha=0.5) +
  geom_point(color = nfl_wr$team_color, cex = 5, alpha = .6) +
  geom_smooth(method = "lm") +
  geom_text_repel(aes(label=name_salary_own)) +
  labs(x = "sum_sd",
       y = "fantasyPoints",
       title = paste("WRs, NFL Weeks 1-",max(nfl_2021$week)),
       caption = "Twitter: Its_MikeF | Data: PFF") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

#Tight Ends
nfl_te <- nfl_pff_chart_te_matchup

nfl_te <- nfl_te %>% 
  left_join(nfl_salaries, by = c('offPlayer' = 'Name'))

nfl_te <- nfl_te %>% 
  left_join(nfl_pff_dk_own, by = c('offPlayer' = 'player'))

nfl_te <- nfl_te %>% 
  select(offPlayer, 
         TeamAbbrev,
         Salary, 
         ownership,
         offYprr,
         adv,
         offRoutes,
         offTr,
         offFr, 
         offCPct,
         offGrade,
         defPlayer, 
         defGrade) %>% 
  arrange(-offYprr) %>% 
  drop_na() %>% 
  view(title = "NFL TEs")

#Salary Table
nfl_wr_salary_table <- nfl_wr %>%
  group_by(TeamAbbrev) %>%
  summarise(wr_sum_salary = round(sum(salary), digits = 0))

nfl_wr_count <- table(nfl_wr$TeamAbbrev)

nfl_te_salary_table <- nfl_te %>% 
  select(TeamAbbrev, Salary)

names(nfl_te_salary_table)[2] <- 'te_sum_salary'

nfl_reciever_salary <- nfl_wr_salary_table %>% 
  left_join(nfl_te_salary_table, by = c('TeamAbbrev' = 'TeamAbbrev'))

nfl_reciever_salary$total_rec_salary <- nfl_reciever_salary$wr_sum_salary + nfl_reciever_salary$te_sum_salary

nfl_reciever_salary$total_rec_salary <- rowSums(nfl_reciever_salary[,c("wr_sum_salary", "te_sum_salary")], na.rm=TRUE)

nfl_reciever_salary$total_rec_salary_sd <- round((nfl_reciever_salary$total_rec_salary - mean(nfl_reciever_salary$total_rec_salary, na.rm=T)) / sd(nfl_reciever_salary$total_rec_salary, na.rm = T), digits = 2)

nfl_qb <- nfl_qb %>% 
  left_join(nfl_reciever_salary, by = c('TeamAbbrev' = 'TeamAbbrev'))

#Defense
def <- nfl_pff_dk_own %>% 
  filter(position == "D") %>% 
  view(title = "NFL DST")