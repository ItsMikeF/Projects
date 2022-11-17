#lets analyze the nfl dfs slate

#load packages
suppressMessages({
  library(nflfastR) #nflfastr nflseedr nflplotr
  library(tidyverse) #ggplot2 dplyr tibble tidyr purrr forecats 
  library(ggrepel) #automatically position non-overlapping text labels
  library(glue) #interpreted literal strings
  library(gt)
})


# 0.0 define team labels --------------------------------------------------

team_names <- c('ARZ'='ARI', 'BLT'='BAL', 'CLV'='CLE', 'HST'='HOU', 'JAX'='JAC', 'LA'='LAR')

# 1.0 load and clean files ------------------------------------------------

week <- 11

# 1.1 load dk slate -------------------------------------------------------

nfl_salaries <- read.csv(glue("./contests/2022_w{week}/DKSalaries.csv"))
nfl_salaries <- replace(nfl_salaries, nfl_salaries == 'Eli Mitchell', 'Elijah Mitchell')

# 1.2 load dk ownership ---------------------------------------------------

nfl_pff_dk_own <- read.csv(glue("./contests/2022_w{week}/pff/dk-ownership.csv"))

# 1.3 load projections ----------------------------------------------------

nfl_pff_projections <- read.csv(glue("./contests/2022_w{week}/pff/projections.csv"))
nfl_pff_projections <- replace(nfl_pff_projections, nfl_pff_projections =='D.K. Metcalf','DK Metcalf')

#proj_rg <- read.csv(glue("./contests/2022_w{week}/projections.csv"))

# 1.4 load qb data --------------------------------------------------------

nfl_pff_qb <- read.csv(glue("./contests/2022_w{week}/pff/passing_summary.csv"))
nfl_pff_passing_concept <- read.csv(glue("./contests/2022_w{week}/pff/passing_concept.csv"))
nfl_pff_passing_pressure_blitz <- read.csv(glue("./contests/2022_w{week}/pff/passing_pressure.csv"))

# 1.5 load wr data --------------------------------------------------------

nfl_pff_wr <- read.csv(glue("./contests/2022_w{week}/pff/receiving_summary.csv"))
nfl_pff_wr <- replace(nfl_pff_wr, nfl_pff_wr =='D.K. Metcalf','DK Metcalf')

nfl_pff_receiving_scheme <- read.csv(glue("./contests/2022_w{week}/pff/receiving_scheme.csv"))
nfl_pff_receiving_scheme <- replace(nfl_pff_receiving_scheme, nfl_pff_receiving_scheme =='D.K. Metcalf','DK Metcalf')

#load the wr matchup table
nfl_pff_chart_wr_cb_matchup <- read.csv(glue("./contests/2022_w{week}/pff/wr_cb_matchup_chart.csv")) %>% 
  filter(defPlayer == 'All Defenders')

nfl_pff_chart_wr_cb_matchup <- nfl_pff_chart_wr_cb_matchup %>% 
  replace(., nfl_pff_chart_wr_cb_matchup =='ARZ','ARI') %>% 
  replace(., nfl_pff_chart_wr_cb_matchup =='BLT','BAL') %>% 
  replace(., nfl_pff_chart_wr_cb_matchup =='CLV','CLE') %>% 
  replace(., nfl_pff_chart_wr_cb_matchup =='HST','HOU') %>% 
  replace(., nfl_pff_chart_wr_cb_matchup =='JAX','JAC') %>% 
  replace(., nfl_pff_chart_wr_cb_matchup =='LA','LAR') %>% 
  mutate(advantage = round(nfl_pff_chart_wr_cb_matchup$advantage, digits = 1), 
         expectedSnaps = round(nfl_pff_chart_wr_cb_matchup$expectedSnaps, digits = 1))

nfl_pff_chart_wr_cb_matchup <- replace(nfl_pff_chart_wr_cb_matchup, nfl_pff_chart_wr_cb_matchup =='D.K. Metcalf','DK Metcalf')

# 1.6 load rb data --------------------------------------------------------

nfl_pff_rb <- read.csv(glue("./contests/2022_w{week}/pff/rushing_summary.csv"))

# 1.7 load te data --------------------------------------------------------

nfl_pff_chart_te_matchup <- read.csv(glue("./contests/2022_w{week}/pff/te_matchup_chart.csv"))

# 1.8 load ol data --------------------------------------------------------

nfl_pff_pblk <- read.csv(glue("./contests/2022_w{week}/pff/line_pass_blocking_efficiency.csv"))
nfl_pff_pblk <- nfl_pff_pblk %>% 
  mutate(across('team_name', str_replace, 'ARZ', 'ARI'),
         across('team_name', str_replace, 'BLT', 'BAL'), 
         across('team_name', str_replace, 'CLV', 'CLE'), 
         across('team_name', str_replace, 'HST', 'HOU'),
         across('team_name', str_replace, 'LA', 'LAR'), 
         across('team_name', str_replace, 'LARC', 'LAC')) %>% 
  mutate(pbe_rank = round(rank(-nfl_pff_pblk$pbe), digits = 0), 
         pbe_sd = round((nfl_pff_pblk$pbe - mean(nfl_pff_pblk$pbe, na.rm=T)) / sd(nfl_pff_pblk$pbe, na.rm = T), digits = 2))
         
nfl_pff_chart_oline_dline_matchup <- read.csv(glue("./contests/2022_w{week}/pff/oline_dline_matchup_chart.csv"))
nfl_pff_chart_oline_dline_matchup <- nfl_pff_chart_oline_dline_matchup %>% 
  replace(., nfl_pff_chart_oline_dline_matchup == 'ARZ', 'ARI') %>% 
  replace(., nfl_pff_chart_oline_dline_matchup == 'BLT', 'BAL') %>% 
  replace(., nfl_pff_chart_oline_dline_matchup == 'CLV', 'CLE') %>% 
  replace(., nfl_pff_chart_oline_dline_matchup == 'HST', 'HOU') %>% 
  replace(., nfl_pff_chart_oline_dline_matchup == 'LA', 'LAR')


# 1.9 misc adjustments ----------------------------------------------------

nfl_salaries$week <- max(nfl_2022$week)+1

# 2.0 Create Positions ----------------------------------------------------

# 2.1 Defenses ------------------------------------------------------------

def <- nfl_pff_dk_own %>% 
  filter(position == "D") %>% 
  mutate(across('opponent', str_replace, 'JAC', 'JAX'), 
         across('opponent', str_replace, 'LA', 'LAR'), 
         across('opponent', str_replace, 'LARC', 'LAC'), 
         across('opponent', str_replace, 'LARR', 'LA')) %>% 
  left_join(nfl_2022_def %>% select(defteam, def_pass_epa_rank, def_rush_epa_rank), by=c("team"="defteam")) %>% 
  left_join(nfl_2022_off %>% select(posteam, off_pass_epa_rank, off_rush_epa_rank), by=c("opponent"="posteam")) %>% 
  mutate(rush_adv = off_rush_epa_rank-def_rush_epa_rank,
         pass_adv = off_pass_epa_rank-def_pass_epa_rank,
         delta = (off_pass_epa_rank-def_pass_epa_rank) + (off_rush_epa_rank-def_rush_epa_rank)) %>% 
  view(title = "NFL DST") %>% 
  write.csv(file=glue("./contests/2022_w{week}/pos/def.csv"))

# 2.2 te ------------------------------------------------------------------

nfl_te <- nfl_pff_chart_te_matchup %>% 
  left_join(nfl_salaries, by = c('offPlayer' = 'Name')) %>% 
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
         offGrade,
         defPlayer, 
         defGrade) %>% 
  arrange(-offYprr) %>% 
  drop_na() %>% 
  view(title = "NFL TEs")

# 2.3 wr ------------------------------------------------------------------

nfl_wr <- nfl_salaries %>%
  left_join(nfl_pff_wr, by = c('Name' = 'player')) %>% 
  left_join(nfl_pff_dk_own, by = c('Name' = 'player')) %>%
  filter(Position == "WR" & ownership > 0) %>% 
  left_join(nfl_pff_chart_wr_cb_matchup, by = c('Name' = 'offPlayer')) %>% 
  left_join(nfl_2022_def, by = c('opponent' = 'defteam')) %>% 
  left_join(defense_coverage_scheme, by = c('opponent' = 'team_name')) %>% 
  left_join(nfl_pff_receiving_scheme, by = c('Name' = 'player')) %>% 
  left_join(nfl_pff_projections, by = c('Name' = 'playerName')) %>% 
  left_join(nfl_pff_def_table, by = c('opponent' = 'team_name'))

nfl_wr <- nfl_wr %>% 
  mutate(name_salary = paste(Name, Salary), 
         name_salary_own = paste(Name, Salary, ownership), 
         yprr_sd = round((yprr - mean(yprr, na.rm=T)) / sd(yprr, na.rm = T), digits = 2), 
         advantage_sd = round((advantage - mean(advantage, na.rm=T)) / sd(advantage, na.rm = T), digits = 2), 
         man_grade_yprr_man_cov = round((man_grades_pass_route * man_yprr * man_percentage), digits = 1), 
         man_grade_yprr_man_cov_sd = round((man_grade_yprr_man_cov - mean(man_grade_yprr_man_cov, na.rm=T)) / sd(man_grade_yprr_man_cov, na.rm = T), digits = 2), 
         targets_per_game = round(targets / player_game_count.x, digits = 1),
         targets_per_game_sd = round((targets_per_game - mean(targets_per_game, na.rm=T)) / sd(targets_per_game, na.rm = T), digits = 2), )

nfl_wr$sum_sd <- 
  (0.20 * nfl_wr$yprr_sd) + 
  (0.20 * nfl_wr$targets_per_game_sd) +
  (0.20 * nfl_wr$advantage_sd) - 
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
         targets_per_game,
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
         touchdowns,
         yards_after_catch_per_reception, 
         name_salary_own) %>%
  arrange(-sum_sd) %>%
  view(title = "NFL WRs")

#Salary Table
nfl_wr_salary_table <- nfl_wr %>%
  group_by(TeamAbbrev) %>%
  summarise(wr_sum_salary = round(mean(salary), digits = 0))

nfl_wr_count <- table(nfl_wr$TeamAbbrev)

nfl_te_salary_table <- nfl_te %>% 
  select(TeamAbbrev, Salary)

names(nfl_te_salary_table)[2] <- 'te_sum_salary'

nfl_reciever_salary <- nfl_wr_salary_table %>% 
  left_join(nfl_te_salary_table, by = c('TeamAbbrev' = 'TeamAbbrev'))

nfl_reciever_salary$total_rec_salary <- nfl_reciever_salary$wr_sum_salary + nfl_reciever_salary$te_sum_salary

nfl_reciever_salary$total_rec_salary <- rowSums(nfl_reciever_salary[,c("wr_sum_salary", "te_sum_salary")], na.rm=TRUE)

nfl_reciever_salary$total_rec_salary_sd <- round((nfl_reciever_salary$total_rec_salary - mean(nfl_reciever_salary$total_rec_salary, na.rm=T)) / sd(nfl_reciever_salary$total_rec_salary, na.rm = T), digits = 2)

nfl_wr %>% 
  group_by(TeamAbbrev) %>% 
  summarize(total_sum_sd = sum(sum_sd, na.rm = T)) %>% 
  arrange(-total_sum_sd)

# 2.3.2 wr chart ----------------------------------------------------------

nfl_wr %>%
  ggplot(aes(x = sum_sd , y = fantasyPoints)) +
  geom_hline(yintercept = mean(nfl_wr$fantasyPoints, na.rm = T), color = "red", linetype = "dashed", alpha=0.5) +
  geom_vline(xintercept =  mean(nfl_wr$sum_sd, na.rm = T), color = "red", linetype = "dashed", alpha=0.5) +
  #geom_point(color = nfl_wr$team_color, cex = 5, alpha = .6) +
  geom_point(cex = 5, alpha = .6) +
  geom_smooth(method = "lm") +
  geom_text_repel(aes(label=name_salary_own)) +
  labs(x = "sum_sd",
       y = "fantasyPoints",
       title = paste("WRs, NFL Weeks 1-",max(nfl_2022$week)),
       caption = "Twitter: Its_MikeF | Data: PFF") +
  theme_dark() +
  theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

# 2.4 rbs -----------------------------------------------------------------

nfl_rb <- nfl_salaries %>%
  left_join(nfl_pff_rb, by = c('Name' = 'player')) %>% 
  left_join(nfl_pff_dk_own, by = c('Name' = 'player')) %>%
  filter(Position == "RB" & ownership > 0) %>% 
  left_join(nfl_2022_def, by = c('opponent' = 'defteam')) %>% 
  left_join(nfl_2022_off, by = c('team' = 'posteam')) %>%
  left_join(nfl_pff_chart_oline_dline_matchup, by = c('TeamAbbrev' = 'offTeam')) %>% 
  left_join(nfl_pff_projections, by = c('Name' = 'playerName')) %>% 
  left_join(nfl_pff_def_table, by = c('opponent' = 'team_name'))

nfl_rb <- nfl_rb %>% 
  mutate(name_salary = paste(Name, Salary), 
         name_salary_own = paste(Name, Salary, ownership), 
         touches_game = round(total_touches / player_game_count, digits = 1), 
         mtf_per_attempt = round(elu_rush_mtf / rushAtt, digits = 1), 
         runBlockAdv_sd = round((runBlockAdv - mean(runBlockAdv, na.rm=T)) / sd(runBlockAdv, na.rm = T), digits = 2), 
         yco_attempt_sd = round((yco_attempt - mean(yco_attempt, na.rm=T)) / sd(yco_attempt, na.rm = T), digits = 2), 
         touches_game_sd = round((touches_game - mean(touches_game, na.rm=T)) / sd(touches_game, na.rm = T), digits = 2), 
         off_def = (def_rush_epa_rank+rdef_rank)/2 - off_rush_epa_rank)

nfl_rb$sum_sd <- round(
    (0.05 * nfl_rb$runBlockAdv_sd) +
    (0.20 * nfl_rb$off_rush_epa_sd) -
    (0.20 * nfl_rb$def_rush_epa_sd) - 
    (0.20 * nfl_rb$rdef_sd) + 
    (0.05 * (nfl_rb$yco_attempt_sd - nfl_rb$tack_sd)) +
    (0.40 * nfl_rb$touches_game_sd), digits = 3)

nfl_rb %>% 
  select(Name,
         TeamAbbrev,
         Salary,
         ownership,
         fantasyPoints,
         sum_sd,
         touches_game,
         runBlockAdv,
         opponent,
         off_def,
         off_rush_epa_rank,
         def_rush_epa_rank,
         rdef_rank,
         mtf_per_attempt,
         breakaway_percent,
         elusive_rating,
         tack_rank,
         grades_offense,
         yco_attempt,
         yprr, 
         name_salary_own) %>%
  arrange(-sum_sd) %>%
  #gt() %>% 
  view(title = "NFL RBs")


# 2.4.2 rb chart ----------------------------------------------------------

nfl_rb %>%
  ggplot(aes(x = sum_sd , y = fantasyPoints)) +
  geom_hline(yintercept = mean(nfl_rb$fantasyPoints, na.rm = T), color = "red", linetype = "dashed", alpha=0.5) +
  geom_vline(xintercept =  mean(nfl_rb$sum_sd, na.rm = T), color = "red", linetype = "dashed", alpha=0.5) +
  geom_point(cex = 5, alpha = .6) +
  geom_smooth(method = "lm") +
  geom_text_repel(aes(label=name_salary_own)) +
  labs(x = "sum_sd",
       y = "fantasyPoints",
       title = paste("RBs, NFL Weeks 1-",max(nfl_2022$week)),
       caption = "Twitter: Its_MikeF | Data: PFF") +
  theme_dark() +
  theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))


# 2.5.1 qbs -----------------------------------------------------------------

#QB
nfl_qb <- nfl_salaries %>%
  left_join(nfl_pff_qb, by = c('Name' = 'player')) %>% 
  left_join(nfl_pff_dk_own, by = c('Name' = 'player')) %>%
  filter(Position == "QB" & ownership > 0) %>% 
  left_join(nfl_pff_pblk, by = c('TeamAbbrev' = 'team_name')) %>% 
  left_join(nfl_pff_passing_concept, by = c('Name' = 'player')) %>% 
  left_join(nfl_reciever_salary, by = c('TeamAbbrev' = 'TeamAbbrev'))

player <- nfl_pff_passing_pressure_blitz$player
blitz_dropbacks_percent <- nfl_pff_passing_pressure_blitz$blitz_dropbacks_percent / 100
blitz_grades_pass <- nfl_pff_passing_pressure_blitz$blitz_grades_pass
pressure_grades_pass <- nfl_pff_passing_pressure_blitz$pressure_grades_pass

nfl_qb_blitz <- tibble(player, blitz_dropbacks_percent, blitz_grades_pass, pressure_grades_pass)

nfl_qb <- nfl_qb %>% 
  left_join(nfl_qb_blitz, by = c('Name' = 'player')) %>% 
  left_join(nfl_2022_def, by = c('opponent' = 'defteam')) %>% 
  left_join(nfl_pff_def_table, by = c('opponent' = 'team_name')) %>% 
  left_join(team_blitz, by = c('opponent' = 'TeamAbbrev')) %>% 
  left_join(nfl_pff_projections, by = c('Name' = 'playerName')) 

nfl_qb <- nfl_qb %>% 
  mutate(points_per_dollar = round(fantasyPoints / (Salary/100), digits = 3), 
         name_salary = paste(Name, Salary), 
         name_salary_own = paste(Name, Salary, ownership), 
         btt_twp_ratio = round(btt_rate / twp_rate, digits = 1), 
         btt_twp_ratio_sd = round((btt_twp_ratio - mean(btt_twp_ratio, na.rm=T)) / sd(btt_twp_ratio, na.rm = T), digits = 2), 
         dropbacks_game = round(dropbacks.x / player_game_count, digits = 1), 
         pressure_vs_prsh = round(pressure_grades_pass/ prsh, digits =1), 
         pressure_vs_prsh_sd = round((pressure_vs_prsh - mean(pressure_vs_prsh, na.rm=T)) / sd(pressure_vs_prsh, na.rm = T), digits = 2), 
         ttt_run_p2s = round(avg_time_to_throw*grades_run/pressure_to_sack_rate, digits = 1))

#making an edit
#nfl_qb$expected_blitz_rate <- round(rowMeans(nfl_qb[ , c("blitz_dropbacks_percent","blitz_rate")], na.rm=TRUE), digits = 3)
#nfl_qb$blitz_grades_pass_sq_exp_blitz_rate <- round(nfl_qb$blitz_grades_pass^2 * nfl_qb$expected_blitz_rate, digits = -1)
#nfl_qb$blitz_grades_pass_sq_exp_blitz_rate_sd <- round((nfl_qb$blitz_grades_pass_sq_exp_blitz_rate - mean(nfl_qb$blitz_grades_pass_sq_exp_blitz_rate, na.rm=T)) / sd(nfl_qb$blitz_grades_pass_sq_exp_blitz_rate, na.rm = T), digits = 2)

nfl_qb$blitz_grades_pass_sq_blitz_rate <- round(nfl_qb$blitz_grades_pass^2 * nfl_qb$Bltz., digits = -1)
nfl_qb$blitz_grades_pass_sq_blitz_rate_sd <- round((nfl_qb$blitz_grades_pass_sq_blitz_rate - mean(nfl_qb$blitz_grades_pass_sq_blitz_rate, na.rm=T)) / sd(nfl_qb$blitz_grades_pass_sq_blitz_rate, na.rm = T), digits = 2)

nfl_qb$grades_pass_sd <- round((nfl_qb$grades_pass - weighted.mean(nfl_qb$grades_pass, nfl_qb$dropbacks.x, na.rm=T)) / sd(nfl_qb$grades_pass, na.rm = T), digits = 2)

test <- as.data.frame(names(nfl_qb))

nfl_qb$sum_sd <- round(
  (0.35 * nfl_qb$grades_pass_sd) +
  (0.20 * nfl_qb$def_pass_epa_sd) +
  (0.20 * (nfl_qb$total_rec_salary_sd - nfl_qb$cov_sd)) +
  (0.15 * nfl_qb$blitz_grades_pass_sq_blitz_rate_sd)+
  (0.10 * nfl_qb$pressure_vs_prsh_sd) +
  (0.05 * (nfl_qb$pbe_sd - nfl_qb$prsh_sd)), digits = 3)

nfl_qb %>%
  select(Name,
         TeamAbbrev,
         Salary,
         ownership,
         fantasyPoints,
         sum_sd,
         points_per_dollar,
         opponent, 
         def_pass_epa_rank,
         def_rank, 
         cov_rank,
         grades_pass,
         ttt_run_p2s,
         def_rush_epa_rank,
         grades_pass_sd,
         def_pass_epa_sd,
         dropbacks.x,
         pbe_rank,
         prsh_rank,
         prsh, 
         pressure_grades_pass, 
         pressure_vs_prsh, 
         btt_twp_ratio,
         dropbacks_game,
         blitz_dropbacks_percent,
         blitz_grades_pass,
         #blitz_rate,
         bltz_rank,
         #expected_blitz_rate,
         blitz_grades_pass_sq_blitz_rate,
         blitz_grades_pass_sq_blitz_rate_sd,
         #blitz_grades_pass_sq_exp_blitz_rate,
         #blitz_grades_pass_sq_exp_blitz_rate_sd,
         total_rec_salary,
         total_rec_salary_sd,
         cov_sd,
         name_salary_own) %>%
  arrange(-sum_sd) %>% 
  view(title = "NFL QBs")

# 2.5.2 qb charts ---------------------------------------------------------

nfl_qb <- nfl_qb %>% 
  rename(blitz_rate = Bltz.)

nfl_qb_chart <- nfl_qb %>% 
  filter(Salary > 4500)

nfl_qb_chart %>%
  ggplot(aes(x = sum_sd , y = fantasyPoints)) +
  geom_hline(yintercept = mean(nfl_qb_chart$fantasyPoints, na.rm = T), color = "red", linetype = "dashed", alpha=0.5) +
  geom_vline(xintercept =  mean(nfl_qb_chart$sum_sd, na.rm = T), color = "red", linetype = "dashed", alpha=0.5) +
  #geom_point(color = nfl_qb_chart$team_color, cex = 5, alpha = .6) +
  geom_point(cex = 5, alpha = .6) +
  geom_smooth(method = "lm") +
  geom_text_repel(aes(label=name_salary_own)) +
  labs(x = "sum_sd",
       y = "fantasyPoints",
       title = paste("QBs, NFL Weeks 1-",max(nfl_2022$week)),
       caption = "Twitter: Its_MikeF | Data: PFF") +
  theme_dark() +
  theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  ylim(12, 26)
