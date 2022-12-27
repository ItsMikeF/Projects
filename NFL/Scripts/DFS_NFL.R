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

week <- 16
folder <- glue("./contests/2022_w{week}")

# 1.1 load dk slate -------------------------------------------------------

nfl_salaries <- read.csv(glue("{folder}/DKSalaries.csv"))

nfl_salaries <- nfl_salaries %>% 
  replace(., nfl_salaries =='DJ Moore','D.J. Moore')

# 1.2 load dk ownership ---------------------------------------------------

nfl_pff_dk_own <- read.csv(glue("{folder}/pff/dk-ownership.csv"))

# 1.3 load projections ----------------------------------------------------

nfl_pff_projections <- read.csv(glue("{folder}/pff/projections.csv"))
rg <- read.csv(paste0(folder, "/", list.files(path = folder, pattern = "projections_draftkings_nfl")))

rg <- rg %>% 
  replace(., rg =='DJ Moore','D.J. Moore')

# 1.4 load qb data --------------------------------------------------------

nfl_pff_qb <- read.csv(glue("{folder}/pff/passing_summary.csv"))
nfl_pff_passing_concept <- read.csv(glue("{folder}/pff/passing_concept.csv"))
nfl_pff_passing_pressure_blitz <- read.csv(glue("{folder}/pff/passing_pressure.csv"))

# 1.5 load wr data --------------------------------------------------------

nfl_pff_wr <- read.csv(glue("{folder}/pff/receiving_summary.csv"))
nfl_pff_wr <- replace(nfl_pff_wr, nfl_pff_wr =='D.K. Metcalf','DK Metcalf')

nfl_pff_receiving_scheme <- read.csv(glue("{folder}/pff/receiving_scheme.csv"))
nfl_pff_receiving_scheme <- replace(nfl_pff_receiving_scheme, nfl_pff_receiving_scheme =='D.K. Metcalf','DK Metcalf')

#load the wr matchup table
nfl_pff_chart_wr_cb_matchup <- read.csv(glue("{folder}/pff/wr_cb_matchup_chart.csv")) %>% 
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

nfl_pff_rb <- read.csv(glue("{folder}/pff/rushing_summary.csv"))

# 1.7 load te data --------------------------------------------------------

nfl_pff_chart_te_matchup <- read.csv(glue("{folder}/pff/te_matchup_chart.csv"))

# 1.8 load ol data --------------------------------------------------------

nfl_pff_pblk <- read.csv(glue("{folder}/pff/line_pass_blocking_efficiency.csv"))
nfl_pff_pblk <- nfl_pff_pblk %>% 
  mutate(across('team_name', str_replace, 'ARZ', 'ARI'),
         across('team_name', str_replace, 'BLT', 'BAL'), 
         across('team_name', str_replace, 'CLV', 'CLE'), 
         across('team_name', str_replace, 'HST', 'HOU'),
         across('team_name', str_replace, 'LA', 'LAR'), 
         across('team_name', str_replace, 'LARC', 'LAC')) %>% 
  mutate(pbe_rank = round(rank(-nfl_pff_pblk$pbe), digits = 0), 
         pbe_sd = round((nfl_pff_pblk$pbe - mean(nfl_pff_pblk$pbe, na.rm=T)) / sd(nfl_pff_pblk$pbe, na.rm = T), digits = 2))
         
nfl_pff_chart_oline_dline_matchup <- read.csv(glue("{folder}/pff/oline_dline_matchup_chart.csv"))
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

def <- rg %>% 
  filter(pos == "DST") %>% 
  #mutate(across('opponent', str_replace, 'JAC', 'JAX'), 
   #      across('opponent', str_replace, 'LA', 'LAR'), 
    #     across('opponent', str_replace, 'LARC', 'LAC'), 
     #    across('opponent', str_replace, 'LARR', 'LA')) %>% 
  left_join(nfl_2022_def %>% select(defteam, def_pass_epa_rank, def_rush_epa_rank), by=c("team"="defteam")) %>% 
  left_join(nfl_2022_off %>% select(posteam, off_pass_epa_rank, off_rush_epa_rank), by=c("opp"="posteam")) %>% 
  mutate(rush_adv = off_rush_epa_rank-def_rush_epa_rank,
         pass_adv = off_pass_epa_rank-def_pass_epa_rank,
         delta = (off_pass_epa_rank-def_pass_epa_rank) + (off_rush_epa_rank-def_rush_epa_rank)) %>% 
  select(team, opp, name, fpts, ceil, floor, proj_own, salary, rg_value, def_pass_epa_rank, off_pass_epa_rank, pass_adv, def_rush_epa_rank, off_rush_epa_rank, rush_adv, delta) %>% 
  arrange(-salary) %>% 
  view(title = "DST") %>% 
  write.csv(file=glue("{folder}/pos/def.csv"))

# 2.2 te ------------------------------------------------------------------

nfl_te <- rg %>% filter(pos=="TE") %>% 
  left_join(nfl_pff_chart_te_matchup, by = c('name'='offPlayer')) %>% 
  left_join(nfl_pff_wr %>% filter(position=="TE"), by = c('name'='player')) %>%
  left_join(nfl_pff_receiving_scheme %>% select(player, man_yprr, man_routes, zone_yprr, zone_routes), by = c('name' = 'player')) %>% 
  
  left_join(nfl_2022_def, by = c('opp' = 'defteam')) %>% 
  left_join(nfl_pff_def_table, by = c('opp' = 'team_name')) %>% 
  left_join(defense_coverage_scheme, by = c('opp' = 'team_name')) %>% 
  
  left_join(nfl_pff_projections %>% select(playerName, fantasyPoints), by = c('name' = 'playerName')) %>% 
  left_join(nfl_pff_dk_own %>% select(player, ownership), by=c('name'='player')) %>% 
  mutate(man_zone_yprr_split = man_yprr - zone_yprr) %>% 
  arrange(-fpts) %>% 
  select(team,
         opp,
         name,
         salary, 
         fpts, 
         fantasyPoints,
         proj_own,
         ownership,
         offYprr,
         grades_offense,
         adv, 
         def_pass_epa_rank, 
         cov_rank,
         man_zone_yprr_split,
         man_rank, 
         def_man_grade_rank,
         man_yprr,
         man_routes, 
         zone_rank, 
         def_zone_grade_rank, 
         zone_yprr, 
         zone_routes) %>% 
  view(title = "TEs") %>%
  write.csv(file = glue("{folder}/pos/te.csv"))

# 2.3 wr ------------------------------------------------------------------

nfl_wr <- nfl_salaries %>%
  left_join(nfl_pff_wr, by = c('Name' = 'player')) %>% 
  left_join(rg, by=c("Name" = "name")) %>% 
  left_join(nfl_pff_dk_own, by = c('Name' = 'player')) %>%
  filter(pos == "WR" & proj_own >= 0) %>% 
  left_join(nfl_pff_chart_wr_cb_matchup, by = c('Name' = 'offPlayer')) %>% 
  left_join(nfl_2022_def, by = c('opp' = 'defteam')) %>% 
  left_join(defense_coverage_scheme, by = c('opp' = 'team_name')) %>% 
  left_join(nfl_pff_receiving_scheme, by = c('Name' = 'player')) %>% 
  left_join(nfl_pff_projections, by = c('Name' = 'playerName')) %>% 
  left_join(nfl_pff_def_table, by = c('opp' = 'team_name')) %>% 
  left_join(slot, by=c('opp'='team_name'))

nfl_wr <- nfl_wr %>% 
  mutate(name_salary = paste(Name, Salary), 
         name_salary_own = paste(Name, Salary, proj_own), 
         yprr_sd = round((yprr - mean(yprr, na.rm=T)) / sd(yprr, na.rm = T), digits = 2), 
         advantage_sd = round((advantage - mean(advantage, na.rm=T)) / sd(advantage, na.rm = T), digits = 2), 
         man_grade_yprr_man_cov = round((man_grades_pass_route * man_yprr * man_percentage), digits = 1), 
         man_grade_yprr_man_cov_sd = round((man_grade_yprr_man_cov - mean(man_grade_yprr_man_cov, na.rm=T)) / sd(man_grade_yprr_man_cov, na.rm = T), digits = 2), 
         targets_per_game = round(targets / player_game_count.x, digits = 1),
         targets_per_game_sd = round((targets_per_game - mean(targets_per_game, na.rm=T)) / sd(targets_per_game, na.rm = T), digits = 2), 
         man_zone_yprr_split = man_yprr - zone_yprr)

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
         proj_own,
         fpts,
         grades_offense, 
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
         yards_after_catch_per_reception, 
         name_salary_own) %>%
  arrange(-fpts) %>%
  view(title = "WRs") %>% 
  write.csv(file = glue("{folder}/pos/wr.csv"))

#Salary Table
nfl_wr_salary_table <- nfl_wr %>%
  group_by(TeamAbbrev) %>%
  summarise(wr_sum_salary = round(mean(Salary), digits = 0))

nfl_wr_count <- table(nfl_wr$TeamAbbrev)

nfl_te_salary_table <- nfl_te %>% 
  group_by(team) %>% 
  summarise(te_sum_salary = round(mean(salary), digits = 0))

names(nfl_te_salary_table)[2] <- 'te_sum_salary'

nfl_reciever_salary <- nfl_wr_salary_table %>% 
  left_join(nfl_te_salary_table, by = c('TeamAbbrev' = 'team'))

nfl_reciever_salary$total_rec_salary <- nfl_reciever_salary$wr_sum_salary + nfl_reciever_salary$te_sum_salary

nfl_reciever_salary$total_rec_salary <- rowSums(nfl_reciever_salary[,c("wr_sum_salary", "te_sum_salary")], na.rm=TRUE)

nfl_reciever_salary$total_rec_salary_sd <- round((nfl_reciever_salary$total_rec_salary - mean(nfl_reciever_salary$total_rec_salary, na.rm=T)) / sd(nfl_reciever_salary$total_rec_salary, na.rm = T), digits = 2)

nfl_wr %>% 
  group_by(TeamAbbrev) %>% 
  summarize(total_sum_sd = sum(sum_sd, na.rm = T)) %>% 
  arrange(-total_sum_sd)

# 2.4 rbs -----------------------------------------------------------------

nfl_rb <- nfl_salaries %>%
  left_join(nfl_pff_rb, by = c('Name' = 'player')) %>% 
  left_join(rg, by=c("Name" = "name")) %>% 
  left_join(nfl_pff_dk_own, by = c('Name' = 'player')) %>%
  filter(pos == "RB" & proj_own > 0) %>% 
  left_join(nfl_2022_def, by = c('opp' = 'defteam')) %>% 
  left_join(nfl_2022_off, by = c('team.x' = 'posteam')) %>%
  left_join(nfl_pff_chart_oline_dline_matchup, by = c('TeamAbbrev' = 'offTeam')) %>% 
  left_join(nfl_pff_projections, by = c('Name' = 'playerName')) %>% 
  left_join(nfl_pff_def_table, by = c('opp' = 'team_name'))

nfl_rb <- nfl_rb %>% 
  mutate(name_salary = paste(Name, Salary), 
         name_salary_own = paste(Name, Salary, proj_own), 
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
         proj_own,
         fpts,
         sum_sd,
         touches_game,
         runBlockAdv,
         opp,
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
  view(title = "RBs") %>% 
  write.csv(file = glue("{folder}/pos/rb.csv"))

# 2.5.1 qbs -----------------------------------------------------------------

#QB
nfl_qb <- nfl_salaries %>%
  left_join(nfl_pff_qb, by = c('Name' = 'player')) %>% 
  left_join(rg, by=c("Name" = "name")) %>% 
  left_join(nfl_pff_dk_own, by = c('Name' = 'player')) %>%
  filter(pos == "QB" & proj_own > 0) %>% 
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
  left_join(nfl_2022_def, by = c('opp' = 'defteam')) %>% 
  left_join(nfl_pff_def_table, by = c('opp' = 'team_name')) %>% 
  left_join(team_blitz, by = c('opp' = 'team_name')) %>% 
  left_join(nfl_pff_projections, by = c('Name' = 'playerName')) 

nfl_qb <- nfl_qb %>% 
  mutate(points_per_dollar = round(fantasyPoints / (Salary/100), digits = 3), 
         name_salary = paste(Name, Salary), 
         name_salary_own = paste(Name, Salary, proj_own), 
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

nfl_qb$blitz_grades_pass_sq_blitz_rate <- round(nfl_qb$blitz_grades_pass^2 * nfl_qb$blitz_team, digits = -1)
nfl_qb$blitz_grades_pass_sq_blitz_rate_sd <- round((nfl_qb$blitz_grades_pass_sq_blitz_rate - mean(nfl_qb$blitz_grades_pass_sq_blitz_rate, na.rm=T)) / sd(nfl_qb$blitz_grades_pass_sq_blitz_rate, na.rm = T), digits = 2)

nfl_qb$grades_pass_sd <- round((nfl_qb$grades_pass - weighted.mean(nfl_qb$grades_pass, nfl_qb$dropbacks.x, na.rm=T)) / sd(nfl_qb$grades_pass, na.rm = T), digits = 2)

test <- as.data.frame(names(nfl_qb))

nfl_qb$sum_sd <- round(
  (0.30 * nfl_qb$grades_pass_sd) +
  (0.30 * nfl_qb$def_pass_epa_sd) +
  (0.10 * (nfl_qb$total_rec_salary_sd - nfl_qb$cov_sd)) +
  (0.10 * nfl_qb$blitz_grades_pass_sq_blitz_rate_sd)+
  (0.00 * nfl_qb$pressure_vs_prsh_sd) +
  (0.00 * (nfl_qb$pbe_sd - nfl_qb$prsh_sd)), digits = 3)

nfl_qb %>%
  select(Name,
         TeamAbbrev,
         Salary,
         proj_own,
         fpts,
         grades_pass,
         dropbacks_game,
         sum_sd,
         opp, 
         def_pass_epa_rank,
         def_rank, 
         cov_rank,
         ttt_run_p2s,
         grades_run, 
         def_rush_epa_rank,
         def_pass_epa_sd,
         pbe_rank,
         prsh_rank,
         prsh, 
         pressure_grades_pass, 
         avg_time_to_throw,
         pressure_vs_prsh, 
         blitz_dropbacks_percent,
         blitz_grades_pass,
         blitz_rank,
         blitz_grades_pass_sq_blitz_rate,
         total_rec_salary,
         name_salary_own) %>%
  arrange(-sum_sd) %>% 
  view(title = "QBs") %>% 
  write.csv(file = glue("{folder}/pos/qb.csv"))
