#lets analyze the nfl dfs slate

#load packages
suppressMessages({
  library(nflfastR) #nflfastr nflseedr nflplotr
  library(tidyverse) #ggplot2 dplyr tibble tidyr purrr forecats 
  library(ggrepel) #automatically position non-overlapping text labels
  library(glue) #interpreted literal strings
  library(gt)
})

# 1.0 inputs and slate data ------------------------------------------------

team_names = c('ARZ'='ARI', 'BLT'='BAL', 'CLV'='CLE', 'HST'='HOU', 'JAX'='JAC', 'LA'='LAR')
name_changes=c('DJ Moore'='D.J. Moore')

week = 5
folder = glue("./01_data/contests/2023_w{week}")

salaries <- read.csv(glue("{folder}/DKSalaries.csv")) %>% 
  mutate(Name=replace(Name, Name=='DJ Moore','D.J. Moore'), 
         week = max(pbp$week)+1)

#rg <- read.csv(paste0(folder, "/", list.files(path = folder, pattern = "projections_draftkings_nfl"))) %>% mutate(name=replace(name, name=='DJ Moore','D.J. Moore'))

# 2.0 Defenses ------------------------------------------------------------

def <- salaries %>% 
  filter(Roster.Position == "DST") %>% 
  select(Name, Salary) %>% 
  left_join(pbp_def %>% select(defteam, def_pass_epa_rank, def_rush_epa_rank), by=c("team"="defteam")) %>% 
  left_join(pbp_off %>% select(posteam, off_pass_epa_rank, off_rush_epa_rank), by=c("opp"="posteam")) %>% 
  mutate(rush_adv = off_rush_epa_rank-def_rush_epa_rank,
         pass_adv = off_pass_epa_rank-def_pass_epa_rank,
         delta = (off_pass_epa_rank-def_pass_epa_rank) + (off_rush_epa_rank-def_rush_epa_rank)) %>% 
  select(team, opp, name, fpts, ceil, floor, proj_own, salary, rg_value, def_pass_epa_rank, off_pass_epa_rank, pass_adv, def_rush_epa_rank, off_rush_epa_rank, rush_adv, delta) %>% 
  arrange(-delta) %>% 
  view(title = "DST")

defense <- function(variables) {
  def <<- rg %>% 
    filter(pos == "DST") %>% 
    left_join(pbp_def %>% select(defteam, def_pass_epa_rank, def_rush_epa_rank), by=c("team"="defteam")) %>% 
    left_join(pbp_off %>% select(posteam, off_pass_epa_rank, off_rush_epa_rank), by=c("opp"="posteam")) %>% 
    mutate(rush_adv = off_rush_epa_rank-def_rush_epa_rank,
           pass_adv = off_pass_epa_rank-def_pass_epa_rank,
           delta = (off_pass_epa_rank-def_pass_epa_rank) + (off_rush_epa_rank-def_rush_epa_rank)) %>% 
    select(team, opp, name, fpts, ceil, floor, proj_own, salary, rg_value, def_pass_epa_rank, off_pass_epa_rank, pass_adv, def_rush_epa_rank, off_rush_epa_rank, rush_adv, delta) %>% 
    arrange(-delta) %>% 
    view(title = "DST")
}
defense()

# 3.0 te ------------------------------------------------------------------

chart_te_matchup <- read.csv(glue("{folder}/pff/te_matchup_chart.csv"))
receiving_summary <- read.csv(glue("{folder}/pff/receiving_summary.csv"))
receiving_summary <- replace(receiving_summary, receiving_summary =='D.K. Metcalf','DK Metcalf')

receiving_scheme <- read.csv(glue("{folder}/pff/receiving_scheme.csv"))
receiving_scheme <- replace(receiving_scheme, receiving_scheme =='D.K. Metcalf','DK Metcalf')


te <- rg %>% filter(pos=="TE") %>% 
  left_join(chart_te_matchup, by = c('name'='offPlayer')) %>% 
  left_join(receiving_summary %>% filter(position=="TE"), by = c('name'='player')) %>%
  left_join(receiving_scheme %>% select(player, man_yprr, man_routes, zone_yprr, zone_routes), by = c('name' = 'player')) %>% 
  
  left_join(pbp_def, by = c('opp' = 'defteam')) %>% 
  left_join(def_table, by = c('opp' = 'team_name')) %>% 
  left_join(defense_coverage_scheme, by = c('opp' = 'team_name')) %>% 
  
  mutate(man_zone_yprr_split = man_yprr - zone_yprr) %>% 
  arrange(-fpts) %>% 
  select(team,
         opp,
         name,
         salary, 
         fpts, 
         proj_own,
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
         zone_routes) %>% view (title = "TEs")

# 4.0 wr ------------------------------------------------------------------

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
  replace(., chart_wr_cb_matchup =='DJ Moore','D.J. Moore') %>% 
  mutate(advantage = round(chart_wr_cb_matchup$advantage, digits = 1), 
         expectedSnaps = round(chart_wr_cb_matchup$expectedSnaps, digits = 1))

chart_wr_cb_matchup <- replace(chart_wr_cb_matchup, chart_wr_cb_matchup =='D.K. Metcalf','DK Metcalf')

wr <- function(variables) {
  
}

wr <- salaries %>%
  left_join(receiving_summary %>% 
              replace(., receiving_summary =='DJ Moore','D.J. Moore'), 
            by = c('Name' = 'player')) %>% 
  #left_join(rg, by=c("Name" = "name")) %>% 
  #filter(pos == "WR" & proj_own >= 0) %>% 
  left_join(chart_wr_cb_matchup, by = c('Name' = 'offPlayer')) %>% 
  left_join(pbp_def, by = c('opp' = 'defteam')) %>% 
  left_join(defense_coverage_scheme, by = c('opp' = 'team_name')) %>% 
  left_join(receiving_scheme %>% 
              replace(., receiving_scheme =='DJ Moore','D.J. Moore'),
            by = c('Name' = 'player')) %>% 
  left_join(def_table, by = c('opp' = 'team_name')) %>% 
  left_join(slot, by=c('opp'='team_name'))

wr <- wr %>% 
  mutate(name_salary = paste(Name, Salary), 
         name_salary_own = paste(Name, Salary, proj_own), 
         yprr_sd = round((yprr - mean(yprr, na.rm=T)) / sd(yprr, na.rm = T), digits = 2), 
         advantage_sd = round((advantage - mean(advantage, na.rm=T)) / sd(advantage, na.rm = T), digits = 2), 
         man_grade_yprr_man_cov = round((man_grades_pass_route * man_yprr * man_percentage), digits = 1), 
         man_grade_yprr_man_cov_sd = round((man_grade_yprr_man_cov - mean(man_grade_yprr_man_cov, na.rm=T)) / sd(man_grade_yprr_man_cov, na.rm = T), digits = 2), 
         targets_per_game = round(targets / player_game_count.x, digits = 1),
         targets_per_game_sd = round((targets_per_game - mean(targets_per_game, na.rm=T)) / sd(targets_per_game, na.rm = T), digits = 2), 
         man_zone_yprr_split = man_yprr - zone_yprr, 
         slot_rate_cov = round((slot_rate/100)*slot +(1-slot_rate/100)*wide,digits=1), 
         adv = grades_pass_route-slot_rate_cov)

wr$sum_sd <- 
  (0.20 * wr$yprr_sd) + 
  (0.20 * wr$targets_per_game_sd) +
  (0.20 * wr$advantage_sd) - 
  (0.10 * wr$def_pass_epa_sd) -
  (0.10 * wr$cov_sd) + 
  (0.20 * wr$man_grade_yprr_man_cov_sd)

wr %>%
  filter(proj_own != 0) %>% 
  select(Name,
         TeamAbbrev,
         Salary,
         proj_own,
         fpts,
         grades_pass_route, 
         advantage,
         adv,
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
         slot_rate_cov,
         slot_rate, 
         slot,
         wide, 
         def_zone_grade_rank,
         def_pass_epa,
         touchdowns,
         yards_after_catch_per_reception, 
         name_salary_own) %>%
  arrange(-sum_sd) %>%
  view(title = "WRs")

#Salary Table
wr_salary_table <- wr %>%
  group_by(TeamAbbrev) %>%
  summarise(wr_sum_salary = round(mean(Salary), digits = 0))

wr_count <- table(wr$TeamAbbrev)

te_salary_table <- te %>% 
  group_by(team) %>% 
  summarise(te_sum_salary = round(mean(salary), digits = 0))

names(te_salary_table)[2] <- 'te_sum_salary'

reciever_salary <- wr_salary_table %>% 
  left_join(te_salary_table, by = c('TeamAbbrev' = 'team'))

reciever_salary$total_rec_salary <- reciever_salary$wr_sum_salary + reciever_salary$te_sum_salary

reciever_salary$total_rec_salary <- rowSums(reciever_salary[,c("wr_sum_salary", "te_sum_salary")], na.rm=TRUE)

reciever_salary$total_rec_salary_sd <- round((reciever_salary$total_rec_salary - mean(reciever_salary$total_rec_salary, na.rm=T)) / sd(reciever_salary$total_rec_salary, na.rm = T), digits = 2)

wr %>% 
  group_by(TeamAbbrev) %>% 
  summarize(total_sum_sd = sum(sum_sd, na.rm = T)) %>% 
  arrange(-total_sum_sd)

# 5.0 rbs -----------------------------------------------------------------

rushing_summary <- read.csv(glue("{folder}/pff/rushing_summary.csv"))

chart_oline_dline_matchup <- read.csv(glue("{folder}/pff/oline_dline_matchup_chart.csv"))
chart_oline_dline_matchup <- chart_oline_dline_matchup %>% 
  replace(., chart_oline_dline_matchup == 'ARZ', 'ARI') %>% 
  replace(., chart_oline_dline_matchup == 'BLT', 'BAL') %>% 
  replace(., chart_oline_dline_matchup == 'CLV', 'CLE') %>% 
  replace(., chart_oline_dline_matchup == 'HST', 'HOU') %>% 
  replace(., chart_oline_dline_matchup == 'LA', 'LAR')

rb <- function(variables) {
  
}

rb <- salaries %>%
  left_join(rushing_summary, by = c('Name' = 'player')) %>% 
  left_join(rg, by=c("Name" = "name")) %>% 
  filter(pos == "RB" & proj_own >= 0) %>% 
  left_join(pbp_def, by = c('opp' = 'defteam')) %>% 
  left_join(pbp_off, by = c('team' = 'posteam')) %>%
  left_join(chart_oline_dline_matchup, by = c('TeamAbbrev' = 'offTeam')) %>% 
  left_join(def_table, by = c('opp' = 'team_name'))

rb <- rb %>% 
  mutate(name_salary = paste(Name, Salary), 
         name_salary_own = paste(Name, Salary, proj_own), 
         touches_game = round(total_touches / player_game_count, digits = 1), 
         #mtf_per_attempt = round(elu_rush_mtf / rushAtt, digits = 1), 
         runBlockAdv_sd = round((runBlockAdv - mean(runBlockAdv, na.rm=T)) / sd(runBlockAdv, na.rm = T), digits = 2), 
         yco_attempt_sd = round((yco_attempt - mean(yco_attempt, na.rm=T)) / sd(yco_attempt, na.rm = T), digits = 2), 
         touches_game_sd = round((touches_game - mean(touches_game, na.rm=T)) / sd(touches_game, na.rm = T), digits = 2), 
         off_def = (def_rush_epa_rank+rdef_rank)/2 - off_rush_epa_rank)

rb$sum_sd <- round(
    (0.05 * rb$runBlockAdv_sd) +
    (0.20 * rb$off_rush_epa_sd) -
    (0.20 * rb$def_rush_epa_sd) - 
    (0.20 * rb$rdef_sd) + 
    (0.05 * (rb$yco_attempt_sd - rb$tack_sd)) +
    (0.40 * rb$touches_game_sd), digits = 3)

rb %>% 
  filter(proj_own != 0) %>% 
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
         #mtf_per_attempt,
         breakaway_percent,
         elusive_rating,
         tack_rank,
         grades_offense,
         yco_attempt,
         yprr, 
         name_salary_own) %>%
  arrange(-sum_sd) %>%
  view(title = "RBs")

# 6.0 qbs --------------------------------------------------------

pblk <- read.csv(glue("{folder}/pff/line_pass_blocking_efficiency.csv"))
pblk <- pblk %>% 
  mutate(across('team_name', str_replace, 'ARZ', 'ARI'),
         across('team_name', str_replace, 'BLT', 'BAL'), 
         across('team_name', str_replace, 'CLV', 'CLE'), 
         across('team_name', str_replace, 'HST', 'HOU'),
         across('team_name', str_replace, 'LA', 'LAR'), 
         across('team_name', str_replace, 'LARC', 'LAC')) %>% 
  mutate(pbe_rank = round(rank(-pblk$pbe), digits = 0), 
         pbe_sd = round((pblk$pbe - mean(pblk$pbe, na.rm=T)) / sd(pblk$pbe, na.rm = T), digits = 2))

passing_summary <- read.csv(glue("{folder}/pff/passing_summary.csv"))
passing_concept <- read.csv(glue("{folder}/pff/passing_concept.csv"))
passing_pressure_blitz <- read.csv(glue("{folder}/pff/passing_pressure.csv"))

qb_ids <- pbp %>% select(passer_id, passer) %>% drop_na() %>% unique()

qb <- function(variables) {
  
}

#QB
qb <- salaries %>%
  left_join(passing_summary, by = c('Name' = 'player')) %>% 
  left_join(rg, by=c("Name" = "name")) %>% 
  filter(pos == "QB" & proj_own >= 0) %>% 
  left_join(pblk, by = c('TeamAbbrev' = 'team_name')) %>% 
  left_join(passing_concept, by = c('Name' = 'player')) %>% 
  left_join(reciever_salary, by = c('TeamAbbrev' = 'TeamAbbrev'))

player <- passing_pressure_blitz$player
blitz_dropbacks_percent <- passing_pressure_blitz$blitz_dropbacks_percent / 100
blitz_grades_pass <- passing_pressure_blitz$blitz_grades_pass
pressure_grades_pass <- passing_pressure_blitz$pressure_grades_pass

qb_blitz <- tibble(player, blitz_dropbacks_percent, blitz_grades_pass, pressure_grades_pass)

qb <- qb %>% 
  left_join(qb_blitz, by = c('Name' = 'player')) %>% 
  left_join(pbp_def, by = c('opp' = 'defteam')) %>% 
  left_join(def_table, by = c('opp' = 'team_name')) %>% 
  left_join(team_blitz, by = c('opp' = 'team_name')) 

qb <- qb %>% 
  mutate(#points_per_dollar = round(fantasyPoints / (Salary/100), digits = 3), 
         name_salary = paste(Name, Salary), 
         name_salary_own = paste(Name, Salary, proj_own), 
         btt_twp_ratio = round(btt_rate / twp_rate, digits = 1), 
         btt_twp_ratio_sd = round((btt_twp_ratio - mean(btt_twp_ratio, na.rm=T)) / sd(btt_twp_ratio, na.rm = T), digits = 2), 
         dropbacks_game = round(dropbacks.x / player_game_count, digits = 1), 
         pressure_vs_prsh = round(pressure_grades_pass/ prsh, digits =1), 
         pressure_vs_prsh_sd = round((pressure_vs_prsh - mean(pressure_vs_prsh, na.rm=T)) / sd(pressure_vs_prsh, na.rm = T), digits = 2), 
         ttt_run_p2s = round(avg_time_to_throw*grades_run/pressure_to_sack_rate, digits = 1))

qb$blitz_grades_pass_sq_blitz_rate <- round(qb$blitz_grades_pass^2 * qb$blitz_team, digits = -1)
qb$blitz_grades_pass_sq_blitz_rate_sd <- round((qb$blitz_grades_pass_sq_blitz_rate - mean(qb$blitz_grades_pass_sq_blitz_rate, na.rm=T)) / sd(qb$blitz_grades_pass_sq_blitz_rate, na.rm = T), digits = 2)

qb$grades_pass_sd <- round((qb$grades_pass - weighted.mean(qb$grades_pass, qb$dropbacks.x, na.rm=T)) / sd(qb$grades_pass, na.rm = T), digits = 2)

test <- as.data.frame(names(qb))

qb$sum_sd <- round(
  (0.30 * qb$grades_pass_sd) +
  (0.30 * qb$def_pass_epa_sd) +
  (0.10 * (qb$total_rec_salary_sd - qb$cov_sd)) +
  (0.10 * qb$blitz_grades_pass_sq_blitz_rate_sd)+
  (0.00 * qb$pressure_vs_prsh_sd) +
  (0.00 * (qb$pbe_sd - qb$prsh_sd)), digits = 3)

qb %>%
  filter(proj_own != 0) %>% 
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
         pbe_rank,
         prsh_rank,
         prsh, 
         pressure_grades_pass, 
         avg_time_to_throw,
         avg_depth_of_target, 
         pressure_vs_prsh, 
         blitz_dropbacks_percent,
         blitz_grades_pass,
         blitz_rank,
         blitz_grades_pass_sq_blitz_rate,
         total_rec_salary,
         name_salary_own) %>%
  arrange(-sum_sd) %>% 
  view(title = "QBs")
