

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
