# rbs

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

rushing_summary <- read.csv(glue("{folder}/pff/rushing_summary.csv"))

chart_oline_dline_matchup <- read.csv(glue("{folder}/pff/oline_dline_matchup_chart.csv"))
chart_oline_dline_matchup <- chart_oline_dline_matchup %>% 
  replace(., chart_oline_dline_matchup == 'ARZ', 'ARI') %>% 
  replace(., chart_oline_dline_matchup == 'BLT', 'BAL') %>% 
  replace(., chart_oline_dline_matchup == 'CLV', 'CLE') %>% 
  replace(., chart_oline_dline_matchup == 'HST', 'HOU') %>% 
  replace(., chart_oline_dline_matchup == 'LA', 'LAR')

rb <- salaries %>%
  filter(pos == "RB") %>%
  mutate(name = str_replace(name, "Brian Robinson Jr\\.", "Brian Robinson")) %>% 
  left_join(rushing_summary, by = c('name' = 'player')) %>% 
  #left_join(rg, by=c("name" = "name")) %>% 
  #filter(pos == "RB" & proj_own >= 0) %>% 
  left_join(pbp_def, by = c('opp' = 'defteam')) %>% 
  left_join(pbp_off, by = c('team' = 'posteam')) %>%
  left_join(chart_oline_dline_matchup, by = c('team' = 'offTeam')) %>% 
  left_join(def_table, by = c('opp' = 'team_name')) %>% 
  mutate(name_salary = paste(name, salary), 
         #name_salary_own = paste(name, salary, proj_own), 
         touches_game = round(total_touches / player_game_count, digits = 1), 
         #mtf_per_attempt = round(elu_rush_mtf / rushAtt, digits = 1), 
         runBlockAdv_sd = round((runBlockAdv - mean(runBlockAdv, na.rm=T)) / sd(runBlockAdv, na.rm = T), digits = 2), 
         yco_attempt_sd = round((yco_attempt - mean(yco_attempt, na.rm=T)) / sd(yco_attempt, na.rm = T), digits = 2), 
         touches_game_sd = round((touches_game - mean(touches_game, na.rm=T)) / sd(touches_game, na.rm = T), digits = 2), 
         off_def = (def_rush_epa_rank+rdef_rank)/2 - off_rush_epa_rank, 
         bco_delta = offYardsBco - defYardsBco, 
         attempts_game = round(attempts / player_game_count, digits = 1),
         gap_attempts_game = round(gap_attempts / player_game_count, digits = 1), 
         zone_attempts_game = round(zone_attempts / player_game_count, digits = 1), 
         yards_per_game = round(attempts_game * ypa, digits = 1), 
         first_downs_att = round(first_downs / attempts, digits = 1), 
         targets_game = round(targets / player_game_count, digits = 1))

rb$sum_sd <- round(
    (0.05 * rb$runBlockAdv_sd) +
    (0.20 * rb$off_rush_epa_sd) -
    (0.20 * rb$def_rush_epa_sd) - 
    (0.20 * rb$rdef_sd) + 
    (0.05 * (rb$yco_attempt_sd - rb$tack_sd)) +
    (0.40 * rb$touches_game_sd), 
    digits = 3)

rb %>% 
  #filter(proj_own != 0) %>% 
  select(name,
         team,
         salary,
         sum_sd,
         touches_game,
         targets_game,
         attempts_game,
         gap_attempts_game,
         zone_attempts_game,
         first_downs, 
         first_downs_att,
         ypa, 
         yards_per_game,
         offYardsBco,
         defYardsBco,
         bco_delta,
         runBlockAdv,
         opp,
         off_def,
         off_rush_epa_rank,
         def_rush_epa_rank,
         rdef_rank,
         breakaway_percent,
         elusive_rating,
         tack_rank,
         grades_offense,
         yco_attempt,
         yprr) %>%
  arrange(-sum_sd) %>%
  view(title = "RBs")
