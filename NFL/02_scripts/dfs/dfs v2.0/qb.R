
week = 9
folder = glue("./01_data/contests/2023_w{sprintf(\"%02d\", week)}")

# load salary
dk_salaries <- function(){
  salaries <<- read.csv(glue("{folder}/DKSalaries.csv")) %>% 
    select(1,3,6:8) %>% 
    rename_with(~c("pos", "name", "salary", "game_info", "team")) %>% 
    separate(game_info, sep = "@", into = c("alpha", "bravo")) %>% 
    separate(bravo, sep = " ", into = c("charlie", "delta"), extra = "drop") %>% 
    mutate(opp = if_else(team == alpha, charlie, alpha)) %>% 
    select(pos, name, salary, team, opp) %>% 
    mutate(name = str_replace(name, "Gardner Minshew II","Gardner Minshew")) %>% 
    left_join(pff_own %>% select(player, ownership), by = c("name" = "player"))
}
dk_salaries()

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


#QB
qb <- salaries %>% 
  filter(pos=="QB") %>%
  mutate(name = str_replace(name, "Gardner Minshew II","Gardner Minshew")) %>% 
  filter(salary > 5000) %>% 
  left_join(passing_summary, by = c('name' = 'player')) %>% 
  left_join(pblk, by = c('team' = 'team_name')) %>% 
  left_join(passing_concept, by = c('name' = 'player')) %>% 
  left_join(reciever_salary, by = c('team' = 'team'))

player <- passing_pressure_blitz$player
blitz_dropbacks_percent <- passing_pressure_blitz$blitz_dropbacks_percent / 100
blitz_grades_pass <- passing_pressure_blitz$blitz_grades_pass
pressure_grades_pass <- passing_pressure_blitz$pressure_grades_pass

qb_blitz <- tibble(player, blitz_dropbacks_percent, blitz_grades_pass, pressure_grades_pass)

qb <- qb %>% 
  left_join(qb_blitz, by = c('name' = 'player')) %>% 
  left_join(pbp_def, by = c('opp' = 'defteam')) %>% 
  left_join(def_table, by = c('opp' = 'team_name')) %>% 
  left_join(team_blitz, by = c('opp' = 'team_name')) 

qb <- qb %>% 
  mutate(#points_per_dollar = round(fantasyPoints / (salary/100), digits = 3), 
    name_salary = paste(name, salary), 
    #name_salary_own = paste(name, salary, proj_own), 
    btt_twp_ratio = round(btt_rate / twp_rate, digits = 1), 
    btt_twp_ratio_sd = round((btt_twp_ratio - mean(btt_twp_ratio, na.rm=T)) / sd(btt_twp_ratio, na.rm = T), digits = 2), 
    dropbacks_game = round(dropbacks.x / player_game_count, digits = 1), 
    pressure_vs_prsh = round(pressure_grades_pass/ prsh, digits =1), 
    pressure_vs_prsh_sd = round((pressure_vs_prsh - mean(pressure_vs_prsh, na.rm=T)) / sd(pressure_vs_prsh, na.rm = T), digits = 2), 
    ttt_run_p2s = round(avg_time_to_throw*grades_run/pressure_to_sack_rate, digits = 1))

qb$blitz_grades_pass_sq_blitz_rate <- round(qb$blitz_grades_pass^2 * qb$blitz_team, digits = -1)
qb$blitz_grades_pass_sq_blitz_rate_sd <- round((qb$blitz_grades_pass_sq_blitz_rate - mean(qb$blitz_grades_pass_sq_blitz_rate, na.rm=T)) / sd(qb$blitz_grades_pass_sq_blitz_rate, na.rm = T), digits = 2)

qb$grades_pass_sd <- round((qb$grades_pass - weighted.mean(qb$grades_pass, qb$dropbacks.x, na.rm=T)) / sd(qb$grades_pass, na.rm = T), digits = 2)

qb$sum_sd <- round(
    (0.30 * qb$grades_pass_sd) +
    (0.30 * qb$def_pass_epa_sd) +
    (0.10 * (qb$total_rec_salary_sd - qb$cov_sd)) +
    (0.10 * qb$blitz_grades_pass_sq_blitz_rate_sd)+
    (0.00 * qb$pressure_vs_prsh_sd) +
    (0.00 * (qb$pbe_sd - qb$prsh_sd)), 
  digits = 3)

qb %>%
  filter(is.na(ownership) == F) %>% 
  select(name,
         team,
         salary,
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
         blitz_grades_pass_sq_blitz_rate) %>%
  arrange(-sum_sd) %>% 
  view(title = "QBs")
