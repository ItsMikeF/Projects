# tight ends

week = 10
folder = glue("./01_data/contests/2023_w{sprintf(\"%02d\", week)}")

# load pff data
chart_te_matchup <- read.csv(glue("{folder}/pff/te_matchup_chart.csv"))
receiving_summary <- read.csv(glue("{folder}/pff/receiving_summary.csv"))
receiving_summary <- replace(receiving_summary, receiving_summary =='D.K. Metcalf','DK Metcalf') 
receiving_scheme <- read.csv(glue("{folder}/pff/receiving_scheme.csv"))

# create te dataframe
te <- salaries %>% filter(pos=="TE") %>% 
  left_join(chart_te_matchup, by = c('name'='offPlayer')) %>% 
  left_join(receiving_summary %>% filter(position=="TE"), by = c('name'='player')) %>%
  left_join(receiving_scheme %>% select(player, man_yprr, man_routes, zone_yprr, zone_routes), by = c('name' = 'player')) %>% 
  
  left_join(pbp_def, by = c('opp' = 'defteam')) %>% 
  left_join(def_table, by = c('opp' = 'team_name')) %>% 
  left_join(defense_coverage_scheme, by = c('opp' = 'team_name')) %>% 
  
  mutate(man_zone_yprr_split = man_yprr - zone_yprr, 
         test = man_zone_yprr_split * man_percentage) %>% 
  select(team,
         opp,
         name,
         salary, 
         ownership, 
         offYprr,
         grades_offense,
         adv, 
         def_pass_epa_rank, 
         cov_rank,
         man_zone_yprr_split,
         man_rank, 
         man_percentage, 
         test, 
         def_man_grade_rank,
         man_yprr,
         man_routes, 
         zone_rank, 
         def_zone_grade_rank, 
         zone_yprr, 
         zone_routes) %>% 
  view (title = "TEs")
