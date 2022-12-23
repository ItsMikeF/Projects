nfl_2022 <- load_pbp(2022)

list <- list()

  # 1.0 def pass epa --------------------------------------------------------
  
  nfl_2022_def_pass <- nfl_2022 %>% 
    filter(pass == 1 & wp > 0.1 & wp < 0.9 & half_seconds_remaining > 120) %>% 
    group_by(defteam, week) %>% 
    summarize(def_pass_epa = round(mean(epa), digits = 3),
              n_plays = n()) %>% 
    arrange(def_pass_epa) %>% 
    mutate(def_pass_epa_rank = round(rank(def_pass_epa), digits = 0), 
           def_pass_epa_sd = round((def_pass_epa - weighted.mean(def_pass_epa, n_plays, na.rm=T)) / sd(def_pass_epa, na.rm = T), digits = 2))
  
  #def rush epa
  nfl_2022_def_rush <- nfl_2022 %>% 
    filter(rush == 1 &
             wp > 0.1 &
             wp < 0.9 &
             half_seconds_remaining > 120) %>% 
    group_by(defteam, week) %>% 
    summarize(def_rush_epa = round(mean(epa), digits = 3),
              n_plays = n()) %>% 
    arrange(def_rush_epa) %>% 
    mutate(def_rush_epa_rank = round(rank(def_rush_epa), digits = 0), 
           def_rush_epa_sd = round((def_rush_epa - weighted.mean(def_rush_epa, n_plays, na.rm=T)) / sd(def_rush_epa, na.rm = T), digits = 2))
  
  nfl_2022_def <- nfl_2022_def_pass %>% 
    left_join(nfl_2022_def_rush, by = c('defteam')) %>% 
    mutate(total_plays = n_plays.x + n_plays.y,
           #defteam = gsub('JAX','JAC', defteam), 
           defteam = gsub('LA','LAR', defteam), 
           defteam = gsub('LARC','LAC', defteam))
  
  
  for (week in 1:14) {
  # 2.0 defense pff table -------------------------------------------------------
  
  nfl_pff_def <- read.csv(glue("./contests/2022_w{week}/pff/defense_summary.csv"))
  
  nfl_pff_def_table <- nfl_pff_def %>%
    group_by(team_name) %>%
    summarise(def = round(weighted.mean(grades_defense, snap_counts_defense), digits = 1),
              rdef = round(weighted.mean(grades_run_defense, snap_counts_run_defense), digits = 1),
              tack = round(weighted.mean(grades_tackle, snap_counts_defense, na.rm = TRUE), digits = 1),
              prsh = round(weighted.mean(grades_pass_rush_defense, snap_counts_pass_rush), digits = 1),
              cov = round(weighted.mean(grades_coverage_defense, snap_counts_coverage), digits =1))
  
  nfl_pff_def_table <- nfl_pff_def_table %>% 
    mutate(team_name = gsub('ARZ','ARI', team_name), 
           team_name = gsub('BLT','BAL', team_name), 
           team_name = gsub('CLV','CLE', team_name), 
           team_name = gsub('HST','HOU', team_name), 
           #team_name = gsub('JAX','JAC', team_name), 
           team_name = gsub('LA','LAR', team_name), 
           team_name = gsub('LARC','LAC', team_name))
  
  nfl_2022_def <- nfl_2022_def %>% left_join(nfl_pff_def_table, by=c("defteam"="team_name"))
  
  # 3.0 pff defense coverage scheme -------------------------------------------------
  
  nfl_pff_defense_coverage_scheme <- read.csv(glue("./contests/2022_w{week}/pff/defense_coverage_scheme.csv")) %>% 
    mutate(team_name = gsub('ARZ','ARI', team_name), 
           team_name = gsub('BLT','BAL', team_name), 
           team_name = gsub('CLV','CLE', team_name), 
           team_name = gsub('HST','HOU', team_name), 
           team_name = gsub('JAX','JAC', team_name), 
           team_name = gsub('LA','LAR', team_name)) 
  
  defense_coverage_scheme <- nfl_pff_defense_coverage_scheme %>% 
    select(player,
           team_name,
           man_snap_counts_coverage, 
           man_snap_counts_coverage_percent,
           man_grades_coverage_defense,
           zone_snap_counts_coverage,
           zone_snap_counts_coverage_percent,
           zone_grades_coverage_defense) %>% 
    group_by(team_name) %>% 
    summarize(man_snaps = sum(man_snap_counts_coverage), 
              zone_snaps = sum(zone_snap_counts_coverage),
              def_man_grade = weighted.mean(man_grades_coverage_defense, man_snap_counts_coverage), 
              def_zone_grade = weighted.mean(zone_grades_coverage_defense, zone_snap_counts_coverage)) %>% 
    mutate(man_percentage = round(man_snaps / (man_snaps + zone_snaps), digits = 3), 
           man_rank =  round(rank(-man_percentage), digits = 0), 
           def_man_grade = round(def_man_grade, digits = 1), 
           def_man_grade_rank = round(rank(-def_man_grade), digits = 0), 
           zone_percentage = 1 - man_percentage, 
           zone_rank = round(rank(-zone_percentage), digits = 0), 
           def_zone_grade = round(def_zone_grade, digits = 1), 
           def_zone_grade_rank = round(rank(-def_zone_grade), digits = 0))
  
  list[[week]] <- nfl_2022_def %>% 
    left_join(defense_coverage_scheme, by=c("defteam"="team_name")) %>% 
    mutate(week = week)
}

def <- bind_rows(list)
