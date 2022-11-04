#gather nfl defense data

#load packages
suppressMessages({
  library(nflfastR) #nflfastr nflseedr nflplotr
  library(tidyverse) #ggplot2 dplyr tibble tidyr purrr forecats 
  library(ggrepel) #automatically position non-overlapping text labels
  library(glue) #interpreted literal strings
})

# 0.0 Define Inputs -------------------------------------------------------

week = 9

# 1.0 defense epa table -------------------------------------------------------

epa_def <- function(week, year, wp_lower, wp_upper, half_seconds_remaining, print_plot) {
  #define the values
  week = week
  year = year
  wp_lower = wp_lower
  wp_upper = wp_upper
  half_seconds_remaining = half_seconds_remaining
  
  assign(paste("nfl", year, sep = "_"), load_pbp(year), envir = .GlobalEnv)
  
  #def pass epa
  nfl_2022_def_pass <- nfl_2022 %>% 
    filter(pass == 1 &
             wp > 0.1 &
             wp < 0.9 &
             half_seconds_remaining > 120) %>% 
    group_by(defteam) %>% 
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
    group_by(defteam) %>% 
    summarize(def_rush_epa = round(mean(epa), digits = 3),
              n_plays = n()) %>% 
    arrange(def_rush_epa) %>% 
    mutate(def_rush_epa_rank = round(rank(def_rush_epa), digits = 0), 
           def_rush_epa_sd = round((def_rush_epa - weighted.mean(def_rush_epa, n_plays, na.rm=T)) / sd(def_rush_epa, na.rm = T), digits = 2))
  
  nfl_2022_def <<- nfl_2022_def_pass %>% 
    left_join(nfl_2022_def_rush, by = c('defteam')) %>% 
    mutate(total_plays = n_plays.x + n_plays.y,
           defteam = gsub('JAX','JAC', defteam), 
           defteam = gsub('LA','LAR', defteam), 
           defteam = gsub('LARC','LAC', defteam))
  
  #plot the data
  if (print_plot == 'yes') {
    nfl_2022_def <- nfl_2022_def %>% 
      left_join(teams_colors_logos, by = c('defteam' = 'team_abbr'))
    
    nfl_2022_def %>%
      ggplot(aes(x = def_pass_epa, y = def_rush_epa)) +
      geom_hline(yintercept = mean(nfl_2022_def$def_rush_epa), color = "red", linetype = "dashed", alpha=0.5) +
      geom_vline(xintercept =  mean(nfl_2022_def$def_pass_epa), color = "red", linetype = "dashed", alpha=0.5) +
      geom_point(color = nfl_2022_def$team_color, cex = nfl_2022_def$total_plays / (0.1*max(nfl_2022_def$total_plays)), alpha = .6) +
      #geom_image(aes(image = team_logo_espn), size = nfl_2022_def$total_plays / (0.1*max(nfl_2022_def$total_plays)), asp = 16 / 9) +
      geom_text_repel(aes(label=defteam)) +
      scale_y_discrete(limits = rev) +
      labs(x = "Pass EPA Allowed",
           y = "Rush EPA Allowed",
           title = paste("Def EPA Allowed, NFL Weeks 1-",(max(nfl_2022$week))),
           caption = glue("{wp_lower} < wp < {wp_upper}, half_secs > {half_seconds_remaining}
       Twitter: Its_MikeF | Data: @nflfastR")) +
      theme_bw() +
      theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold")) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
  } else {
    print("Nothing to see here.")
  }
  
}

epa_def(week, 2022, 0.1, 0.9, 120, 'no')

# 2.0 offense epa table -------------------------------------------------------

epa_off <- function(wp_lower, wp_upper, half_seconds_remaining, print_plot) {
  get("nfl_2022", envir = .GlobalEnv)
  nfl_2022_off_pass <- nfl_2022 %>% 
    filter(pass == 1 &
             wp > .10 &
             wp < .90 &
             half_seconds_remaining > 120) %>% 
    group_by(posteam) %>% 
    summarize(off_pass_epa = round(mean(epa), digits = 3),
              n_plays = n()) %>% 
    arrange(off_pass_epa) %>% 
    mutate(off_pass_epa_rank = round(rank(-off_pass_epa), digits = 0), 
           off_pass_epa_sd = round((off_pass_epa - weighted.mean(off_pass_epa, n_plays, na.rm=T)) / sd(off_pass_epa, na.rm = T), digits = 2))
  
  nfl_2022_off_rush <- nfl_2022 %>% 
    filter(rush == 1 &
             wp > .10 &
             wp < .90 &
             half_seconds_remaining > 120) %>% 
    group_by(posteam) %>% 
    summarize(off_rush_epa = round(mean(epa), digits = 3),
              n_plays = n()) %>% 
    arrange(off_rush_epa) %>% 
    mutate(off_rush_epa_rank = round(rank(-off_rush_epa), digits = 0), 
           off_rush_epa_sd = round((off_rush_epa - weighted.mean(off_rush_epa, n_plays, na.rm=T)) / sd(off_rush_epa, na.rm = T), digits = 2))
  
  nfl_2022_off <<- nfl_2022_off_pass %>% 
    left_join(nfl_2022_off_rush, by = c('posteam')) %>% 
    mutate(total_plays = n_plays.x + n_plays.y)
  
  if (print_plot == 'yes') {
    nfl_2022_off <- nfl_2022_off %>% 
      left_join(teams_colors_logos, by = c('posteam' = 'team_abbr'))
    
    nfl_2022_off %>%
      ggplot(aes(x = off_pass_epa, y = off_rush_epa)) +
      geom_hline(yintercept = mean(nfl_2022_off$off_rush_epa), color = "red", linetype = "dashed", alpha=0.5) +
      geom_vline(xintercept =  mean(nfl_2022_off$off_pass_epa), color = "red", linetype = "dashed", alpha=0.5) +
      geom_point(color = nfl_2022_off$team_color, cex = nfl_2022_off$total_plays / (0.1*max(nfl_2022_off$total_plays)), alpha = .6) +
      #geom_image(aes(image = team_logo_espn), size = nfl_2022_off$total_plays / (0.1*max(nfl_2022_off$total_plays)), asp = 16 / 9) +
      geom_text_repel(aes(label=posteam)) +
      scale_y_discrete(limits = rev) +
      labs(x = "Pass EPA",
           y = "Rush EPA",
           title = paste("OFF EPA, NFL Weeks 1-",(max(nfl_2022$week))),
           caption = "0.2 < wp < 0.8, half_secs > 120
       Twitter: Its_MikeF | Data: @nflfastR") +
      theme_bw() +
      theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold")) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
  } else {
    print("Move along.")
  }
  
}

epa_off(0.1, 0.9, 120, 'no')

# 3.0 defense pff table -------------------------------------------------------

nfl_pff_def <- read.csv(glue("./contests/2022_w{week}/pff/defense_summary.csv"))

nfl_pff_def_table <- nfl_pff_def %>%
  group_by(team_name) %>%
  summarise(def = round(weighted.mean(grades_defense, snap_counts_defense), digits = 1),
            rdef = round(weighted.mean(grades_run_defense, snap_counts_run_defense), digits = 1),
            tack = round(weighted.mean(grades_tackle, snap_counts_defense, na.rm = TRUE), digits = 1),
            prsh = round(weighted.mean(grades_pass_rush_defense, snap_counts_pass_rush), digits = 1),
            cov = round(weighted.mean(grades_coverage_defense, snap_counts_coverage), digits =1))

nfl_pff_def_table <- nfl_pff_def_table %>% 
  mutate(def_rank = round(rank(-def), digits = 0), 
         def_sd = round((def - mean(def)) / sd(def, na.rm = T), digits = 2),
         
         rdef_rank = round(rank(-nfl_pff_def_table$rdef), digits = 0), 
         rdef_sd = round((rdef - mean(rdef)) / sd(rdef, na.rm = T), digits = 2),
         
         tack_rank = round(rank(-nfl_pff_def_table$tack), digits = 0), 
         tack_sd = round((tack - mean(tack)) / sd(tack, na.rm = T), digits = 2),
         
         prsh_rank = round(rank(-nfl_pff_def_table$prsh), digits = 0), 
         prsh_sd = round((prsh - mean(prsh)) / sd(prsh, na.rm = T), digits = 2),
         
         cov_rank = round(rank(-nfl_pff_def_table$cov), digits = 0),
         cov_sd = round((cov - mean(cov)) / sd(cov, na.rm = T), digits = 2),
         
         
         
         team_name = gsub('ARZ','ARI', team_name), 
         team_name = gsub('BLT','BAL', team_name), 
         team_name = gsub('CLV','CLE', team_name), 
         team_name = gsub('HST','HOU', team_name), 
         team_name = gsub('JAX','JAC', team_name), 
         team_name = gsub('LA','LAR', team_name), 
         team_name = gsub('LARC','LAC', team_name))

# 3.1 pff defense coverage scheme -------------------------------------------------

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

# 4.0 pfr defense blitz  ----------------------------------------------------------
#https://www.pro-football-reference.com/years/2022/opp.htm
sportsref_download <- read.csv(glue("./contests/2022_w{week}/sportsref_download.csv"))

sportsref_download$Bltz. <- round(as.numeric(sub("%","",sportsref_download$Bltz.))/100, digits = 3)
sportsref_download$bltz_rank <- round(rank(-sportsref_download$Bltz.), digits = 0)

team_blitz <- sportsref_download %>% 
  select(Tm,
         Bltz.,
         bltz_rank)

nfl_team_table <- read.csv("nfl_team_table.csv")

team_blitz <- team_blitz %>% 
  left_join(nfl_team_table, by = c('Tm' = 'Tm')) %>%
  arrange(bltz_rank)

team_blitz <- replace(team_blitz, team_blitz == 'JAX', 'JAC')