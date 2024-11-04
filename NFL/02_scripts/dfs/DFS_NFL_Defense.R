#gather nfl defense data

#load packages
suppressMessages({
  library(nflfastR)
  library(nflseedR)
  library(nflreadr)
  library(tidyverse) #ggplot2 dplyr tibble tidyr purrr forecats 
  library(ggrepel) #automatically position non-overlapping text labels
  library(glue) #interpreted literal strings
  library(gt)
  library(gtExtras)
  library(lubridate)
})

# 0.0 Define Inputs -------------------------------------------------------

# define year
nfl_year <- year(Sys.Date())

# save pbp
pbp <- load_pbp(nfl_year)

# define game week
schedule <- load_schedules(nfl_year)

# set today as target date
target_date <- Sys.Date()
target_row <- which.min(abs((as.Date(schedule$gameday)-target_date)))

game_week = as.numeric(schedule$week[target_row])

depth_charts <- load_depth_charts(seasons = nfl_year) %>% 
  filter(week == game_week) %>%
  filter(position == "QB") %>% 
  filter(depth_team == 1)

# 1.0 defense epa table -------------------------------------------------------

epa_def <- function() {
  
  #def pass epa
  pbp_def_pass <- pbp %>% 
    filter(pass == 1 &
             week < game_week) %>% 
    group_by(defteam) %>% 
    summarize(def_pass_epa = round(mean(epa, na.rm = T), digits = 3),
              n_plays = n()) %>% 
    arrange(def_pass_epa) %>% 
    mutate(def_pass_epa_rank = round(rank(def_pass_epa), digits = 0), 
           def_pass_epa_sd = round((def_pass_epa - weighted.mean(def_pass_epa, n_plays, na.rm=T)) / sd(def_pass_epa, na.rm = T), digits = 2))
  
  #def rush epa
  pbp_def_rush <- pbp %>% 
    filter(rush == 1 &
             week < game_week) %>% 
    group_by(defteam) %>% 
    summarize(def_rush_epa = round(mean(epa, na.rm = T), digits = 3),
              n_plays = n()) %>% 
    arrange(def_rush_epa) %>% 
    mutate(def_rush_epa_rank = round(rank(def_rush_epa), digits = 0), 
           def_rush_epa_sd = round((def_rush_epa - weighted.mean(def_rush_epa, n_plays, na.rm=T)) / sd(def_rush_epa, na.rm = T), digits = 2))
  
  pbp_def <<- pbp_def_pass %>% 
    left_join(pbp_def_rush, by = c('defteam')) %>% 
    mutate(total_plays = n_plays.x + n_plays.y,
           defteam = gsub('LA','LAR', defteam), 
           defteam = gsub('LARC','LAC', defteam), 
           avg_rank = (def_rush_epa_rank + def_pass_epa_rank) /2)
  
}
epa_def()

epa_def_print <- function(print_plot) {
  #plot the data
  if (print_plot == 'yes') {
    pbp_def <- pbp_def %>% 
      left_join(teams_colors_logos, by = c('defteam' = 'team_abbr'))
    
    pbp_def %>%
      ggplot(aes(x = def_pass_epa, y = def_rush_epa)) +
      geom_hline(yintercept = mean(pbp_def$def_rush_epa), color = "red", linetype = "dashed", alpha=0.5) +
      geom_vline(xintercept =  mean(pbp_def$def_pass_epa), color = "red", linetype = "dashed", alpha=0.5) +
      geom_point(color = pbp_def$team_color, cex = pbp_def$total_plays / (0.1*max(pbp_def$total_plays)), alpha = .6) +
      #geom_image(aes(image = team_logo_espn), size = pbp_def$total_plays / (0.1*max(pbp_def$total_plays)), asp = 16 / 9) +
      geom_text_repel(aes(label=defteam)) +
      scale_y_discrete(limits = rev) +
      labs(x = "Pass EPA Allowed",
           y = "Rush EPA Allowed",
           title = paste("Def EPA Allowed, NFL Weeks 1-",(max(pbp$week))),
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

def_rankings <- pbp_def %>% 
  select(1,4,8) %>% 
  mutate(def_rank = (def_pass_epa_rank + def_rush_epa_rank)/2) %>% 
  arrange(def_rank) 

# 2.0 offense epa table -------------------------------------------------------

epa_off <- function(print_plot) {
  pbp_off_pass <- pbp %>% 
    filter(pass == 1 &
             week < game_week) %>% 
    group_by(posteam) %>% 
    summarize(off_pass_epa = round(mean(epa), digits = 3),
              n_plays = n()) %>% 
    arrange(off_pass_epa) %>% 
    mutate(off_pass_epa_rank = round(rank(-off_pass_epa), digits = 0), 
           off_pass_epa_sd = round((off_pass_epa - weighted.mean(off_pass_epa, n_plays, na.rm=T)) / sd(off_pass_epa, na.rm = T), digits = 2))
  
  pbp_off_rush <- pbp %>% 
    filter(rush == 1 &
             week < game_week) %>% 
    group_by(posteam) %>% 
    summarize(off_rush_epa = round(mean(epa), digits = 3),
              n_plays = n()) %>% 
    arrange(off_rush_epa) %>% 
    mutate(off_rush_epa_rank = round(rank(-off_rush_epa), digits = 0), 
           off_rush_epa_sd = round((off_rush_epa - weighted.mean(off_rush_epa, n_plays, na.rm=T)) / sd(off_rush_epa, na.rm = T), digits = 2))
  
  pbp_off <<- pbp_off_pass %>% 
    left_join(pbp_off_rush, by = c('posteam')) %>% 
    mutate(total_plays = n_plays.x + n_plays.y, 
           posteam = gsub('LA','LAR', posteam), 
           posteam = gsub('LARC','LAC', posteam),
           avg_rank = (off_rush_epa_rank + off_pass_epa_rank) /2)
  
}
epa_off()

off_rankings <- pbp_off %>% 
  select(1,4,8) %>% 
  mutate(off_rank = (off_pass_epa_rank + off_rush_epa_rank)/2) %>% 
  arrange(off_rank) 

# 2.1 Merge Offense and Defense rankings ----------------------------------

team_rankings <- function() {
  team_rankings <- def_rankings %>% 
    left_join(off_rankings, by=c("defteam"="posteam")) %>% 
    rename(team = defteam) %>% 
    left_join(teams_colors_logos %>% select(team_abbr, team_logo_espn), 
              by = c("team"="team_abbr")) %>% 
    mutate(team_rank = round((def_rank+off_rank)/2, digits =1)) %>% 
    arrange(team_rank) %>% 
    relocate(team_rank, .after = team)
  
  # create a GT table with the rankings
  tab1 <- team_rankings %>%
    slice(1:16) %>% 
    rename(logo = team_logo_espn) %>% 
    relocate(logo, .after = team) %>% 
    gt() %>% 
    gt_img_rows(columns = logo, height = 50) %>% 
    data_color(columns = def_rank, colors = scales::col_numeric(
      palette = c("green", "red"), 
      domain = c(1, 32)
    )) %>% 
    data_color(columns = off_rank, colors = scales::col_numeric(
      palette = c("green", "red"), 
      domain = c(1, 32)
    )) %>% 
    gt_theme_dark() %>% 
    tab_header(
      title = glue("2023 Week {week}: NFL Team Rankings")
    ) 
  
  # create a GT table with the rankings
  tab2 <- team_rankings %>%
    slice(17:32) %>% 
    rename(logo = team_logo_espn) %>% 
    relocate(logo, .after = team) %>% 
    gt() %>% 
    gt_img_rows(columns = logo, height = 50) %>% 
    data_color(columns = def_rank, colors = scales::col_numeric(
      palette = c("green", "red"), 
      domain = c(1, 32)
    )) %>% 
    data_color(columns = off_rank, colors = scales::col_numeric(
      palette = c("green", "red"), 
      domain = c(1, 32)
    )) %>% 
    gt_theme_dark() %>% 
    tab_header(
      title = glue("2023 Week {week}: NFL Team Rankings")
    ) 
  
  listed_tables <- list(tab1, tab2)
  
  #split the table into two tables side by side with 16 rows each
  gt_two_column_layout(listed_tables,
                       output = "viewer")
  
  gtsave(team_rankings_gt, 
         filename = glue("./03_plots/team_rankings/2023 Week {week}: NFL Team Rankings.html"))
}

# 3.0 defense pff table -------------------------------------------------------

def_table <- function(week) {
  def <- read.csv(glue("./01_data/contests/2023_w{sprintf(\"%02d\", week)}/pff/defense_summary.csv"))
  
  def_table <- def %>%
    group_by(team_name) %>%
    summarise(def = round(weighted.mean(grades_defense, snap_counts_defense), digits = 1),
              rdef = round(weighted.mean(grades_run_defense, snap_counts_run_defense), digits = 1),
              tack = round(weighted.mean(grades_tackle, snap_counts_defense, na.rm = TRUE), digits = 1),
              prsh = round(weighted.mean(grades_pass_rush_defense, snap_counts_pass_rush), digits = 1),
              cov = round(weighted.mean(grades_coverage_defense, snap_counts_coverage), digits =1))
  
  def_table <<- def_table %>% 
    mutate(def_rank = round(rank(-def), digits = 0), 
           def_sd = round((def - mean(def)) / sd(def, na.rm = T), digits = 2),
           
           rdef_rank = round(rank(-def_table$rdef), digits = 0), 
           rdef_sd = round((rdef - mean(rdef)) / sd(rdef, na.rm = T), digits = 2),
           
           tack_rank = round(rank(-def_table$tack), digits = 0), 
           tack_sd = round((tack - mean(tack)) / sd(tack, na.rm = T), digits = 2),
           
           prsh_rank = round(rank(-def_table$prsh), digits = 0), 
           prsh_sd = round((prsh - mean(prsh)) / sd(prsh, na.rm = T), digits = 2),
           
           cov_rank = round(rank(-def_table$cov), digits = 0),
           cov_sd = round((cov - mean(cov)) / sd(cov, na.rm = T), digits = 2),
           
           team_name = gsub('ARZ','ARI', team_name), 
           team_name = gsub('BLT','BAL', team_name), 
           team_name = gsub('CLV','CLE', team_name), 
           team_name = gsub('HST','HOU', team_name), 
           #team_name = gsub('JAX','JAC', team_name), 
           team_name = gsub('LA','LAR', team_name), 
           team_name = gsub('LARC','LAC', team_name))
}
def_table(game_week)

# 3.1 defense coverage scheme -------------------------------------------------

defense_coverage_scheme <- function(week) {
  defense_coverage_scheme <- read.csv(glue("./01_data/contests/2023_w{sprintf(\"%02d\", week)}/pff/defense_coverage_scheme.csv")) %>% 
    mutate(team_name = gsub('ARZ','ARI', team_name), 
           team_name = gsub('BLT','BAL', team_name), 
           team_name = gsub('CLV','CLE', team_name), 
           team_name = gsub('HST','HOU', team_name), 
           team_name = gsub('LA','LAR', team_name), 
           team_name = gsub('LARC','LAC', team_name), ) 
  
  defense_coverage_scheme <<- defense_coverage_scheme %>% 
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
}
defense_coverage_scheme(game_week)

# 3.2 Slot and Wide Coverage ----------------------------------------------

slot <- function(week){
  slot <-  read.csv(glue("./01_data/contests/2023_w{sprintf(\"%02d\", week)}/pff/defense_summary.csv")) %>% 
    group_by(team_name) %>% 
    summarise(slot = round(weighted.mean(grades_coverage_defense, snap_counts_slot, na.rm = T), digits = 2)) %>% 
    mutate(slot_rank = dense_rank(desc(slot)))
  
  wide <- read.csv(glue("./01_data/contests/2023_w{sprintf(\"%02d\", week)}/pff/defense_summary.csv")) %>% 
    group_by(team_name) %>% 
    summarise(wide = round(weighted.mean(grades_coverage_defense, snap_counts_corner, na.rm = T), digits = 2)) %>% 
    mutate(wide_rank = dense_rank(desc(wide)))
  
  slot <<- slot %>% left_join(wide, by=c('team_name'))
}
slot(game_week)

# 4.0 pff defense blitz  ----------------------------------------------------------

blitz <- function(week) {
  defense_summary <- read.csv(glue("./01_data/contests/2023_w{sprintf(\"%02d\", week)}/pff/defense_summary.csv"))
  pass_rush_summary <- read.csv(glue("./01_data/contests/2023_w{sprintf(\"%02d\", week)}/pff/pass_rush_summary.csv")) 
  run_defense_summary <- read.csv(glue("./01_data/contests/2023_w{sprintf(\"%02d\", week)}/pff/run_defense_summary.csv")) 
  defense_coverage_summary <- read.csv(glue("./01_data/contests/2023_w{sprintf(\"%02d\", week)}/pff/defense_coverage_summary.csv"))
  defense_coverage_scheme <- read.csv(glue("./01_data/contests/2023_w{sprintf(\"%02d\", week)}/pff/defense_coverage_scheme.csv"))
  slot_coverage <- read.csv(glue("./01_data/contests/2023_w{sprintf(\"%02d\", week)}/pff/slot_coverage.csv"))
  pass_rush_productivity <- read.csv(glue("./01_data/contests/2023_w{sprintf(\"%02d\", week)}/pff/pass_rush_productivity.csv"))
  
  def_list <- list()
  
  def_list[[1]] <- list(defense_summary, 
                        pass_rush_summary %>% select(2,6:dim(pass_rush_summary)[2]), 
                        run_defense_summary %>% select(2,6:dim(run_defense_summary)[2]), 
                        defense_coverage_summary %>% select(2,6:dim(defense_coverage_summary)[2]), 
                        defense_coverage_scheme %>% select(2,6:dim(defense_coverage_scheme)[2]), 
                        slot_coverage %>% select(2,6:dim(slot_coverage)[2]), 
                        pass_rush_productivity %>% select(2,6:dim(pass_rush_productivity)[2])) %>% 
    reduce(left_join, by = "player_id")
  
  def <- def_list[[1]] %>% filter(!position %in% c('G','WR','HB','FB','TE'))
  
  #blitz
  def_blitz_pos <- def %>% 
    group_by(team_name, position) %>% 
    summarise(snaps = sum(snap_counts_pass_rush.x)) %>% 
    ungroup()
  
  def_blitz <- def %>% 
    group_by(team_name) %>% 
    summarise(prsh_snaps = sum(snap_counts_pass_rush.x))
  
  def_blitz_pos <- def_blitz_pos %>% 
    left_join(def_blitz, by=c('team_name')) 
  
  def_blitz_pos <- def_blitz_pos %>% 
    mutate(blitz_pos = round(snaps/prsh_snaps, digits = 3)) %>% 
    filter(position == "LB" | position == "CB" | position == "S") 
  
  team_blitz <<- def_blitz_pos %>% 
    group_by(team_name) %>% 
    summarise(blitz_team = sum(blitz_pos)) %>% 
    mutate(blitz_rank = dense_rank(desc(blitz_team)), 
           team_name = gsub('ARZ','ARI', team_name), 
           team_name = gsub('BLT','BAL', team_name), 
           team_name = gsub('CLV','CLE', team_name), 
           team_name = gsub('HST','HOU', team_name), 
           #team_name = gsub('JAX','JAC', team_name), 
           team_name = gsub('LA','LAR', team_name), 
           team_name = gsub('LARC','LAC', team_name))
  
  def_blitz_pos <- def_blitz_pos %>% 
    left_join(team_blitz, by=c('team_name')) 
  
}
blitz(game_week)
