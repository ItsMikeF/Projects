#load packages
library(nflfastR) #functions to efficiently access NFL pbp data
library(tidyverse) #metapackage
library(ggrepel) #automatically position non-overlapping text labels with ggplot2

# 1.0 defense epa table -------------------------------------------------------
year <- 2022
nfl_2022 <- load_pbp(year)

nfl_2022_def_pass <- nfl_2022 %>% 
  filter(pass == 1 &
           wp > .20 &
           wp < .80 &
           half_seconds_remaining > 120) %>% 
  group_by(defteam) %>% 
  summarize(def_pass_epa = round(mean(epa), digits = 3),
            n_plays = n()) %>% 
  arrange(def_pass_epa)

nfl_2022_def_pass$def_pass_epa_rank <- round(rank(nfl_2022_def_pass$def_pass_epa), digits = 0)

nfl_2022_def_pass$def_pass_epa_multiplier <- round(nfl_2022_def_pass$def_pass_epa / mean(nfl_2022_def_pass$def_pass_epa), digits = 2)
nfl_2022_def_pass$def_pass_epa_sd <- round((nfl_2022_def_pass$def_pass_epa - weighted.mean(nfl_2022_def_pass$def_pass_epa, nfl_2022_def_pass$n_plays, na.rm=T)) / sd(nfl_2022_def_pass$def_pass_epa, na.rm = T), digits = 2)

nfl_2022_def_rush <- nfl_2022 %>% 
  filter(rush == 1 &
           wp > .20 &
           wp < .80 &
           half_seconds_remaining > 120) %>% 
  group_by(defteam) %>% 
  summarize(def_rush_epa = round(mean(epa), digits = 3),
            n_plays = n()) %>% 
  arrange(def_rush_epa)

nfl_2022_def_rush$def_rush_epa_rank <- round(rank(nfl_2022_def_rush$def_rush_epa), digits = 0)

nfl_2022_def_rush$def_rush_epa_multiplier <- round(nfl_2022_def_rush$def_rush_epa / mean(nfl_2022_def_rush$def_rush_epa), digits = 2)
nfl_2022_def_rush$def_rush_epa_sd <- round((nfl_2022_def_rush$def_rush_epa - weighted.mean(nfl_2022_def_rush$def_rush_epa, nfl_2022_def_rush$n_plays, na.rm=T)) / sd(nfl_2022_def_rush$def_rush_epa, na.rm = T), digits = 2)

nfl_2022_def <- nfl_2022_def_pass %>% 
  left_join(nfl_2022_def_rush, by = c('defteam')) 

nfl_2022_def$total_plays <- nfl_2022_def$n_plays.x + nfl_2022_def$n_plays.y

nfl_2022_def <- replace(nfl_2022_def, nfl_2022_def == 'LA', 'LAR')

# 1.1 defense epa plot --------------------------------------------------------
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
       caption = "0.2 < wp < 0.8, half_secs > 120
       Twitter: Its_MikeF | Data: @nflfastR") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

nfl_2022_def <- replace(nfl_2022_def, nfl_2022_def == 'JAX', 'JAC')


# 2.0 offense epa table -------------------------------------------------------
nfl_2022_off_pass <- nfl_2022 %>% 
  filter(pass == 1 &
           wp > .20 &
           wp < .80 &
           half_seconds_remaining > 120) %>% 
  group_by(posteam) %>% 
  summarize(off_pass_epa = round(mean(epa), digits = 3),
            n_plays = n()) %>% 
  arrange(off_pass_epa)

nfl_2022_off_pass$off_pass_epa_rank <- round(rank(-nfl_2022_off_pass$off_pass_epa), digits = 0)

nfl_2022_off_pass$off_pass_epa_multiplier <- round(nfl_2022_off_pass$off_pass_epa / mean(nfl_2022_off_pass$off_pass_epa), digits = 2)
nfl_2022_off_pass$off_pass_epa_sd <- round((nfl_2022_off_pass$off_pass_epa - weighted.mean(nfl_2022_off_pass$off_pass_epa, nfl_2022_off_pass$n_plays, na.rm=T)) / sd(nfl_2022_off_pass$off_pass_epa, na.rm = T), digits = 2)

nfl_2022_off_rush <- nfl_2022 %>% 
  filter(rush == 1 &
           wp > .20 &
           wp < .80 &
           half_seconds_remaining > 120) %>% 
  group_by(posteam) %>% 
  summarize(off_rush_epa = round(mean(epa), digits = 3),
            n_plays = n()) %>% 
  arrange(off_rush_epa)

nfl_2022_off_rush$off_rush_epa_rank <- round(rank(-nfl_2022_off_rush$off_rush_epa), digits = 0)

nfl_2022_off_rush$off_rush_epa_multiplier <- round(nfl_2022_off_rush$off_rush_epa / mean(nfl_2022_off_rush$off_rush_epa), digits = 2)
nfl_2022_off_rush$off_rush_epa_sd <- round((nfl_2022_off_rush$off_rush_epa - weighted.mean(nfl_2022_off_rush$off_rush_epa, nfl_2022_off_rush$n_plays, na.rm=T)) / sd(nfl_2022_off_rush$off_rush_epa, na.rm = T), digits = 2)

nfl_2022_off <- nfl_2022_off_pass %>% 
  left_join(nfl_2022_off_rush, by = c('posteam')) 

nfl_2022_off$total_plays <- nfl_2022_off$n_plays.x + nfl_2022_off$n_plays.y

nfl_2022_off <- replace(nfl_2022_off, nfl_2022_off == 'LA', 'LAR')


# 2.1 offense epa plot --------------------------------------------------------
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

nfl_2022_off <- replace(nfl_2022_off, nfl_2022_off == 'JAX', 'JAC')


# 3.0 defense pff table -------------------------------------------------------
nfl_pff_def <- read.csv("./contests/2022_w2/pff/defense_summary.csv")

nfl_pff_def_table <- nfl_pff_def %>%
  group_by(team_name) %>%
  summarise(def = round(weighted.mean(grades_defense, snap_counts_defense), digits = 1),
            rdef = round(weighted.mean(grades_run_defense, snap_counts_run_defense), digits = 1),
            tack = round(weighted.mean(grades_tackle, snap_counts_defense, na.rm = TRUE), digits = 1),
            prsh = round(weighted.mean(grades_pass_rush_defense, snap_counts_pass_rush), digits = 1),
            cov = round(weighted.mean(grades_coverage_defense, snap_counts_coverage), digits =1))

nfl_pff_def_table$def_rank <- round(rank(-nfl_pff_def_table$def), digits = 0)
nfl_pff_def_table$rdef_rank <- round(rank(-nfl_pff_def_table$rdef), digits = 0)
nfl_pff_def_table$tack_rank <- round(rank(-nfl_pff_def_table$tack), digits = 0)
nfl_pff_def_table$prsh_rank <- round(rank(-nfl_pff_def_table$prsh), digits = 0)
nfl_pff_def_table$cov_rank <- round(rank(-nfl_pff_def_table$cov), digits = 0)

nfl_pff_def_table <- replace(nfl_pff_def_table, nfl_pff_def_table == 'ARZ', 'ARI')
nfl_pff_def_table <- replace(nfl_pff_def_table, nfl_pff_def_table == 'BLT', 'BAL')
nfl_pff_def_table <- replace(nfl_pff_def_table, nfl_pff_def_table == 'CLV', 'CLE')
nfl_pff_def_table <- replace(nfl_pff_def_table, nfl_pff_def_table == 'HST', 'HOU')
nfl_pff_def_table <- replace(nfl_pff_def_table, nfl_pff_def_table == 'JAX', 'JAC')
nfl_pff_def_table <- replace(nfl_pff_def_table, nfl_pff_def_table == 'LA', 'LAR')

nfl_pff_def_table$def_multiplier <- round(nfl_pff_def_table$def / mean(nfl_pff_def_table$def), digits = 2)
nfl_pff_def_table$rdef_multiplier <- round(nfl_pff_def_table$rdef / mean(nfl_pff_def_table$rdef), digits = 2)
nfl_pff_def_table$tack_multiplier <- round(nfl_pff_def_table$tack / mean(nfl_pff_def_table$tack), digits = 2)
nfl_pff_def_table$prsh_multiplier <- round(nfl_pff_def_table$prsh / mean(nfl_pff_def_table$prsh), digits = 2)
nfl_pff_def_table$cov_multiplier <- round(nfl_pff_def_table$cov / mean(nfl_pff_def_table$cov), digits = 2)

nfl_pff_def_table$def_sd <- round((nfl_pff_def_table$def - mean(nfl_pff_def_table$def)) / sd(nfl_pff_def_table$def), digits = 2)
nfl_pff_def_table$rdef_sd <- round((nfl_pff_def_table$rdef - mean(nfl_pff_def_table$rdef)) / sd(nfl_pff_def_table$rdef), digits = 2)
nfl_pff_def_table$tack_sd <- round((nfl_pff_def_table$tack - mean(nfl_pff_def_table$tack)) / sd(nfl_pff_def_table$tack), digits = 2)
nfl_pff_def_table$prsh_sd <- round((nfl_pff_def_table$prsh - mean(nfl_pff_def_table$prsh)) / sd(nfl_pff_def_table$prsh), digits = 2)
nfl_pff_def_table$cov_sd <- round((nfl_pff_def_table$cov - mean(nfl_pff_def_table$cov)) / sd(nfl_pff_def_table$cov), digits = 2)

# defense coverage scheme -------------------------------------------------
nfl_pff_defense_coverage_scheme <- read.csv("./contests/2022_w2/pff/defense_coverage_scheme.csv")

nfl_pff_defense_coverage_scheme <- replace(nfl_pff_defense_coverage_scheme, nfl_pff_defense_coverage_scheme == 'ARZ', 'ARI')
nfl_pff_defense_coverage_scheme <- replace(nfl_pff_defense_coverage_scheme, nfl_pff_defense_coverage_scheme == 'BLT', 'BAL')
nfl_pff_defense_coverage_scheme <- replace(nfl_pff_defense_coverage_scheme, nfl_pff_defense_coverage_scheme == 'CLV', 'CLE')
nfl_pff_defense_coverage_scheme <- replace(nfl_pff_defense_coverage_scheme, nfl_pff_defense_coverage_scheme == 'HST', 'HOU')
nfl_pff_defense_coverage_scheme <- replace(nfl_pff_defense_coverage_scheme, nfl_pff_defense_coverage_scheme == 'JAX', 'JAC')
nfl_pff_defense_coverage_scheme <- replace(nfl_pff_defense_coverage_scheme, nfl_pff_defense_coverage_scheme == 'LA', 'LAR')

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
            def_zone_grade = weighted.mean(zone_grades_coverage_defense, zone_snap_counts_coverage))

defense_coverage_scheme$man_percentage <- round(defense_coverage_scheme$man_snaps / (defense_coverage_scheme$man_snaps + defense_coverage_scheme$zone_snaps), digits = 3)
defense_coverage_scheme$man_rank <-  round(rank(-defense_coverage_scheme$man_percentage), digits = 0)
defense_coverage_scheme$def_man_grade <- round(defense_coverage_scheme$def_man_grade, digits = 1)
defense_coverage_scheme$def_man_grade_rank <- round(rank(-defense_coverage_scheme$def_man_grade), digits = 0)

defense_coverage_scheme$zone_percentage <- 1 - defense_coverage_scheme$man_percentage
defense_coverage_scheme$zone_rank <- round(rank(-defense_coverage_scheme$zone_percentage), digits = 0)
defense_coverage_scheme$def_zone_grade <- round(defense_coverage_scheme$def_zone_grade, digits = 1)
defense_coverage_scheme$def_zone_grade_rank <- round(rank(-defense_coverage_scheme$def_zone_grade), digits = 0)

defense_coverage_scheme$total_cov_snaps <- defense_coverage_scheme$man_snaps + defense_coverage_scheme$zone_snaps


# 3.1 defense blitz  ----------------------------------------------------------
#https://www.pro-football-reference.com/years/2022/opp.htm
sportsref_download <- read.csv("./contests/2022_w2/sportsref_download.csv")

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
