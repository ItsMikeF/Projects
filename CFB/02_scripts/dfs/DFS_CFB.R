library(tidyverse)
library(ggrepel)

###Import Data###

cfb_salaries <- read.csv("DKSalaries.csv")

cfb_pff_qb <- read.csv("passing_summary.csv")
cfb_pff_wr <- read.csv("receiving_summary.csv")
cfb_pff_rb <- read.csv("rushing_summary.csv")
cfb_pff_def <- read.csv("defense_summary.csv")

cfb_schools <- read.csv("CFB_Schools.csv")

cfb_odds <- read.csv("cfb-odds-rotowire.csv")

cfb_odds <- cfb_odds %>% 
  left_join(cfb_schools, by = c('ï..' = 'Rotowire'))

###Add Opponent to Salaries###

cfb_salaries <- cfb_salaries %>%
  separate(Game.Info, c("Away", "String"), sep = "@") %>%
  separate(String, c("Home", "Date", "Time"), sep = " ")

cfb_salaries$Opponent <- if_else(cfb_salaries$Home == cfb_salaries$TeamAbbrev, cfb_salaries$Away, cfb_salaries$Home)

###Defense###

cfb_def <- cfb_pff_def %>%
  group_by(team_name) %>%
  summarise(def = round(weighted.mean(grades_defense, snap_counts_defense), digits = 1),
            rdef = round(weighted.mean(grades_run_defense, snap_counts_run_defense), digits = 1),
            tack = round(weighted.mean(grades_tackle, snap_counts_defense, na.rm = TRUE), digits = 1),
            prsh = round(weighted.mean(grades_pass_rush_defense, snap_counts_pass_rush), digits = 1),
            cov = round(weighted.mean(grades_coverage_defense, snap_counts_coverage), digits =1))

cfb_def$def_rank <- round(rank(-cfb_def$def), digits = 0)
cfb_def$rdef_rank <- round(rank(-cfb_def$rdef), digits = 0)
cfb_def$tack_rank <- round(rank(-cfb_def$tack), digits = 0)
cfb_def$prsh_rank <- round(rank(-cfb_def$prsh), digits = 0)
cfb_def$cov_rank <- round(rank(-cfb_def$cov), digits = 0)

cfb_def <- cfb_def %>%
  left_join(cfb_schools, by = c('team_name' = 'School'))

###QB###

cfb_qb <- cfb_salaries %>%
  left_join(cfb_pff_qb, by = c('Name' = 'player'))

cfb_qb <- cfb_qb %>%
  filter(Position == "QB" &
           Salary > 3000 &
           big_time_throws > 0 &
           AvgPointsPerGame > 0 &
           dropbacks > (.2 * max(cfb_qb$dropbacks, na.rm = TRUE)))

cfb_qb <- cfb_qb %>%
  left_join(cfb_def, by = c('Opponent' = 'Abbrev')) 

cfb_qb$name_salary <- paste(cfb_qb$Name, cfb_qb$Salary)

cfb_qb <- cfb_qb %>% 
  left_join(cfb_odds, by = c('TeamAbbrev' = 'Abbrev'))

cfb_qb$dropbacks_game <- round(cfb_qb$dropbacks / cfb_qb$player_game_count, digits = 0)

cfb_qb %>%
  select(Name,
         Salary,
         TeamAbbrev,
         dropbacks_game,
         Cover, 
         Total.Points, 
         Implied.Score,
         Opponent,
         def_rank,
         cov_rank,
         AvgPointsPerGame,
         grades_pass,
         yards,
         ypa,
         avg_depth_of_target,
         btt_rate,
         twp_rate,
         accuracy_percent,
         avg_time_to_throw,
         pressure_to_sack_rate,
         prsh_rank,
         grades_run,
         dropbacks, 
         player_game_count) %>%
  arrange(-grades_pass) %>%
  view(title = "CFB QBs")

###CFB QB Chart###

cfb_qb_chart <- cfb_qb %>% 
  filter(Salary > 5000)

cfb_qb_chart %>%
  ggplot(aes(x = btt_rate , y = twp_rate)) +
  geom_hline(yintercept = mean(cfb_qb_chart$twp_rate), color = "red", linetype = "dashed", alpha=0.5) +
  geom_vline(xintercept =  mean(cfb_qb_chart$btt_rate), color = "red", linetype = "dashed", alpha=0.5) +
  geom_point(color = rainbow(dim(cfb_qb_chart)[1]), cex = cfb_qb_chart$dropbacks_game / (0.1 * max(cfb_qb_chart$dropbacks_game)), alpha = .6) +
  geom_text_repel(aes(label=name_salary)) +
  labs(x = "Big Time Throw %",
       y = "Turnover Worthy Play %",
       title = "QBs, CFB Weeks 1-13",
       caption = "Salary > 5000
       Twitter: Its_MikeF | Data: PFF") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold")) +
  #scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_reverse(n.breaks = 10)

###RB###

cfb_rb <- cfb_salaries %>%
  left_join(cfb_pff_rb, by = c('Name' = 'player'))

cfb_rb <- cfb_rb %>%
  filter(Position == "RB" &
           Salary > 3000 &
           yprr > 0 &
           total_touches > (0.10 * max(cfb_rb$total_touches, na.rm = TRUE)) &
           elusive_rating > 50 &
           grades_offense > 50)

cfb_rb <- cfb_rb %>%
  left_join(cfb_def, by = c('Opponent' = 'Abbrev'))

cfb_rb <- cfb_rb %>% 
  left_join(cfb_odds, by= c('TeamAbbrev' = 'Abbrev'))

cfb_rb %>%
  select(Name,
         Salary,
         TeamAbbrev,
         Opponent,
         Cover, 
         Total.Points, 
         Implied.Score,
         rdef_rank,
         AvgPointsPerGame,
         elusive_rating,
         elu_rush_mtf,
         grades_offense,
         total_touches, 
         touchdowns,
         yco_attempt,
         ypa,
         rec_yards,
         yprr) %>%
  arrange(-grades_offense) %>%
  view(title = "CFB RBs")

###WR###

cfb_wr <- cfb_salaries %>%
  left_join(cfb_pff_wr, by = c('Name' = 'player'))

cfb_wr <- cfb_wr %>%
  filter(Position == "WR" &
           Salary > 3000 &
           targets > (0.2 * max(cfb_wr$targets, na.rm = TRUE)))

cfb_wr <- cfb_wr %>%
  left_join(cfb_def, by = c('Opponent' = 'Abbrev'))

cfb_wr <- cfb_wr %>% 
  left_join(cfb_odds, by = c('TeamAbbrev' = 'Abbrev'))

cfb_wr %>%
  select(Name,
         Salary,
         TeamAbbrev,
         Opponent,
         cov_rank,
         Cover, 
         Total.Points, 
         Implied.Score,
         AvgPointsPerGame,
         yprr,
         yards,
         avg_depth_of_target,
         targeted_qb_rating,
         targets,
         touchdowns,
         yards_after_catch_per_reception) %>%
  arrange(-yprr) %>%
  view(title = "CFB WRs")
