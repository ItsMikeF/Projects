library(tidyverse)
library(ggrepel)

cfb_standings <- read.csv('contest-standings-117293185.csv')

###Standings###

cfb_standings <- cfb_standings %>%
  rename(Own = X.Drafted)

cfb_results <- cfb_standings %>%
  select(Player, 
         Roster.Position, 
         Own,
         FPTS)

cfb_results$Own <- round(as.numeric(sub("%","",cfb_results$Own)), digits = 1)
cfb_results$FPTS <- as.numeric(cfb_results$FPTS)
cfb_results$ppo <- round(cfb_results$FPTS / cfb_results$Own, digits = 1)

cfb_results <- cfb_results %>%
  drop_na()

###Lineups###

cfb_lineups <- cfb_standings %>%
  select(EntryId,
         EntryName,
         Points, 
         Lineup)

cfb_lineups$Points <- round(cfb_lineups$Points, digits = 1)

cfb_lineups <- separate(cfb_lineups, Lineup, into = c(letters,"aa","bb","cc","dd"), sep = " ")

sum(if_else(cfb_lineups$a[1] == c("QB","RB","WR","FLEX","S-FLEX"),1,0))

cfb_lineups$player1 <- paste(cfb_lineups$b, cfb_lineups$c)
cfb_lineups$player2 <- paste(cfb_lineups$e, cfb_lineups$f)
cfb_lineups$player3 <- paste(cfb_lineups$h, cfb_lineups$i)
cfb_lineups$player4 <- paste(cfb_lineups$k, cfb_lineups$l)
cfb_lineups$player5 <- paste(cfb_lineups$n, cfb_lineups$o)
cfb_lineups$player6 <- paste(cfb_lineups$q, cfb_lineups$r)
cfb_lineups$player7 <- paste(cfb_lineups$t, cfb_lineups$u)
cfb_lineups$player8 <- paste(cfb_lineups$w, cfb_lineups$x)

winning_lineup_players <- c(cfb_lineups$player1[1], cfb_lineups$player2[1], cfb_lineups$player3[1], cfb_lineups$player4[1], cfb_lineups$player5[1], 
                            cfb_lineups$player6[1], cfb_lineups$player7[1], cfb_lineups$player8[1])
winning_lineup_positions <- c(cfb_lineups$a[1], cfb_lineups$d[1], cfb_lineups$g[1], cfb_lineups$j[1], cfb_lineups$m[1], cfb_lineups$p[1],
                              cfb_lineups$s[1], cfb_lineups$v[1])

winning_lineup <- tibble(winning_lineup_players, winning_lineup_positions)

winning_lineup <- winning_lineup %>% 
  left_join(cfb_salaries, by = c('winning_lineup_players' = 'Name'))

winning_lineup <- winning_lineup %>% 
  left_join(cfb_results, by = c("winning_lineup_players" = "Player"))

winning_lineup <- winning_lineup %>% 
  select(winning_lineup_players, 
         winning_lineup_positions,
         Position, 
         Salary, 
         TeamAbbrev, 
         Opponent,
         Own, 
         FPTS, 
         ppo)

winning_lineup <-  winning_lineup %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(where(is.character), ~""))) %>% 
  view(title = "Winning lu")

###QB Results Check###

cfb_qb_results <- cfb_qb %>%
  left_join(cfb_results, by = c('Name' = 'Player'))

cfb_qb_results <- cfb_qb_results %>%
  drop_na()

cfb_qb_results <- cfb_qb_results %>%
  filter(FPTS > (0.05 * max(cfb_qb_results$FPTS))) %>%
  select(Name,
         Salary,
         TeamAbbrev,
         Opponent,
         Own,
         FPTS,
         grades_pass,
         yards, 
         ypa) %>%
  arrange(-grades_pass) %>%
  view(title = "QBs Results")

###QB Metric###

qb_metric <- lm(formula = FPTS ~ grades_pass, data = cfb_qb_results)
qb_grade_r <- round(summary(qb_metric)$r.squared, digits = 4)

###QB Results Chart###

ggplot(cfb_qb_results, aes(x = Own, y = FPTS)) +
  geom_hline(yintercept = mean(cfb_qb$Own), color = "red", linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept =  mean(cfb_qb$FPTS), color = "red", linetype = "dashed", alpha = 0.5) +
  geom_point() +
  geom_text_repel(aes(label = Name)) +
  labs(x = "Ownership %",
       y = "Fantasy Points",
       title = "CFB QBs, 2021 Week 8",
       caption = "Twitter: Its_MikeF | Data: DK") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

###RB Results Check###

cfb_rb <- cfb_rb %>%
  left_join(cfb_results, by = c('Name' = 'Player'))

cfb_rb %>%
  select(Name,
         Salary,
         TeamAbbrev,
         Opponent,
         Own,
         FPTS,
         elusive_rating) %>%
  arrange(-elusive_rating) %>%
  view(title = "RBs Results")

###RB Metric###

rb_elusive_rating <- lm(formula = FPTS ~ elusive_rating, data = cfb_rb)
summary(rb_elusive_rating)$r.squared

rb_grades_offense <- lm(formula = FPTS ~ grades_offense, data = cfb_rb)
rb_metric_r <- round(summary(rb_grades_offense)$r.squared, digits = 4)

###RB Results Chart

ggplot(cfb_rb, aes(x = elusive_rating, y = FPTS)) +
  geom_hline(yintercept = mean(cfb_rb$FPTS), color = "red", linetype = "dashed", alpha=0.5) +
  geom_vline(xintercept = mean(cfb_rb$elusive_rating), color = "red", linetype = "dashed", alpha = 0.5) +
  stat_smooth(geom = "line", alpha = 0.5, se = FALSE, method = 'lm') +
  geom_text_repel(aes(label = Name)) +
  geom_point() +
  labs(x = "Elusive Rating",
       y = "Fantasy Points",
       title = "RB Fantasy Production vs ELU",
       caption = "Twitter: @Its_MikeF | Data: DK") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

###WR Results Check###

cfb_wr <- cfb_wr %>%
  left_join(cfb_results, by = c('Name' = 'Player'))

cfb_wr %>%
  select(Name,
         Salary,
         TeamAbbrev,
         Opponent,
         cov_rank,
         Own, 
         FPTS,
         yprr) %>%
  arrange(-yprr) %>%
  view(title = "WRs Results")

###WR Metric###

wr_yprr <- lm(formula =  FPTS ~ yprr, data = cfb_wr)
summary(wr_yprr)$r.squared

wr_grades_offense <- lm(formula = FPTS ~ grades_offense, data = cfb_wr)
wr_metric_r <- round(summary(wr_grades_offense)$r.squared, digits = 4)

###WR Results Chart###

ggplot(cfb_wr, aes(x = yprr, y =FPTS)) +
  geom_hline(yintercept = mean(cfb_wr$FPTS, na.rm = TRUE), color = "red", linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = mean(cfb_wr$yprr), color = "red", linetype = "dashed", alpha = 0.5) +
  stat_smooth(geom = "line", alpha = 0.5, se = FALSE, method = 'lm') +
  geom_text_repel(aes(label = Name)) +
  geom_point(alpha = 0.6) +
  labs(x = "Yards Per Route Run",
       y = "Fantasy Points",
       title = "WR Fantasy Points vs YPRR", 
       caption = "Twitter: @Its_MikeF | Data: DraftKings") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

###Summary Table###

Rsq_Table <-
  tibble(Position = c("QB", "RB", "WR"),
         PFF_Grade = c(qb_grade_r, rb_metric_r, wr_metric_r))
