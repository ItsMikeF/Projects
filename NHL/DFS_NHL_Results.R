library(tidyverse)
library(ggrepel)

nhl_standings <- read.csv('contest-standings-117298741.csv')

###Standings###

nhl_standings <- nhl_standings %>%
  rename(Own = X.Drafted)

nhl_results <- nhl_standings %>%
  select(Player, 
         Roster.Position, 
         Own,
         FPTS)

nhl_results$Own <- round(as.numeric(sub("%","",nhl_results$Own)), digits = 1)
nhl_results$FPTS <- as.numeric(nhl_results$FPTS)
nhl_results$ppo <- round(nhl_results$FPTS / nhl_results$Own, digits = 1)

nhl_results <- nhl_results %>%
  drop_na()

###Lineups###

nhl_lineups <- nhl_standings %>%
  select(EntryId,
         EntryName,
         Points, 
         Lineup)

nhl_lineups$Points <- round(nhl_lineups$Points, digits = 1)

nhl_lineups <- separate(nhl_lineups, Lineup, into = c(letters,"aa","bb","cc","dd"), sep = " ")

nhl_lineups$player1 <- paste(nhl_lineups$b, nhl_lineups$c)
nhl_lineups$player2 <- paste(nhl_lineups$e, nhl_lineups$f)
nhl_lineups$player3 <- paste(nhl_lineups$h, nhl_lineups$i)
nhl_lineups$player4 <- paste(nhl_lineups$k, nhl_lineups$l)
nhl_lineups$player5 <- paste(nhl_lineups$n, nhl_lineups$o)
nhl_lineups$player6 <- paste(nhl_lineups$q, nhl_lineups$r)
nhl_lineups$player7 <- paste(nhl_lineups$t, nhl_lineups$u)
nhl_lineups$player8 <- paste(nhl_lineups$w, nhl_lineups$x)
nhl_lineups$player9 <- paste(nhl_lineups$z, nhl_lineups$aa)

winning_lineup_players <- c(nhl_lineups$player1[1], nhl_lineups$player2[1], nhl_lineups$player3[1], nhl_lineups$player4[1], nhl_lineups$player5[1], 
                           nhl_lineups$player6[1], nhl_lineups$player7[1], nhl_lineups$player8[1], nhl_lineups$player9[1])
winning_lineup_positions <- c(nhl_lineups$a[1], nhl_lineups$d[1], nhl_lineups$g[1], nhl_lineups$j[1], nhl_lineups$m[1], nhl_lineups$p[1],
                             nhl_lineups$s[1], nhl_lineups$v[1],nhl_lineups$y[1])

winning_lineup <- tibble(winning_lineup_players, winning_lineup_positions)

winning_lineup <- winning_lineup %>% 
  left_join(nhl_salaries, by = c('winning_lineup_players' = 'Name'))

winning_lineup <- winning_lineup %>% 
  left_join(nhl_results, by = c("winning_lineup_players" = "Player"))

winning_lineup <- winning_lineup %>% 
  select(winning_lineup_players, 
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

###Centers###

nhl_results_c <- nhl_results %>% 
  filter(Roster.Position == "C") %>% 
  view(title = "C Results")

###Center Metric###

center_metric <- lm(formula = FPTS ~ gsae, data = goalies)
center_gsae_ice_r <- round(summary(goalie_metric)$r.squared, digits = 4)

###Centers Plot###

ggplot(nhl_results_c, aes(x = Own, y = FPTS)) +
  geom_hline(yintercept = mean(nhl_results_c$FPTS), color = "red", linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = mean(nhl_results_c$Own), color = "red", linetype = "dashed", alpha = 0.5) +
  stat_smooth(geom = "line", alpha = 0.5, se = FALSE, method = 'lm') +
  geom_point() +
  geom_text_repel(aes(label = Player)) +
  labs(x = "Ownership %", 
       y = "Fantasy Points", 
       title = "2021 11 02 Centers, Contest ID 117298741", 
       caption = "Twitter: Its_MikeF| Data: Draft Kings") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
theme(plot.title = element_text(size = 32, hjust = 0.5, face = "bold") +
        scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
        scale_x_continuous(breaks = scales::pretty_breaks(n = 10)))

###Wingers###

nhl_results_w <- nhl_results %>% 
  filter(Roster.Position == "W") %>% 
  view(title = "W Results")

###Defenders###

nhl_results_d <- nhl_results %>% 
  filter(Roster.Position == "D") %>% 
  view(title = "D Results")

###Goalies Results###

goalies <- goalies %>% 
  left_join(nhl_results, by = c('Name' = 'Player'))

goalies <- goalies %>% 
  select(Name, 
         TeamAbbrev,
         Salary, 
         Own, 
         FPTS, 
         Opponent, 
         xOnGoal, 
         gsae_ice) %>% 
  drop_na() %>% 
  view()

###Goalie Plot###

ggplot(goalies, aes(x = gsae_ice, y = FPTS)) +
  geom_hline(yintercept = mean(goalies$FPTS), color = "red", linetype = "dashed", alpha = 0.5, na.rm = TRUE) +
  geom_vline(xintercept = mean(goalies$gsae_ice), color = "red", linetype = "dashed", alpha = 0.5) +
  stat_smooth(geom = "line", alpha = 0.5, se = FALSE, method = 'lm') +
  geom_point() +
  geom_text_repel(aes(label = Name)) +
  labs(x = "Goals Saved Above Expected per Ice time", 
       y = "Fantasy Points", 
       title = "2021 11 02 Goalies, Contest ID 117298741", 
       caption = "Twitter: Its_MikeF | Data: DraftKings") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
theme(plot.title = element_text(size = 32, hjust = 0.5, face = "bold") +
        scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
        scale_x_continuous(breaks = scales::pretty_breaks(n = 10)))

###Goalie Metric###

goalie_metric <- lm(formula = FPTS ~ gsae, data = goalies)
goalie_gsae_ice_r <- round(summary(goalie_metric)$r.squared, digits = 4)