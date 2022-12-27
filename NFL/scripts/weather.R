#load packages
suppressMessages({
  library(nflfastR) #nflfastr nflseedr nflplotr
  library(nflreadr) #
  library(tidyverse) #ggplot2 dplyr tibble tidyr purrr forecats 
  library(ggrepel) #automatically position non-overlapping text labels
  library(glue) #interpreted literal strings
})


load <- load_pbp(2017:2022) 

abc <- data.frame(names(load))

sch <- load_schedules(2017:2022) %>% 
  select(game_id, gameday, away_team, away_score, home_team, home_score, away_moneyline, home_moneyline, result, spread_line, total_line, temp, wind)

test <- load %>%
  filter(pass == 1 & passer == "P.Mahomes") %>% 
  group_by(game_date, passer, posteam, defteam, week, temp) %>% 
  summarize(pass_attempt = sum(pass_attempt, na.rm = T),
            passing_yards = sum(passing_yards, na.rm = T),
            pass_touchdown = sum(pass_touchdown, na.rm = T),
            interception = sum(interception, na.rm = T),
            
            rushing_yards = sum(rushing_yards, na.rm = T),
            rush_attempt = sum(rush_attempt, na.rm = T),
            rush_touchdown = sum(rush_touchdown, na.rm = T),
            fumble_lost = sum(fumble_lost, na.rm = T),
            
            epa = round(mean(qb_epa), digits = 3),
            cpoe = round(mean(cpoe, na.rm = T), digits = 2)
  ) %>% 
  mutate(big_py = ifelse(passing_yards > 300, 1,0), 
         fpts = 
           pass_touchdown * 4 +
           passing_yards * .04 +
           interception * -1 +
           rushing_yards * .1 +
           rush_touchdown * 6 +
           fumble_lost * -1 +
           big_py * 3, 
         temp = mean(temp, na.rm = T)) %>% 
  select(passer, epa, fpts, defteam, week, posteam, temp)

test %>% 
  ggplot(aes(x = temp , y = fpts)) +
  geom_hline(yintercept = mean(test$fpts, na.rm = T), color = "red", linetype = "dashed", alpha=0.5) +
  geom_vline(xintercept =  mean(test$temp, na.rm = T), color = "red", linetype = "dashed", alpha=0.5) +
  #geom_point(color = nfl_wr$team_color, cex = 5, alpha = .6) +
  geom_point(cex = 5, alpha = .6) +
  geom_smooth(method = "lm") +
  #geom_text_repel(aes(label=name_salary_own)) +
  labs(x = "temp",
       y = "fpts",
       title = "P.Mahomes Fpts",
       caption = "Twitter: Its_MikeF | Data: PFF") +
  theme_dark() +
  theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
