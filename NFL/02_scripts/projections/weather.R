# lets see how weather affects qb fpts

#load packages
suppressMessages({
  library(nflfastR) #nflfastr nflseedr nflplotr
  library(nflreadr) #
  library(tidyverse) #ggplot2 dplyr tibble tidyr purrr forecats 
  library(ggrepel) #automatically position non-overlapping text labels
  library(glue) #interpreted literal strings
})

# load the pbp
pbp <- load_pbp(2018:2022) 

# searchable df of names of pbp data columns
abc <- data.frame(names(load))

# load schedule data
sch <- load_schedules(2018:2022) %>% 
  select(game_id, gameday, away_team, away_score, home_team, home_score, 
         away_moneyline, home_moneyline, result, 
         spread_line, total_line, temp, wind)

# get qb pass fpts
pbp_pass <- pbp %>%
  group_by(game_date, passer, passer_id, posteam, defteam, week, temp) %>% 
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
         fpts_pass = 
           pass_touchdown * 4 +
           passing_yards * .04 +
           interception * -1 +
           rushing_yards * .1 +
           rush_touchdown * 6 +
           fumble_lost * -1 +
           big_py * 3, 
         temp = mean(temp, na.rm = T), 
         join = paste0(game_date, passer_id)) %>% 
  select(game_date, passer, passer_id, epa, fpts_pass, 
         defteam, week, posteam, temp, join) %>% 
  drop_na()

# get rushing stats
pbp_rush <- pbp %>% 
  group_by(game_date, rusher, rusher_id, posteam, defteam, week) %>% 
  summarise(
    rushing_yards = sum(rushing_yards, na.rm = T),
    rush_attempt = sum(rush_attempt, na.rm = T),
    rush_touchdown = sum(rush_touchdown, na.rm = T),
    fumble_lost = sum(fumble_lost, na.rm = T)) %>% 
  drop_na() %>% 
  ungroup() %>% 
  group_by(game_date, rusher, rusher_id, posteam, defteam, week) %>% 
  mutate(fpts_rush = 
           rushing_yards * .1 +
           rush_touchdown * 6 +
           fumble_lost * -1, 
         join = paste0(game_date, rusher_id)) %>% 
  ungroup() %>% 
  arrange(-rushing_yards)

# join rushing and passing fpts
qb_pbp <- pbp_pass %>% 
  left_join(pbp_rush %>% select(join, fpts_rush), by=c("join")) %>% 
  replace_na(., replace = list(fpts_rush = 0)) %>% 
  select(-join) %>% 
  mutate(fpts = fpts_pass + fpts_rush)

# group by to see average below 32 F
qb_pbp %>% 
  filter(temp < 32) %>% 
  group_by(passer) %>% 
  summarise(fpts_avg = mean(fpts), 
            n=n()) %>% 
  ungroup() %>% 
  filter(n > 2)

# select quarterback to check
quarterback = "A.Rodgers"

# plot the data
qb_pbp %>% 
  filter(passer == quarterback) %>% 
  ggplot(aes(x = temp , y = fpts)) +
  #geom_point(color = nfl_wr$team_color, cex = 5, alpha = .6) +
  geom_point(cex = 5, alpha = .6) +
  geom_smooth(method = "lm", formula = y ~ poly(x, degree = 3), 
              linetype = "dashed", 
              color = "red", 
              se= F) +
  #geom_text_repel(aes(label=name_salary_own)) +
  labs(x = "temp",
       y = "fpts",
       title = glue("2018:2022 QB Temperature vs Fpts: {quarterback}"),
       caption = "Twitter: Its_MikeF | Data: PFF") +
  annotate("text", x= 80, y =5, size = 6, 
           label = paste("Correlation:", round(cor(qb_pbp$temp, qb_pbp$fpts), digits = 2))) +
  theme_dark() +
  theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
