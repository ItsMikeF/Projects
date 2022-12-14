#Display all pos charts from nfl dfs scripts

#dfs_nfl was getting crowded and these code segments produced plots
#that are never looked at

# 1.0 wr chart ----------------------------------------------------------

nfl_wr %>%
  ggplot(aes(x = sum_sd , y = fantasyPoints)) +
  geom_hline(yintercept = mean(nfl_wr$fantasyPoints, na.rm = T), color = "red", linetype = "dashed", alpha=0.5) +
  geom_vline(xintercept =  mean(nfl_wr$sum_sd, na.rm = T), color = "red", linetype = "dashed", alpha=0.5) +
  #geom_point(color = nfl_wr$team_color, cex = 5, alpha = .6) +
  geom_point(cex = 5, alpha = .6) +
  geom_smooth(method = "lm") +
  geom_text_repel(aes(label=name_salary_own)) +
  labs(x = "sum_sd",
       y = "fantasyPoints",
       title = paste("WRs, NFL Weeks 1-",max(nfl_2022$week)),
       caption = "Twitter: Its_MikeF | Data: PFF") +
  theme_dark() +
  theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

# 2.0 rb chart ----------------------------------------------------------

nfl_rb %>%
  ggplot(aes(x = sum_sd , y = fantasyPoints)) +
  geom_hline(yintercept = mean(nfl_rb$fantasyPoints, na.rm = T), color = "red", linetype = "dashed", alpha=0.5) +
  geom_vline(xintercept =  mean(nfl_rb$sum_sd, na.rm = T), color = "red", linetype = "dashed", alpha=0.5) +
  geom_point(cex = 5, alpha = .6) +
  geom_smooth(method = "lm") +
  geom_text_repel(aes(label=name_salary_own)) +
  labs(x = "sum_sd",
       y = "fantasyPoints",
       title = paste("RBs, NFL Weeks 1-",max(nfl_2022$week)),
       caption = "Twitter: Its_MikeF | Data: PFF") +
  theme_dark() +
  theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

# 3.0 qb charts ---------------------------------------------------------

nfl_qb <- nfl_qb %>% 
  rename(blitz_rate = Bltz.)

nfl_qb_chart <- nfl_qb %>% 
  filter(Salary > 4500)

nfl_qb_chart %>%
  ggplot(aes(x = sum_sd , y = fpts)) +
  geom_hline(yintercept = mean(nfl_qb_chart$fpts, na.rm = T), color = "red", linetype = "dashed", alpha=0.5) +
  geom_vline(xintercept =  mean(nfl_qb_chart$sum_sd, na.rm = T), color = "red", linetype = "dashed", alpha=0.5) +
  #geom_point(color = nfl_qb_chart$team_color, cex = 5, alpha = .6) +
  geom_point(cex = 5, alpha = .6) +
  geom_smooth(method = "lm") +
  geom_text_repel(aes(label=name_salary_own)) +
  labs(x = "sum_sd",
       y = "fantasyPoints",
       title = paste("QBs, NFL Weeks 1-",max(nfl_2022$week)),
       caption = "Twitter: Its_MikeF | Data: PFF") +
  theme_dark() +
  theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  ylim(12, 26)