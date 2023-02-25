#mlbplotr starter guide
#https://camdenk.github.io/mlbplotR/articles/mlbplotR.html

library(mlbplotR)
library(ggplot2)
library(dplyr)
library(scales)
library(baseballr)

teams_colors_logos <- mlbplotR::load_mlb_teams() %>% 
  filter(!team_abbr %in% c("AL", "NL", "MLB")) %>% 
  mutate(
    a = rep(1:6, 5), 
    b = sort(rep(1:5, 6), decreasing=T), 
    alpha = ifelse(grepl("A", team_abbr),1,0.75),
    color = ifelse(grepl("E", team_abbr), "b/w", NA)
  )

ggplot(teams_colors_logos, aes(x=a, y=b)) +
  geom_mlb_logos(aes(team_abbr=team_abbr, color=color, alpha=alpha), width=0.075) +
  geom_label(aes(label=team_abbr, nudge_y=-0.35, alpha=0.5))+
  scale_color_identity()+
  scale_alpha_identity()+
  theme_void()

df <- baseballr::fg_pitcher_leaders(x=2022, y=2022, q = 100, pitcher_type = "sta")

filtered_df <- df %>% 
  filter(Team != " - - -") %>% 
  group_by(Team) %>% 
  slice_min(ERA, n=1) %>% 
  ungroup()

filtered_df %>% 
  ggplot(aes(x=ERA, y=FIP)) +
  geom_smooth(method = lm, se=F) +
  geom_hline(yintercept = mean(filtered_df$FIP), color="red") +
  geom_vline(xintercept = mean(filtered_df$ERA), color="red") +
  geom_mlb_logos(aes(team_abbr=Team),width=0.05,alpha=.7) +
  labs(title = "2022 ERA vs FIP", 
       subtitle = "Each Team's Top Starter by ERA | Min 100 IP", 
       caption = "Data: Fangraphs via baseballr") +
  theme_minimal()+
  theme(plot.title = element_text(face="bold")) +
  scale_x_reverse(breaks=scales::pretty_breaks(), 
                  labels=scales::number_format(accuracy=0.01, decimal.mark = "."), 
                  expand=c(.1,.1)) +
  scale_y_reverse(breaks=scales::pretty_breaks(), 
                  labels=scales::number_format(accuracy=0.01, decimal.mark='.'), 
                  expand=c(.1,.1))

filtered_df %>% 
  mutate(Team=clean_team_abbrs(Team)) %>% 
  ggplot(aes(x=Team, y=HR)) +
  geom_col(aes(color=Team, fill=Team),width = 0.5) +
  geom_mlb_logos(aes(team_abbr=Team),width=0.07, alpha=0.9) +
  scale_color_mlb(type="secondary") +
  scale_fill_mlb(alpha = 0.4) +
  labs(title = "2022 Home Runs Allowed for Top Pitchers", 
       subtitle = "HRs allowed by each teams top starter by ERA, | Min 100 IP", 
       caption = "Data: Fangraphs via baseballR") +
  theme_minimal() +
  theme(plot.title = element_text(face="bold"), 
        axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        panel.grid.major = element_blank()) +
  scale_x_discrete(expand = c(0.05,0.075))

ws_teams <- teams_colors_logos %>% 
  left_join(ws_winner, by=c("team_name"="team"))

ws_teams %>% 
  ggplot(aes(x=Team,y=implied_odds)) +
  geom_col(aes(aes(x=Team,fill=Team),width=0.5)) +
  geom_mlb_logos(aes(team_abbr=Team), width=0.07,alpha=0.7)

ws_teams %>% 
  mutate(Team=clean_team_abbrs(Team)) %>% 
  ggplot(aes(x=Team, y=HR)) +
  geom_col(aes(color=Team, fill=Team),width = 0.5) +
  geom_mlb_logos(aes(team_abbr=Team),width=0.07, alpha=0.9)
