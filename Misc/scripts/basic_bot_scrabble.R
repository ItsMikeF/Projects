library(tidyverse)

games <- read_csv("./data/basic_bot_scrabble/games.csv")
scores <- read_csv("./data/basic_bot_scrabble/scores.csv")
turns <- read_csv("./data/basic_bot_scrabble/turns.csv")

top_players <- turns %>% 
  group_by(nickname) %>% 
  summarise(n=n()) %>% 
  top_n(2,n)
top_players

# second player is always BasicBot's top opponent (first player is BasicBot)
top_player_games <- turns %>% 
  filter(nickname == top_players$nickname[2]) %>% 
  select(game_id)

options(repr.plot.width=9, repr.plot.height=6)

mean_turn_scores <- turns %>%
  filter(game_id %in% top_player_games$game_id) %>%
  group_by(nickname, turn_number) %>%
  summarise(mean_score = mean(score))

ggplot() +
  geom_line(data = turns %>% filter(game_id %in% top_player_games$game_id), 
            aes(x = turn_number, 
                y = score, 
                colour = nickname, 
                group = interaction(game_id, nickname)), alpha = 0.5) +
  geom_line(data = mean_turn_scores, aes(x = turn_number, y = mean_score, colour = nickname), lwd = 1) +
  theme_minimal() +
  labs(title = "Scrabble game turn data for BasicBot and their most frequent opponent", y = NULL)