load <- load_pbp(2022)

abc <- data.frame(names(load))

load %>%
  select(receiver_player_name, receiving_yards, weather)
