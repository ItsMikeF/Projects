# read and aggregate dk exposures for best ball

# Load packages
library(tidyverse)

# Define the processing function for each file
process_file <- function(file_path) {
  df <- read.csv(file_path) %>% 
    select(1:6) %>% 
    drop_na() %>% 
    mutate(pick = c(1:12)) %>% 
    filter(EntryName == "DeepSeaMike") %>% 
    separate(Lineup, 
             into = paste0("player", 0:20), 
             sep = "(?=QB|RB|WR|TE|FLEX|BN)",
             extra = "merge",
             fill = "right")
  
  return(df)
}

# List all the files
files <- list.files("./01_data/exposures/dk/", full.names = TRUE)

# Apply the function to each file and bind them together
result_df <- bind_rows(lapply(files, process_file))

combined_df <- result_df %>%
  gather(key = "player_num", value = "player_value", player1:player20)

dk <- combined_df %>% 
  mutate(player_value = str_replace_all(player_value, "QB", "")) %>% 
  mutate(player_value = str_replace_all(player_value, "RB", "")) %>%
  mutate(player_value = str_replace_all(player_value, "WR", "")) %>%
  mutate(player_value = str_replace_all(player_value, "TE", "")) %>%
  mutate(player_value = str_replace_all(player_value, "FLEX", "")) %>%
  mutate(player_value = str_replace_all(player_value, "BN", "")) %>% 
  mutate(player_value = trimws(player_value)) %>% 
  rename(name = player_value)

# load rankings
ud <- read.csv("./01_data/projections/season/2023/rankings_sep06.csv") %>% 
  mutate(name = paste(firstName, lastName),
         adp = as.numeric(adp)) %>% 
  select(name, adp, projectedPoints, positionRank, slotName, teamName) %>% 
  drop_na(adp)

# join ud to dk
dk <- dk %>% 
  left_join(ud, by=c("name")) %>% 
  rename(pos = slotName)

# group by
expo <- dk %>%
  group_by(name, slotName, teamName) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  mutate(expo = round(n/199, digits = 3)) %>% 
  arrange(-expo) %>% 
  left_join(ud %>% select(name, adp), by=c("name"))

save(expo, file = "./01_data/exposures/dk/dk_exposures.RData")

load("./01_data/exposures/dk/dk_exposures.RData")