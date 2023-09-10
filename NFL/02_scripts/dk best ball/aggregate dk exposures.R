# read and aggregate dk exposures

# 1.0 load packages and rankings -------------------------------------------

# Load packages
library(tidyverse)

# load rankings
ud <- read.csv("./01_data/projections/season/2023/rankings_sep06.csv") %>% 
  mutate(name = paste(firstName, lastName),
         adp = as.numeric(adp)) %>% 
  select(name, adp, projectedPoints, positionRank, slotName, teamName) %>% 
  drop_na(adp)

# 2.0 load and process dk best ball lineup csvs into df -----------------------

# List all the files
files <- list.files("./01_data/exposures/dk_lineups/", full.names = TRUE)

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

# Apply the function to each file and bind them together
result_df <- bind_rows(lapply(files, process_file))

# combine all players into 1 column, remove positions, and join rankings
dk <- result_df %>%
  gather(key = "player_num", value = "player_value", player1:player20) %>% 
  mutate(player_value = str_replace_all(player_value, "QB", "")) %>% 
  mutate(player_value = str_replace_all(player_value, "RB", "")) %>%
  mutate(player_value = str_replace_all(player_value, "WR", "")) %>%
  mutate(player_value = str_replace_all(player_value, "TE", "")) %>%
  mutate(player_value = str_replace_all(player_value, "FLEX", "")) %>%
  mutate(player_value = str_replace_all(player_value, "BN", "")) %>% 
  mutate(player_value = trimws(player_value)) %>% 
  rename(name = player_value) %>% # rename column
  left_join(ud, by=c("name")) %>% # add rankings
  rename(pos = slotName) %>% # rename column for easy join
  mutate(draft_capital = round(100 * 0.98 ^ adp, digits = 1)) # add draft capital


# 3.0 eda -----------------------------------------------------------------

# team analysis
teams <- dk %>% 
  group_by(EntryId) %>% 
  summarise(draft_capital = sum(draft_capital, na.rm = T)) %>% 
  arrange(-draft_capital)

# look up entryid in dk dataframe to review teams

# group by
expo <- dk %>%
  group_by(name, pos, teamName) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  mutate(expo = round(n/199, digits = 3)) %>% 
  arrange(-expo) %>% 
  left_join(ud %>% select(name, adp), by=c("name"))

# save RData object
#save(expo, file = "./01_data/exposures/dk/dk_exposures.RData")