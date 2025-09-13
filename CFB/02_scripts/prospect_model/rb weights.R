# Get rb weights
# Reference: https://cfbfastr.sportsdataverse.org/

# Load packages
suppressMessages({
  library(dplyr)
  library(cfbfastR)
})

# Load key
Sys.setenv("CFBD_API_KEY" = "mp+H+sNsWbwCX9xV4SYfNNmnIAhG6UjylytaResV2WGshJC03K0rm84z+FW55kQU")

# Load roster
team_roster <- cfbd_team_roster(2022)

# filter roster data
team_roster_filtered <- team_roster %>% 
  filter(position == "RB") %>% 
  mutate(player = paste(first_name, last_name)) %>% 
  relocate(player, .after = last_name)

mean(team_roster_filtered$weight, na.rm = T) 
