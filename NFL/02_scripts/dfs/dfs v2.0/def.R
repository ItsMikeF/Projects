#lets analyze the nfl dfs slate

#load packages
suppressMessages({
  library(nflfastR) #nflfastr nflseedr nflplotr
  library(tidyverse) #ggplot2 dplyr tibble tidyr purrr forecats 
  library(ggrepel) #automatically position non-overlapping text labels
  library(glue) #interpreted literal strings
  library(gt)
})

# 1.0 inputs and slate data ------------------------------------------------

team_names = c('ARZ'='ARI', 'BLT'='BAL', 'CLV'='CLE', 'HST'='HOU', 'JAX'='JAC', 'LA'='LAR')
name_changes=c('DJ Moore'='D.J. Moore')

week = 10
sprintf("%02d", week)
folder = glue("./01_data/contests/2023_w{sprintf(\"%02d\", week)}")

# load saber
saber <- read.csv(glue("{folder}/NFL_2023-11-12_DK_Main.csv"))

# load pff own
pff_own <- read.csv(glue("{folder}/dk-ownership.csv"))

# load etr
etr <- read.csv(glue("{folder}/Weekly DraftKings Projections.csv"))

# load salary
dk_salaries <- function(){
  salaries <<- read.csv(glue("{folder}/DKSalaries.csv")) %>% 
    select(1,3,6:8) %>% 
    rename_with(~c("pos", "name", "salary", "game_info", "team")) %>% 
    separate(game_info, sep = "@", into = c("alpha", "bravo")) %>% 
    separate(bravo, sep = " ", into = c("charlie", "delta"), extra = "drop") %>% 
    mutate(opp = if_else(team == alpha, charlie, alpha)) %>% 
    select(pos, name, salary, team, opp) %>% 
    mutate(name = str_replace(name, "Gardner Minshew II","Gardner Minshew")) %>% 
    left_join(pff_own %>% select(player, ownership), by = c("name" = "player"))
}
dk_salaries()

# 2.0 Defenses ------------------------------------------------------------

def <- salaries %>% filter(pos=="DST") %>% 
  select(name, team, salary, opp) %>% 
  left_join(pbp_def %>% select(defteam, def_pass_epa_rank, def_rush_epa_rank), by=c("team"="defteam")) %>% 
  left_join(pbp_off %>% select(posteam, off_pass_epa_rank, off_rush_epa_rank), by=c("opp"="posteam")) %>% 
  mutate(rush_adv = off_rush_epa_rank-def_rush_epa_rank,
         pass_adv = off_pass_epa_rank-def_pass_epa_rank,
         delta = (off_pass_epa_rank-def_pass_epa_rank) + (off_rush_epa_rank-def_rush_epa_rank)) %>% 
  select(team, opp, name, salary, def_pass_epa_rank, off_pass_epa_rank, pass_adv, def_rush_epa_rank, off_rush_epa_rank, rush_adv, delta) %>% 
  arrange(-delta) %>% 
  view(title = "DST")
