#check draft positions


# 0.0 load packages -------------------------------------------------------

suppressMessages({
  library(nflverse) #nflfastr nflseedr nflplotr
  library(tidyverse) #ggplot2 dplyr tibble tidyr purrr forecats 
  library(ggrepel) #automatically position non-overlapping text labels
  library(glue) #interpreted literal strings
  library(gt)
  library(gtExtras)
  library(paletteer)
})


# 1.0 define values -------------------------------------------------------


year = 2023
current_month = month(Sys.Date())
current_month = "jun"


pbp_players <- function() {
  pbp <- load_pbp(2022)
  qb <- pbp %>% select(passer_id, passer) %>% drop_na() %>% unique() %>% rename(id=passer_id, player=passer) 
  rb <- pbp %>% select(rusher_id, rusher) %>% drop_na() %>% unique() %>% rename(id=rusher_id, player=rusher) 
  wr <- pbp %>% select(receiver_id, receiver) %>% drop_na() %>% unique() %>% rename(id=receiver_id, player=receiver) 
  
  players <<- rbind(qb, rb, wr) %>% unique()
}
pbp_players()

# Filter the teams colors logos
teams_colors_logos <- teams_colors_logos %>% 
  filter(!team_abbr %in% c("LA","OAK","SD","STL")) %>% 
  select(team_name, team_abbr, team_logo_espn)

#define function to determine pick positions based on first pick
draft_positions <- function(i) {
  
  #define constants
  league_size <- 12
  rounds <- 18
  picks <<- vector()
  
  print(i)
  picks[1] <<- i
  
  for (j in 1:ceiling(rounds/2)) {
    #assign the picks to a vector
    picks[j*2] <<- i+j*(2*length(seq(i,league_size,by=1))-1)+(j-1)*(2*i-1)
    picks[j*2+1] <<-i+j*(2*length(seq(i,league_size,by=1))-1)+j*(2*i-1)
    
    #print the values
    print(i+j*(2*length(seq(i,league_size,by=1))-1)+(j-1)*(2*i-1))
    print(i+j*(2*length(seq(i,league_size,by=1))-1)+j*(2*i-1))
    
  }
  #filters out picks beyond the draft size
  picks <<- picks[picks %in% 1:(league_size*rounds)]
}


# 2.0 load exposures and projections--------------------------------------------


# load the exposures from UD
exposure <- read.csv(glue("./01_data/exposures/{year}_ud_nfl_expo.csv"))
exposure <- exposure %>% 
  mutate(Picked.At = as.Date(as.POSIXct(exposure$Picked.At, format="%Y-%m-%d %H:%M:%S", tz="UTC")), 
         name = paste(First.Name, Last.Name)) %>% 
  select(name, Team, Position, Picked.At, Pick.Number, Draft)

# Load the most current rankings by reading the last file in the directory
rankings_udd <- read.csv(paste0("./01_data/projections/season/2023/", 
                                  list.files(path = "./01_data/projections/season/2023/")
                                  [max(which(grepl(current_month,list.files(path = "./01_data/projections/season/2023/")
                                                   )
                                             )
                                       )
                                    ]
                                )
                         ) %>% 
  mutate(name = paste(firstName, lastName), 
         adp = as.numeric(adp)) %>% 
  select(name, adp, projectedPoints, positionRank, slotName, teamName)

# join the exposure and rankings
exposure_adp <- exposure %>% 
  left_join(rankings_udd, by=c("name")) %>% 
  mutate(value = Pick.Number-adp, 
         rel_value = round(value/adp, digits = 2)) %>% 
  left_join(teams_colors_logos, by=c("Team"="team_abbr"))



# 3.0 EDA begins --------------------------------------------------------------


# drafts by date
drafts_by_date <- exposure_adp %>% 
  group_by(Picked.At) %>% 
  summarize(total_picks = n(),
            total_value = sum(value, na.rm = T), 
            total_rel_value = sum(rel_value, na.rm = T)) %>% 
  mutate(value_per_pick = round(total_value/total_picks,digits = 2), 
         rel_value_per_pick = round(total_rel_value/total_picks,digits=2))

# best drafts
drafts <- exposure_adp %>% 
  group_by(Draft) %>% 
  summarize(total_picks = n(),
            total_value = sum(value), 
            total_rel_value = sum(rel_value), 
            Picked.At = last(Picked.At)) %>% 
  mutate(value_per_pick = round(total_value/total_picks, digits = 2),
         rel_value_per_pick = round(total_rel_value/total_picks, digits = 3)) %>% 
  arrange(-rel_value_per_pick)


# best draft  -------------------------------------------------------------


best <- exposure_adp %>% 
  filter(Draft=="d0621484-033c-4c92-805d-c934fa13ad51") %>% 
  select(name, Team, Pick.Number, adp, value, rel_value) %>% 
  arrange(Pick.Number)  %>% 
  separate(name, into = c("first_name", "last_name"), sep=" ") %>% 
  mutate(first = substr(first_name, 1, 1), 
         name = paste(first, last_name, sep = "."), 
         player = paste(first_name, last_name)) %>% 
  left_join(players, by=c('name'='player')) %>% 
  left_join(teams_colors_logos %>% select(team_abbr, team_logo_espn), by=c('Team'='team_abbr')) %>% 
  select(player, team_logo_espn, Pick.Number,adp, value, rel_value, id) %>% 
  distinct(player, .keep_all = T)

best %>% 
  gt() %>% 
  gt_img_rows(columns = team_logo_espn) %>% 
  gt_color_rows(rel_value, palette = c("red","green"))

# #top ten highest value picks-------------------------------------------------------------------------

exposure_adp %>% 
  select(name, Pick.Number, adp, value, rel_value, Picked.At, Draft) %>% 
  arrange(-rel_value) %>% 
  slice_head(n=10)


# Team analysis -----------------------------------------------------------


#group by team drafted
exposure_adp %>% 
  group_by(Team, team_logo_espn) %>% 
  summarise(count=n()) %>% 
  arrange(-count) %>% 
  ungroup() %>% 
  rename(team = team_logo_espn) %>% 
  slice_head(n=10) %>% 
  gt() %>% 
  gt_img_rows(columns = team) %>% 
  gt_theme_dark() 


#group by position
exposure_adp %>% 
  group_by(Position) %>% 
  summarise(count=n()) %>% 
  arrange(-count) %>% 
  ungroup() %>% 
  mutate(own = round(count/sum(count),digits = 2)) %>% 
  gt() 


#group by position
exposure_adp %>% 
  group_by(Draft, Team) %>% 
  summarise(count=n()) %>% 
  arrange(-count) %>% 
  ungroup() %>% 
  group_by(Team) %>% 
  summarise(count=n()) %>% 
  arrange(-count) %>% 
  mutate(own = round(count/dim(drafts)[1], digits = 2)) %>% 
  left_join(teams_colors_logos %>% select(team_abbr, team_logo_espn), 
            by = c("Team"="team_abbr")) %>% 
  relocate(team_logo_espn, .after = Team) %>% 
  rename(logo = team_logo_espn) %>% 
  gt() %>% 
  gt_img_rows(logo) %>% 
  gt_theme_dark()

