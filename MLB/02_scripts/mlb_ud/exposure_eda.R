#check draft positions
#review best drafts


# 0.0 load packages -------------------------------------------------------

suppressMessages({
  library(tidyverse) #ggplot2 dplyr tibble tidyr purrr forecats 
  library(ggrepel) #automatically position non-overlapping text labels
  library(glue) #interpreted literal strings
  library(gt)
  library(gtExtras)
  library(paletteer)
  library(mlbplotR)
})

# 1.0 load mlbplotr data --------------------------------------------------------


#load team logos
teams_colors_logos <- mlbplotR::load_mlb_teams() %>% 
  filter(!team_abbr %in% c("AL", "NL", "MLB")) %>% 
  mutate(
    a = rep(1:6, 5), 
    b = sort(rep(1:5, 6), decreasing=T), 
    alpha = ifelse(grepl("A", team_abbr),1,0.75),
    color = ifelse(grepl("E", team_abbr), "b/w", NA)
  )


# 2.0 load exposures and projections--------------------------------------------

exposure <- read.csv("./data/exposure_mar30.csv")

exposure <- exposure %>% 
  mutate(Picked.At = as.Date(as.POSIXct(exposure$Picked.At, format="%Y-%m-%d %H:%M:%S", tz="UTC")), 
         name = paste(First.Name, Last.Name)) %>% 
  select(name, Team, Position, Picked.At, Pick.Number, Draft) %>% 
  left_join(teams_colors_logos %>% select(team_abbr, team_logo_espn), by=c('Team'='team_abbr')) %>% 
  left_join(read.csv("./data/playerids.csv"), by=c('name'='Name')) %>% 
  mutate(
    copy = paste0(name, Draft),
    playerid = as.double(playerid)) %>% 
  distinct(copy, .keep_all = T) %>% 
  left_join(mlbplotR::load_headshots() %>% select(fangraphs_id, espn_headshot) %>% drop_na(fangraphs_id), by=c("playerid"="fangraphs_id")) %>% 
  select(-copy) %>% 
  left_join(read.csv("./projections_season/rankings_mar20.csv") %>% 
              mutate(name = paste(firstName, lastName), 
                     adp = as.numeric(adp)) %>% 
              select(name, adp, projectedPoints, positionRank),
            by=c("name")) %>% 
  mutate(value = Pick.Number-adp, 
         rel_value = round(value/adp, digits = 2),
         positionGroup = gsub("[^A-Z]","",positionRank)) %>% 
  drop_na(adp) %>% 
  arrange(Pick.Number)

#drafts by date
drafts_by_date <- exposure %>% 
  group_by(Picked.At) %>% 
  summarize(total_picks = n(),
            total_value = sum(value, na.rm = T), 
            total_rel_value = sum(rel_value, na.rm = T)) %>% 
  mutate(value_per_pick = round(total_value/total_picks,digits = 2), 
         rel_value_per_pick = round(total_rel_value/total_picks,digits=2))


#top ten picks by value
exposure %>% 
  select(name, Pick.Number, adp, value, rel_value, Picked.At, Draft) %>% 
  arrange(-rel_value) %>% 
  slice_head(n=25)

#group by team drafted
exposure %>% 
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
exposure %>% 
  group_by(Position) %>% 
  summarise(count=n()) %>% 
  arrange(-count) %>% 
  ungroup() %>% 
  mutate(own = round(count/sum(count),digits = 2)) %>% 
  gt() 


#group by position
exposure %>% 
  group_by(Draft, Team) %>% 
  summarise(count=n()) %>% 
  arrange(-count) %>% 
  ungroup() %>% 
  group_by(Team) %>% 
  summarise(count=n()) %>% 
  arrange(-count) %>% 
  gt() %>% 
  gt_theme_dark()

ord <- c("P","IF","OF")

#configurations of P, OF, IF
exposure_config <- exposure %>% 
  group_by(Draft, positionGroup) %>% 
  summarise(count=n()) %>% 
  arrange(Draft, factor(positionGroup, levels = ord)) %>% 
  ungroup() %>% 
  group_by(Draft) %>% 
  summarise(config = as.numeric(paste0(count, collapse = ""))) %>% 
  ungroup() 

exposure_config %>% 
  group_by(config) %>% 
  summarise(count = n()) %>% 
  arrange(-count)

#stacked batters by draft
exposure_batters <- exposure %>% 
  filter(positionGroup != "P") %>% 
  group_by(Draft, Team) %>% 
  summarise(batters = n()) %>% 
  ungroup() %>% 
  filter(batters > 1) %>% 
  group_by(Draft) %>% 
  summarise(batters = sum(batters)) %>% 
  arrange(-batters)

# biggest stack per draft
exposure_big_stack <- exposure %>% 
  filter(positionGroup != "P") %>% 
  group_by(Draft, Team) %>% 
  summarise(batters = n()) %>% 
  ungroup() %>% 
  group_by(Draft) %>% 
  summarise(big_stack = max(batters)) %>% 
  ungroup()

#number of teams with stacked batters per draft
exposure_num_teams <- exposure %>% 
  filter(positionGroup != "P") %>% 
  group_by(Draft, Team) %>% 
  summarise(batters = n()) %>% 
  ungroup() %>% 
  filter(batters > 1) %>% 
  group_by(Draft) %>% 
  summarise(teams_stacked = n())

#find first pick of each draft
first_pick <- exposure %>% 
  filter(Pick.Number < 13) %>% select(name, Draft) %>% 
  rename("first_pick" = "name")

#drafts
drafts <- exposure %>% 
  #drop_na() %>% 
  group_by(Draft) %>% 
  summarize(total_picks = n(),
            total_value = sum(value), 
            total_rel_value = sum(rel_value), 
            Picked.At = last(Picked.At)) %>% 
  mutate(value_per_pick = round(total_value/total_picks, digits = 2),
         rel_value_per_pick = round(total_rel_value/total_picks, digits = 2)) %>% 
  arrange(-rel_value_per_pick) %>% 
  left_join(exposure_config, by=c("Draft")) %>% 
  left_join(exposure_batters, by=c("Draft")) %>% 
  left_join(exposure_big_stack, by=c("Draft")) %>% 
  left_join(exposure_num_teams, by=c("Draft")) %>% 
  left_join(first_pick, by=c("Draft")) %>% 
  mutate(file = paste(config, teams_stacked, batters, big_stack, first_pick))

#create a list from the dataframe
exp_list <- split(exposure, exposure$Draft)

name_mapping <- data.frame(
  old_names = drafts$Draft, 
  new_names = drafts$file, 
  stringsAsFactors = F
)

# Find the indices of the old names in the dataframe
name_indices <- match(names(exp_list), name_mapping$old_names)

# Rename the list elements using the new names from the dataframe
names(exp_list)[!is.na(name_indices)] <- name_mapping$new_names[name_indices[!is.na(name_indices)]]

#group by player
exposure %>% 
  group_by(name, espn_headshot) %>% 
  summarise(count=n()) %>% 
  arrange(-count) %>% 
  ungroup() %>% 
  mutate(own = round(count/dim(drafts)[1],digits = 2)) %>% 
  gt() %>% 
  gt_img_rows(columns = espn_headshot, height = 50)

#lets look at the pitchers i drafted 
exposure %>% 
  filter(positionGroup == "P") %>% 
  group_by(name, espn_headshot, team_logo_espn, projectedPoints) %>% 
  summarise(drafted = round(mean(Pick.Number),digits = 1), adp = last(adp), delta = drafted - adp, count = n(), exp = round(n()/72,digits = 2)) %>% 
  ungroup() %>% 
  arrange(adp) %>% 
  mutate(Rank = seq(1:79)) %>% 
  relocate(Rank) %>% 
  gt() %>% 
  tab_header(title = "The Dinger Pitcher ADP on Underdog") %>% 
  gt_color_rows(columns = c("delta", "exp"), palette = c("red", "green")) %>% 
  gt_img_rows(columns = "espn_headshot") %>% 
  gt_img_rows(columns = "team_logo_espn")

#pick number analysis
exposure %>%
  filter(Pick.Number < 13) %>% 
  group_by(Pick.Number) %>% 
  summarise(n=n())

#pick number analysis
exposure %>%
  filter(Pick.Number < 13) %>% 
  group_by(Pick.Number) %>% 
  summarise(n=n(), rel_value = mean(rel_value)) %>% 
  ggplot(aes(x=factor(Pick.Number), y=rel_value)) +
  geom_bar(stat = "identity") 

exposure %>%
  filter(Pick.Number < 13) %>% 
  ggplot(aes(x = factor(Pick.Number))) + 
  geom_bar() +
  scale_y_continuous(breaks = seq(1,10, by=1))

# best draft  -------------------------------------------------------------

draft_id <- "e350b09c-3389-46f0-8539-337179228c9a"

draft <- exposure %>% 
  filter(Draft==draft_id) %>% 
  select(name, team_logo_espn, espn_headshot, Pick.Number, adp, value, rel_value, projectedPoints) %>% 
  arrange(Pick.Number)

file <- paste(drafts$config[which(drafts$Draft == draft_id)],
              drafts$teams_stacked[which(drafts$Draft == draft_id)],
              drafts$batters[which(drafts$Draft == draft_id)],
              drafts$big_stack[which(drafts$Draft == draft_id)],
              drafts$first_pick[which(drafts$Draft == draft_id)])

draft %>% 
  gt() %>% 
  gt_img_rows(columns = team_logo_espn, height = 50) %>% 
  gt_img_rows(columns = espn_headshot, height = 50) %>% 
  gt_color_rows(rel_value, palette = c("red","green"), domain = c(-.5,.5)) %>% 
  gt_theme_dark() %>% 
  gtsave(filename = paste0(file,".html"))
