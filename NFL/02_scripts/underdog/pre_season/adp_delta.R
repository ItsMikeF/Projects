# best ball ranking analysis

#load packages
suppressMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(nflfastR)
  library(nflreadr)
  library(nflplotR)
  library(fs) #Cross-Platform File System Operations Based on 'libuv'
  library(gt)
  library(gtExtras)
  library(ggrepel) #Automatically Position Non-Overlapping Text Labels with 'ggplot2'
  library(glue)
  library(lubridate)
})



# 1.0 Load rankings -------------------------------------------------------

# rankings delta with logos and headshots
ud_rankings <- function(x, roster_year) {
  
  # define some dates
  first_date <- "apr30"
  current_month <- tolower(as.character(month(Sys.Date(), label = T)[1]))
  
  # load rosters
  rosters <- load_rosters(roster_year) %>% select(full_name, headshot_url)
  
  # Load the opening rankings
  rankings_udd_1 <- read.csv(glue("./01_data/projections/season/2023/rankings_{first_date}.csv")) %>% 
    mutate(name = paste(firstName, lastName),
           adp = as.numeric(adp)) %>% 
    select(name, adp, projectedPoints, positionRank, slotName, teamName) %>% 
    drop_na(adp)
  
  # Load the most current rankings by reading the last file in the directory
  rankings_udd_2 <- read.csv(paste0("./01_data/projections/season/2023/", 
                                    list.files(path = "./01_data/projections/season/2023/")
                                    [max(which(grepl(current_month,list.files(path = "./01_data/projections/season/2023/"))))]
  )
  ) %>% 
    mutate(name = paste(firstName, lastName), 
           adp = as.numeric(adp)) %>% 
    select(name, adp, projectedPoints, positionRank, slotName, teamName) %>% 
    drop_na(adp)
  
  # extract the date from the file name
  second_date <<- str_extract(list.files(path = "./01_data/projections/season/2023/")
                             [max(which(grepl(current_month,list.files(path = "./01_data/projections/season/2023/"))))], 
                             "(?<=_)[a-z]+[0-9]+")
  
  # Combine rankings in a single dataframe
  rankings <<- rankings_udd_1 %>% 
    left_join(rankings_udd_2 %>% select(name, adp), by=c("name")) %>% 
    mutate(delta = adp.x-adp.y, 
           percent_change = round(delta/adp.x*100,digits = 1)) %>% 
    select(name, slotName, adp.y, adp.x, delta, percent_change, projectedPoints, teamName) %>% 
    rename_with(~ first_date, adp.x) %>% 
    rename_with(~ second_date, adp.y) %>% 
    arrange(.[[3]]) %>% 
    mutate(teamName = case_when(
      teamName == "NY Giants" ~ "New York Giants", 
      teamName == "NY Jets" ~ "New York Jets", 
      T ~ teamName
    )) %>% 
    left_join(teams_colors_logos %>% select(team_name, team_logo_espn) %>% distinct(), 
              by=c('teamName'='team_name'),
              relationship = "many-to-many") %>% 
    rename(team = team_logo_espn) %>% 
    left_join(rosters, 
              by=c("name"="full_name")) %>% 
    rename(headshot = headshot_url)
  
  # create a GT table with the rankings
  rankings_gt <- rankings %>% 
    relocate(team, .after = name) %>% 
    relocate(headshot, .before = name) %>% 
    gt() %>% 
    gt_img_rows(columns = team, height = 50) %>% 
    gt_img_rows(columns = headshot, height = 50) %>% 
    data_color(columns = percent_change, colors = scales::col_numeric(
      palette = c("red", "green"), 
      domain = c(min(rankings$percent_change, na.rm = T), max(rankings$percent_change, na.rm = T))
    )) %>% 
    gt_theme_dark() %>% 
    tab_header(
      title = glue("{toupper(second_date)} NFL Best Ball Rankings")
    ) 
  
  gtsave(rankings_gt, filename = glue("./03_plots/best_ball_board/{second_date} UD NFL Board.html"))
  
}
ud_rankings(x, 2022)
#write.csv(rankings, file = "ud_rankings_aug28.csv")

# 2.0 draft picks -------------------------------------------------------------

#define function to determine pick positions based on first pick
draft_positions <- function(i) {
  
  #define constants
  league_size <- 14
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
draft_positions(13)

rankings[picks,] %>% select(-c(8:10))

  
# 3.0 Risers and Fallers --------------------------------------------------

top <- 10

# First round
first_round <- rankings %>% 
  slice_head(n=12) %>% 
  #select(-c(8,9)) %>% 
  relocate(team, .after = name) %>% 
  relocate(headshot, .before = name) %>% 
  drop_na() %>% 
  gt() %>% 
  tab_header(title = glue("2023 NFL Best Ball: First Round")) %>% 
  gt_img_rows(columns = team, height = 50) %>% 
  gt_img_rows(columns = headshot, height = 50) %>% 
  data_color(columns = percent_change, colors = scales::col_numeric(
    palette = c("red", "green"), 
    domain = c(min(rankings$percent_change, na.rm = T), max(rankings$percent_change, na.rm = T))
  )) %>% 
  gt_theme_dark() %>% 
  tab_footnote("Data from Underdog Rankings | Twitter: @Its_MikeF", 
               placement = "auto")

gtsave_extra(first_round, filename = "./03_plots/best_ball_board/first_round.png")
  
# Top 10 Risers
risers <- rankings %>% 
  #select(-c(8,9)) %>% 
  relocate(team, .after = name) %>% 
  relocate(headshot, .before = name) %>% 
  drop_na() %>% 
  arrange(-percent_change) %>% 
  slice_head(n=top) %>% 
  gt() %>% 
  tab_header(title = glue("2023 NFL Best Ball: Top {top} Risers")) %>% 
  gt_img_rows(columns = team, height = 50) %>% 
  gt_img_rows(columns = headshot, height = 50) %>% 
  data_color(columns = percent_change, colors = scales::col_numeric(
    palette = c("red", "green"), 
    domain = c(min(rankings$percent_change, na.rm = T), max(rankings$percent_change, na.rm = T))
  )) %>% 
  gt_theme_dark() %>% 
  tab_footnote("Data from Underdog Rankings | Twitter: @Its_MikeF", 
               placement = "auto")

# Save as image using webshot
gtsave_extra(risers, filename = "./03_plots/best_ball_board/2023 risers.png")

# top 10 late round risers
late_risers <- rankings %>% 
  #select(-c(8,9)) %>% 
  filter(.[[3]] > 200) %>% 
  relocate(team, .after = name) %>% 
  relocate(headshot, .before = name) %>% 
  drop_na() %>% 
  arrange(-percent_change) %>% 
  slice_head(n=top) %>% 
  gt() %>% 
  tab_header(title = "2023 NFL Best Ball: Late Round Risers") %>% 
  gt_img_rows(columns = team, height = 50) %>% 
  gt_img_rows(columns = headshot, height = 50) %>% 
  data_color(columns = percent_change, colors = scales::col_numeric(
    palette = c("red", "green"), 
    domain = c(min(rankings$percent_change, na.rm = T), max(rankings$percent_change, na.rm = T))
  )) %>% 
  gt_theme_dark() %>% 
  tab_footnote("Data from Underdog Rankings | Twitter: @Its_MikeF", 
               placement = "auto")

gtsave_extra(late_risers, filename = "./03_plots/best_ball_board/2023 late risers.png")


# Top 10 Fallers
fallers <- rankings %>% 
  #select(-c(8,9)) %>% 
  relocate(team, .after = name) %>% 
  relocate(headshot, .before = name) %>% 
  drop_na() %>% 
  arrange(percent_change) %>% 
  slice_head(n=top) %>% 
  gt() %>% 
  tab_header(title = glue("2023 NFL Best Ball: Top {top} Fallers")) %>% 
  gt_img_rows(columns = team, height = 50) %>% 
  gt_img_rows(columns = headshot, height = 50) %>% 
  data_color(columns = percent_change, colors = scales::col_numeric(
    palette = c("red", "green"), 
    domain = c(min(rankings$percent_change, na.rm = T), max(rankings$percent_change, na.rm = T))
  )) %>% 
  gt_theme_dark() %>% 
  tab_footnote("Data from Underdog Rankings | Twitter: @Its_MikeF", 
               placement = "auto")

gtsave_extra(fallers, filename = "./03_plots/best_ball_board/2023 fallers.png")

# 5.0 Team Rankings -------------------------------------------------------

# define some dates
first_date <- "may05"
current_month <- "aug"

team <- rankings %>% 
  drop_na() %>% 
  filter(.[[3]] < 216) %>% 
  group_by(teamName) %>% 
  summarise(adp_mean = round(mean(.[[3]], na.rm = T),digits = 1))

team %>%
  select(teamName, team_logo_espn, adp_mean, adp_delta, percent_change) %>% 
  arrange(adp_mean) %>% 
  gt() %>% 
  tab_header(title = "2023 Best Ball - Mean Team ADP Movement", 
             subtitle = glue("Period: {first_date} to {second_date}")) %>% 
  gt_img_rows(columns=team_logo_espn, height = 50) %>% 
  gt_theme_dark() %>% 
  tab_footnote(footnote = "Data from Underdog NFL Rankings, players ADP > 215 filtered out")
