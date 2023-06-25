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

ud_rankings_sflex <- function(x) {
  
  # define some dates
  first_date <- "apr19"
  current_month <- "jun"
  
  # load rosters
  rosters <- load_rosters(2022) %>% select(full_name, headshot_url)
  
  # Load the opening rankings
  rankings_udd_1 <- read.csv(glue("./01_data/projections/season/2023_sflex/rankings_{first_date}_sflex.csv")) %>% 
    mutate(name = paste(firstName, lastName),
           adp = as.numeric(adp)) %>% 
    select(name, adp, projectedPoints, positionRank, slotName, teamName)
  
  # Load the most current rankings by reading the last file in the directory
  rankings_udd_2 <- read.csv(paste0("./01_data/projections/season/2023_sflex/", 
                                    list.files(path = "./01_data/projections/season/2023_sflex/")
                                    [max(which(grepl(current_month,list.files(path = "./01_data/projections/season/2023_sflex/"))))]
  )) %>% 
    mutate(name = paste(firstName, lastName), 
           adp = as.numeric(adp)) %>% 
    select(name, adp, projectedPoints, positionRank, slotName, teamName)
  
  # extract the date from the file name
  second_date <<- str_extract(list.files(path = "./01_data/projections/season/2023/")
                              [max(which(grepl(current_month,list.files(path = "./01_data/projections/season/2023/"))))], 
                              "(?<=_)[a-z]+[0-9]+")
  
  # Combine rankings in a single dataframe
  rankings <<- rankings_udd_1 %>% 
    select(-projectedPoints) %>% 
    left_join(rankings_udd_2 %>% select(name, adp, projectedPoints), by=c("name")) %>% 
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
    left_join(teams_colors_logos %>% select(team_name, team_logo_espn), by=c('teamName'='team_name')) %>% 
    rename(team = team_logo_espn) %>% 
    left_join(rosters, by=c("name"="full_name")) %>% 
    rename(headshot = headshot_url) %>% 
    distinct(name, .keep_all = T)
  
  # create a GT table with the rankings
  rankings_sflex <- rankings %>% 
    #select(-c(8,9)) %>% 
    relocate(team, .after = name) %>% 
    relocate(headshot, .before = name) %>% 
    drop_na() %>% 
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
  
  gtsave(rankings_sflex, filename = glue("./03_plots/best_ball_board/{second_date} UD NFL SFLEX Board.html"))
  
}
ud_rankings(x)

sflex_qb <- rankings %>% 
  filter(slotName == "QB") %>% 
  #select(c(1,3:7)) %>% 
  arrange(-percent_change) %>% 
  #mutate(points_per_adp = projectedPoints / june19) %>% 
  relocate(team, .after = name) %>% 
  relocate(headshot, .before = name) %>% 
  select(-teamName) %>% 
  gt() %>% 
  gt_img_rows(columns = team, height = 50) %>% 
  gt_img_rows(columns = headshot, height = 50) %>% 
  data_color(columns = percent_change, colors = scales::col_numeric(
    palette = c("red", "green"), 
    domain = c(min(rankings$percent_change, na.rm = T), max(rankings$percent_change, na.rm = T))
  )) %>% 
  tab_header(
    title = "2023 UD NFL SFLEX Big Board: QB Risers"
  ) %>% 
  gt_theme_dark()

gtsave(sflex_qb, filename = glue("./03_plots/best_ball_board/{second_date} UD NFL SFLEX Board.html"))
