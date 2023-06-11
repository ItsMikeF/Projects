# merge all pff qb data

#load packages
suppressMessages({
  library(tidyverse)
  library(glue)
  }) 

# Get training data folder
folder <- list.dirs()[which(list.dirs() == "./01_data/training_data/2014")]
year <- substring(folder, nchar(folder)-3, nchar(folder))

# Define folder indices
folder_index_start <- which(list.dirs() == "./01_data/training_data/2014")
folder_index_end <- folder_index_start + (year(Sys.Date())- 1 - 2014)
# minus 1 in offseason, remove for in season


# Get column names from week 1 files
# passing summary
passing_summary <- read.csv(paste0(folder,"/passing_summary (1).csv"))
passing_summary_cols <- colnames(passing_summary)

#passing pressure
passing_pressure <- read.csv(paste0(folder,"/passing_pressure (1).csv"))
passing_pressure_cols <- colnames(passing_pressure)
passing_pressure_cols <- passing_pressure_cols[
  -which(passing_pressure_cols %in% c("blitz_grades_pass_route",
                                      "no_blitz_grades_pass_route",
                                      "no_pressure_grades_pass_route",
                                      "pressure_grades_pass_route"))
  ]

#passing concept
passing_concept <- read.csv(paste0(folder,"/passing_concept (1).csv"))
passing_concept_cols <- colnames(passing_concept)
passing_concept_cols <- passing_concept_cols[
  -which(passing_concept_cols %in% c("no_screen_grades_pass_route",
                                     "npa_grades_pass_route",
                                     "pa_grades_pass_route", 
                                     "screen_grades_pass_route"))
  ]

#passing depth
passing_depth <- read.csv(paste0(folder,"/passing_depth (1).csv"))
passing_depth_cols <- colnames(passing_depth)


#time in pocket
time_in_pocket <- read.csv(paste0(folder,"/time_in_pocket (1).csv"))
time_in_pocket_cols <- colnames(time_in_pocket)
time_in_pocket_cols <- time_in_pocket_cols[
  -which(time_in_pocket_cols %in% c("less_grades_pass_route",
                                    "more_grades_pass_route"))
  ]

# write qb data to a list
qbs <- map(folder_index_start:folder_index_end, function(index){
  folder <- list.dirs()[index]
  year <- substring(folder, nchar(folder)-3, nchar(folder))
  
  map(1:17, function(game_week){
    print(paste("Year:",year, "week:", game_week))
    
    passing_summary <- read.csv(glue("{folder}/passing_summary ({game_week}).csv")) %>% 
      select(all_of(passing_summary_cols))
    
    passing_pressure <- read.csv(glue("{folder}/passing_pressure ({game_week}).csv")) %>% 
      select(passing_pressure_cols[c(2,7:length(passing_pressure_cols))]) %>% 
      select(-c("declined_penalties", "franchise_id", 
               "grades_hands_fumble", "grades_offense", 
               "grades_pass", "grades_run" ))
    
    passing_concept <- read.csv(glue("{folder}/passing_concept ({game_week}).csv")) %>% 
      select(passing_concept_cols[c(2,6:length(passing_concept_cols))]) %>% 
      select(-c("declined_penalties", "franchise_id", "penalties" ))
    
    passing_depth <- read.csv(glue("{folder}/passing_depth ({game_week}).csv")) %>% 
      select(passing_depth_cols[c(2,8:length(passing_depth_cols))])
    
    time_in_pocket <- read.csv(glue("{folder}/time_in_pocket ({game_week}).csv")) %>% 
      select(time_in_pocket_cols[c(2,6:length(time_in_pocket_cols))])
    
    list(passing_summary, passing_pressure, passing_concept, passing_depth, time_in_pocket) %>% 
      reduce(left_join, by = "player_id") %>% 
      mutate(year = year, week = game_week)
    
  })
})

# find where duplicate column names are
names <- names(qbs)
names[which(grepl(pattern = ".x", names))]

# Bind nested list of dataframes 
qbs <- map(qbs, bind_rows)

# Bind list to dataframe
qbs <- bind_rows(qbs)

# Save RData 
saveRDS(qbs, file = "./01_data/training_data/position_groups/qbs.RData")
