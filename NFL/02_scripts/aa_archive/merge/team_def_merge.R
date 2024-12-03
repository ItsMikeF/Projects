# get weekly nfl pff team defense grades

# load necessary libraries
library(dplyr)
library(glue)
library(readr)

team_def <- lapply(2014:2023, function(y) {
  lapply(2:17, function(x) {
    tryCatch({
      table <- read.csv(glue("./01_data/training_data/{y}/defense_summary ({x}).csv")) %>%
        group_by(team_name) %>%
        summarise(def = round(weighted.mean(grades_defense, snap_counts_defense), digits = 1),
                  rdef = round(weighted.mean(grades_run_defense, snap_counts_run_defense), digits = 1),
                  tack = round(weighted.mean(grades_tackle, snap_counts_defense, na.rm = TRUE), digits = 1),
                  prsh = round(weighted.mean(grades_pass_rush_defense, snap_counts_pass_rush), digits = 1),
                  cov = round(weighted.mean(grades_coverage_defense, snap_counts_coverage), digits =1))
      
      table %>% 
        mutate(week = x,
               year = y,
               team_name = gsub('ARZ','ARI', team_name), 
               team_name = gsub('BLT','BAL', team_name), 
               team_name = gsub('CLV','CLE', team_name), 
               team_name = gsub('HST','HOU', team_name), 
               #team_name = gsub('JAX','JAC', team_name), 
               team_name = gsub('LA','LAR', team_name), 
               team_name = gsub('LARC','LAC', team_name), 
               def_join = paste0(year, week, team_name))
    }, error = function(e) {
      message(paste("Failed at year", y, "and week", x, "with message", e$message))
      return(data.frame())
    })
  })
})


# bind to dataframe
team_def <- bind_rows(team_def)

# save team defense object
save(team_def, file = "./01_data/training_data/position_groups/team_def.RData")
