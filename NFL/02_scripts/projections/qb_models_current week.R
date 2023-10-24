# use mahomes model

# load data

table <- read.csv(glue("./01_data/training_data/2023/defense_summary (7).csv")) %>%
  group_by(team_name) %>%
  summarise(def = round(weighted.mean(grades_defense, snap_counts_defense), digits = 1),
            rdef = round(weighted.mean(grades_run_defense, snap_counts_run_defense), digits = 1),
            tack = round(weighted.mean(grades_tackle, snap_counts_defense, na.rm = TRUE), digits = 1),
            prsh = round(weighted.mean(grades_pass_rush_defense, snap_counts_pass_rush), digits = 1),
            cov = round(weighted.mean(grades_coverage_defense, snap_counts_coverage), digits =1))

def_table <- table %>% 
  mutate(week = 7,
         year = 2023,
         team_name = gsub('ARZ','ARI', team_name), 
         team_name = gsub('BLT','BAL', team_name), 
         team_name = gsub('CLV','CLE', team_name), 
         team_name = gsub('HST','HOU', team_name), 
         #team_name = gsub('JAX','JAC', team_name), 
         team_name = gsub('LA','LAR', team_name), 
         team_name = gsub('LARC','LAC', team_name), 
         def_join = paste0(year, week, team_name))

new_data <- def_table %>% 
  filter(team_name == schedule$opponent) %>% 
  select(def, prsh, cov)

# load pass block
offense_blocking <- read.csv("./01_data/training_data/2023/offense_blocking (7).csv")

team_ol <- offense_blocking %>% 
  group_by(team_name) %>% 
  summarise(grades_pass_block = weighted.mean(grades_pass_block, snap_counts_pass_block)) %>% 
  ungroup() 

team_ol %>% 
  filter(team_name == "KC") %>% 
  select(grades_pass_block)


# load model
str(rf_model)

projection <- predict(rf_model, newdata = c(new_data, 
                                            team_ol %>% 
                                              filter(team_name == "KC") %>% 
                                              select(grades_pass_block)))
projection
