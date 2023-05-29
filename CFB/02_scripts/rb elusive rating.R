# lets project cfb elu to nfl elu

# load packages
library(dplyr)
library(glue)

# create list of rb data
cfb_rb_list <- lapply(2014:2022, function(year){
  cfb_rb <- read.csv(glue("./01_data/training_data/{year}/rushing_summary.csv")) %>% 
    mutate(season = year)
})

cfb_rb <- bind_rows(cfb_rb_list) %>% 
  filter(position == "HB") %>% 
  group_by(player_id) %>% 
  summarise(player = last(player),
            season = last(season), 
            attempts = sum(attempts),
            grades_run = round(weighted.mean(grades_run, yards), digits = 1),
            yco_attempt = round(mean(yco_attempt), digits = 1),
            designed_yards_attempt = round(last(designed_yards) / last(attempts), digits = 1),
            elusive_rating = round(weighted.mean(elusive_rating, yards), digits = 1))

save(cfb_rb, file = "./01_data/cfb_rb.Rdata")
