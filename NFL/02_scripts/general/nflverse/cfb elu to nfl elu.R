# lets project cfb elu to nfl elu

# load packages
suppressMessages({
  library(dplyr)
  library(tidyr)
  library(glue)
})

# create list of rb data
nfl_rb_list <- lapply(2014:2022, function(year){
  nfl_rb <- read.csv(glue("./01_data/training_data/{year}/rushing_summary (17).csv")) %>% 
    mutate(season = year)
})

# summarize and filter the nfl data
nfl_rb <- bind_rows(nfl_rb_list) %>% 
  group_by(player_id) %>% 
  summarise(player = last(player),
            season = last(season), 
            attempts = last(attempts), 
            grades_run = last(grades_run),
            yco_attempt = last(yco_attempt), 
            designed_yards_attempt = round(last(designed_yards) / last(attempts), digits = 1),
            elusive_rating = round(mean(elusive_rating, na.rm = T), digits = 1)) %>% 
  ungroup() %>% 
  filter(attempts > 20)

# load the cfb rb Rdata file
load(file = "./01_data/cfb/cfb_rb.Rdata")

# Join the data
rb <- nfl_rb %>% select(player_id, player, attempts, elusive_rating) %>% 
  left_join(cfb_rb %>% select(-player), by=c("player_id")) %>% 
  rename(attempts_nfl = attempts.x, 
         attempts_cfb = attempts.y,
         elu_cfb = elusive_rating.y, 
         elu_nfl = elusive_rating.x) %>% 
  relocate(elu_cfb, .after = elu_nfl) %>% 
  mutate(delta = elu_nfl - elu_cfb) %>% 
  relocate(delta, .after = elu_cfb) %>% 
  drop_na() %>% 
  filter(attempts_nfl > 82) #mean qualified nfl rb attempts

# Correlation between nfl and cfb elu
cor(rb$elu_nfl, rb$elu_cfb)

# create linear model
rb_elu <- lm(elu_nfl ~ elu_cfb + grades_run, data = rb)
rb_elu <- lm(elu_nfl ~ elu_cfb + grades_run + yco_attempt + designed_yards_attempt, data = rb)

# save model
saveRDS(rb_elu, file = "./04_modelS/rb_elu.rds")

# load depth chart data
load(file = "./01_data/depth_chart/depth_chart_data.Rdata")

# load starting running backs
rb1 <- nfl_depth_full$player[which(nfl_depth_full$pos == "RB1")]

rb1[which(rb$player %in% rb1 == F)]

# Load player to predict
new_data <- cfb_rb %>% 
  filter(player == "Bijan Robinson") %>% 
  select(elusive_rating, grades_run) %>% 
  #select(elusive_rating, grades_run, yco_attempt, designed_yards_attempt) %>% 
  rename(elu_cfb = elusive_rating)

predict(rb_elu, newdata = new_data)
