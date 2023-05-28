# lets project nfl passers for 2023

# Load packages
library(nflfastR)
library(dplyr)
library(lubridate)

# Load pbp
pbp <- load_pbp(2018:2022)

# look at the passer stats over last 5 years
pbp %>% 
  filter(season_type == "REG") %>% 
  mutate(year = year(ymd(game_date))) %>% 
  group_by(passer, year) %>% 
  summarise(passing_yards = sum(passing_yards, na.rm = T)) %>% 
  arrange(-passing_yards) %>% 
  filter(passer == "P.Mahomes")

pbp_list <- list()

for (year in 2018:2022) {
  pbp <- load_pbp(year) %>% 
    filter(season_type == "REG") %>% 
    mutate(year = year(ymd(game_date))) %>% 
    group_by(passer) %>% 
    summarise(year = first(year), 
              passing_yards = sum(passing_yards, na.rm = T)) %>% 
    arrange(-passing_yards)
  
  pbp_list[[year-2017]] <- pbp
}

# combine list into dataframe
qbs <- bind_rows(pbp_list)

qbs_2023 <- data.frame(qb1_pbp, NA)[qb1_pbp %in% qbs$passer,] %>% 
  rename_with(~ c("passer", "prediction"), c(1,2))

for (i in 1:dim(qbs_2023)[1]) {
  model <- lm(passing_yards ~ year, data = qbs %>% filter(passer == qbs_2023$passer[i]))
  new_data <- data.frame(year=2023)
  prediction <- round(predict(model, newdata = new_data), digits = 0)
  qbs_2023[i,2] <- prediction
}
qbs_2023 %>% arrange(-prediction)

i=6
model <- lm(passing_yards ~ year, data = qbs %>% filter(passer == qbs_2023$passer[i]))
new_data <- data.frame(year=2023)
prediction <- predict(model, newdata = new_data)
prediction
