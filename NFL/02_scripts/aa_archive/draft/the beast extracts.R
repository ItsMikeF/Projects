
library(tidyverse)
library(tabulizer)

tables <- extract_tables("./draft/TheBeast_NFL_Draft_Guide-6.pdf", pages = 4, password = "Bea$stguide2023!")
qb <- read.csv("./draft/passing_summary.csv")

beast_qbs <- as.data.frame(tables[[1]]) %>% slice_head(n=24) %>% 
  separate('V4', into = c("year", "height"), sep = " ") %>% 
  separate("V2", into = c("prospect_school", "grade"), sep = "(?<=\\s)(?=\\S*$)") %>% 
  mutate(prospect_first = str_split_fixed(prospect_school, " ", 3)[,1], 
         prospect_last = str_split_fixed(prospect_school, " ", 3)[,2], 
         prospect = str_to_title(paste(prospect_first, prospect_last)), 
         school = str_split_fixed(prospect_school, " ", 3)[,3]) %>% 
  rename(weight = "V5", 
         age = "V11") %>% 
  mutate(prospect = str_replace(prospect,"C.j. Stroud", "C.J. Stroud"), 
         prospect = str_replace(prospect,"Aidan O’connell", "Aidan O’Connell"), 
         prospect = str_replace(prospect,"Tanner Mckee", "Tanner McKee")
         ) %>% 
  select(prospect, school, grade, year, height, weight) %>% 
  slice(3:n()) %>% 
  left_join(qb %>% select(player, avg_depth_of_target, avg_time_to_throw, grades_offense, grades_pass, grades_run), 
            by=c("prospect"="player")) %>% 
  mutate(percentile = ntile(grades_pass, 10)*10)

