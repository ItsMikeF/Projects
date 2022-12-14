#lets predict a qb's next game grade based on past game grades and team epa data

#need game grades from every game

#load packages
suppressMessages({
  library(nflfastR) #nflfastr nflseedr nflplotr
  library(tidyverse) #ggplot2 dplyr tibble tidyr purrr forecats 
  library(glue) #interpreted literal strings
})

#define folder
folder <- "./game_grades/2022_qb"

#get the week 2 grades
grades <- read.csv(glue("{folder}/passing_summary (2).csv")) %>% 
  select(player, grades_pass, team_name)

#get the week 1 grades

pbp <- load_pbp(2022)

#all game weeks def pass epa
def_pass_epa <- pbp %>% 
  filter(pass == 1) %>% 
  group_by(defteam, posteam, week) %>% 
  summarize(epa = round(mean(epa), digits = 3)) %>% 
  select(defteam, epa, posteam, week) %>% 
  ungroup() %>% 
  #mutate(join = paste0(posteam, week))
  filter(posteam == "KC")

#mahomes game log
mahomes <- pbp %>% 
  filter(pass == 1 & passer == "P.Mahomes") %>% 
  group_by(passer, defteam, week) %>% 
  summarize(epa = round(mean(epa), digits = 3), 
            pass_yards = sum(passing_yards, na.rm = T)) %>% 
  select(passer, epa, pass_yards, defteam, week) %>% 
  ungroup() %>% 
  mutate(join = paste0(passer, pass_yards)) %>% 
  arrange(week)

qbs_list <- list()

#write csvs to list
for (i in 1:13) {
  passing_summary <- read.csv(glue("{folder}/passing_summary ({i}).csv")) %>% 
    select(player, grades_pass, yards)
  
  qbs_list[[i]] <- list(passing_summary)
}

#write list to dataframe
for(i in 1:13){
  
  write.table(qbs_list[[i]], 
              file = "./game_grades/qbs.csv", sep = ",", 
              col.names = !file.exists("./game_grades/qbs.csv"), 
              append = T, row.names = F)

}

#read csv
qbs <- read.csv(glue("game_grades/qbs.csv")) %>% 
  filter(player == "Patrick Mahomes")

qbs <- read.csv(glue("game_grades/qbs.csv")) %>% 
  mutate(name = player) %>% 
  separate(name, into = c("first_name", "last_name"), sep=" ") %>% 
  mutate(first = substr(first_name, 1, 1), 
         name = paste(first, last_name, sep = ".")) %>% 
  select(name, grades_pass, yards) %>% 
  mutate(join = paste0(name, yards))


#join grades to epa 
mahomes <- mahomes %>% 
  left_join(qbs, by=("join")) 

mahomes <- mahomes %>% 
  select(passer, grades_pass, epa, defteam)

#cbind the data
new <- cbind(mahomes, def_pass_epa %>% select())
