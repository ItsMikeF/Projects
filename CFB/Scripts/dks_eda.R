#lets play some cfb dfs

#load packages
suppressMessages({
  library(tidyverse) #ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, forcats
  library(pdftools) #text extraction, rendering, and converting of pdf documents
  library(tidytext) #Text Mining using 'dplyr', 'ggplot2', and Other Tidy Tools
})

#read csv
dks <- read_csv("./contests/2022_w0/DKSalaries.csv")

qbs <- dks %>% filter(Position=="QB")
rbs <- dks %>% filter(Position=="RB")
wrs <- dks %>% filter(Position=="WR") %>% 
  left_join(read.csv("./contests/2022_w0/receiving_summary.csv"), 
            by=c("Name"="player"))
wrs %>% 
  select(Name, team_name, pass_plays, grades_offense, yprr) %>% 
  filter(pass_plays > 0.2*max(pass_plays, na.rm=T)) %>% 
  arrange(-grades_offense) 
