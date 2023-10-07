#dfs

#load packages
suppressMessages({
  library(nflfastR)
  library(tidyverse)
  library(ggrepel)
})

#load data
dksalaries <- read_csv("./contests/2022_w1/DKSalaries.csv")
names(dksalaries) <- gsub(" ","_",tolower(names(dksalaries)))

qbs <- dksalaries %>% 
  filter(position == "QB")
