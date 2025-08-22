# display upcoming games

library(tidyverse)
library(cfbfastR)

schedule <- load_cfb_schedules(2024)
