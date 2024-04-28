#lets look at the rg projections for cbb

library(tidyverse)
library(gt)

rg <- readxl::read_excel(path = "cbb 3.4.xlsx")

rg <- rg %>% select(Position, Name, ID, Salary, TeamAbbrev, `Proj Pts`, Value,`MIN%`, Role, `Role General`, `Defense vs Pos`, `DVP General`) %>% 
  mutate_if(is.numeric,round, digits =1) %>% 
  mutate(metric = round(`Proj Pts`*`Defense vs Pos`, digits = 1))

unique(rg$Position)

positions <- split(x = rg, f = list(rg$Position))

positions$F %>% 
  gt()

positions$F %>% 
  ggplot(aes(x=Salary, y=`Proj Pts`, label = Name)) +
  #geom_histogram()
  geom_point() +
  geom_label()

hist(positions$F$`Proj Pts`)

positions$G %>% 
  arrange(-`Proj Pts`) %>% 
  gt() %>% 
  data_color(columns = metric, colors = scales::col_numeric(
    palette = c("red", "green"), 
    domain = c(-300, 300)
  )) %>% 
  data_color(columns = `Proj Pts`, colors = scales::col_numeric(
    palette = c("red", "green"), 
    domain = c(-1, 45)
  )) %>% 
  data_color(columns = Salary, colors = scales::col_numeric(
    palette = c("red", "green"), 
    domain = c(4000, 10500)
  )) %>% 
  data_color(columns = `MIN%`, colors = scales::col_numeric(
    palette = c("red", "green"), 
    domain = c(0,1)
  )) %>% 
  gtsave(filename = "cbb.html")
