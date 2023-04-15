#lets calculate the distance between coordinates

library(tidyverse)
library(geosphere)
library(openxlsx)

ports <- read.csv("ports.csv")
plants <- read.csv("plants.csv")

ports_join <- ports %>% left_join(plants %>% select(Plant.Code, Latitude, Longitude), by=c("Plant.Code"))

ports_join <- ports_join %>% 
  rename(port_lat = Latitude.x, 
         port_long = Longitude.x, 
         plant_lat = Latitude.y, 
         plant_long = Longitude.y)

#example distGeo functions
distGeo(c(-123.8300,46.9621),c(-122.3938,47.2594))*.00062137
distGeo(c(ports_join$port_long[1],ports_join$port_lat[1]),c(ports_join$plant_long[1],ports_join$plant_lat[1]))*.00062137

ports_join <- ports_join %>%
  mutate(dist = distGeo(
    p1 = cbind(port_long, port_lat),
    p2 = cbind(plant_long, plant_lat)) *.00062137
  ) 

write.xlsx(ports_join, file = "dist.xlsx")