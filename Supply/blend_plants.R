#load packages
library(tidyverse)
library(usmap)

#import data
blend_plants <- read_csv("blend_plants.csv")


#Example Code for reference

plot_usmap(regions = "counties") + 
  labs(title = "US Counties",
       subtitle = "This is a blank map of the counties of the United States.") + 
  theme(panel.background = element_rect(color = "black", fill = "lightblue"))

plot_usmap(data = statepop, values = "pop_2015", color = "red") + 
  scale_fill_continuous(name = "Population (2015)", label = scales::comma) + 
  theme(legend.position = "right")

#plot i like
plot_usmap(data = statepop, values = "pop_2015", color = "red") + 
  scale_fill_continuous(
    low = "white", high = "red", name = "Population (2015)", label = scales::comma
  ) + theme(legend.position = "right")

#using include and exclude
usmap::plot_usmap(include = c(.pacific, .south_region), 
                  exclude = c("AK","HI"))

#lets try to modify it
data = statepop

state_data <- data %>% 
  left_join(lift_state, by=c("abbr"="abbrev"))

state_data$total_amount <- replace_na(state_data$total_amount, 0)

plot_usmap(data = state_data,
           values = "total_amount",
           color = "blue") + 
  scale_fill_continuous(low = "white",
                        high = "blue",
                        name = "total_amount",
                        label = scales::comma) + 
  theme(legend.position = "right")

state_data

plot_usmap(data = state_data %>% filter(total_amount < 2000000 & total_amount >0),
           values = "total_amount",
           color = "blue") + 
  scale_fill_continuous(low = "white",
                        high = "blue",
                        name = "total_amount",
                        label = scales::comma) + 
  theme(legend.position = "right")

#gganimate main grade volume per port then show projections