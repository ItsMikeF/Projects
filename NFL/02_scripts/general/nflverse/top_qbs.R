#load packages
suppressMessages({
  library(tidyverse) #metapackage
  library(nflverse) #functions to efficiently access NFL pbp data
  library(reshape2)
})

pbp <- load_pbp(seasons = 2018:2021)

names <- tibble(names(pbp))

qbs <- pbp %>% 
  filter(pass == 1 | rush == 1) %>% 
  filter(down %in% 1:4) %>% 
  group_by(id) %>% 
  summarise(name = first(name),
            team = last(posteam),
            epa = round(sum(epa, na.rm = T), digits = 0), 
            qb_epa = round(sum(qb_epa, na.rm = T), digits = 0), 
            cpoe = round(mean(cpoe, na.rm = T), digits = 2),
            dropbacks = sum(qb_dropback),
            plays = n()) %>% 
  arrange(-qb_epa) %>%
  filter(plays > 200) %>% 
  slice_max(qb_epa, n = 10)

ggplot(qbs, aes(x = reorder(id, -qb_epa), y = qb_epa)) +
  geom_col(aes(color = team, fill = team), width = 0.5) +
  scale_color_nfl(type = "secondary") +
  scale_fill_nfl(alpha = 0.4) +
  labs(
    title = "2020 NFL Quarterback EPA per Play",
    y = "EPA/play"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    plot.title = element_text(face = "bold"),
    plot.title.position = "plot",
    
    # it's obvious what the x-axis is so we remove the title
    axis.title.x = element_blank(),
    
    # this line triggers the replacement of gsis ids with player headshots
    axis.text.x = element_nfl_headshot(size = 1)
  )

pbp_sort <- pbp %>% 
  filter(pass == 1 | rush == 1) %>%
  filter(down %in% 1:4) %>% 
  filter(name %in% qbs$name) %>% 
  select(id, name, posteam, epa, qb_epa, cpoe, qb_dropback)

pbp_sort_test <- pbp_sort %>% 
  filter(name == "P.Mahomes") 

ggplot(pbp_sort, aes(qb_dropback,epa)) +
  geom_point(aes(cumsum(pbp_sort_test$qb_dropback), cumsum(pbp_sort_test$epa)))

ggplot() +
  geom_line(data = pbp_sort %>% filter(name == qbs$name[1]),
            aes(x = cumsum(qb_dropback),
                y = cumsum(epa), 
                colour = name)) +
  geom_line(data = pbp_sort %>% filter(name == qbs$name[2]),
            aes(x = cumsum(qb_dropback),
                y = cumsum(epa), 
                colour = name)) +
  geom_line(data = pbp_sort %>% filter(name == qbs$name[3]),
            aes(x = cumsum(qb_dropback),
                y = cumsum(epa),
                colour = name)) +
  geom_line(data = pbp_sort %>% filter(name == qbs$name[4]),
            aes(x = cumsum(qb_dropback),
                y = cumsum(epa), 
                colour = name)) +
  geom_line(data = pbp_sort %>% filter(name == qbs$name[5]),
            aes(x = cumsum(qb_dropback),
                y = cumsum(epa), 
                colour = name)) +
  geom_line(data = pbp_sort %>% filter(name == qbs$name[6]),
            aes(x = cumsum(qb_dropback),
                y = cumsum(epa), 
                colour = name)) +
  geom_line(data = pbp_sort %>% filter(name == qbs$name[7]),
            aes(x = cumsum(qb_dropback),
                y = cumsum(epa), 
                colour = name)) +
  geom_line(data = pbp_sort %>% filter(name == qbs$name[8]),
            aes(x = cumsum(qb_dropback),
                y = cumsum(epa), 
                colour = name)) +
  geom_line(data = pbp_sort %>% filter(name == qbs$name[9]),
            aes(x = cumsum(qb_dropback),
                y = cumsum(epa), 
                colour = name)) +
  geom_line(data = pbp_sort %>% filter(name == qbs$name[10]),
            aes(x = cumsum(qb_dropback),
                y = cumsum(epa), 
                colour = name))
  
plot(cumsum(pbp_sort_test$qb_dropback), cumsum(pbp_sort_test$epa))
