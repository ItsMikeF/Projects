qbs <- rankings %>% 
  filter(slotName == "QB") %>% 
  left_join(qb_grades, by=c("name"="player")) %>% 
  arrange(date2) %>% 
  select(name, date2, grades_pass)

qb_grades <- read.csv("./Training_Data/2022/passing_summary (17).csv")

qbs %>% ggplot(aes(x=date2, y=grades_pass)) +
  geom_point() +
  #geom_text(label=name) +
  geom_label(label = qbs$name) +
  labs(title = "Grades by ADP")
