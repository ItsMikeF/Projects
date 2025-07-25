# contest analysis


# 0.0 Load packages -------------------------------------------------------


# Load packages
suppressMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(janitor)
  library(gt)
  library(glue)
})


# 1.0 Define values -------------------------------------------------------


# Gather folder and file information
folder <- list.dirs()[length(list.dirs())-3]

# find the index of the 'forward slash'
slash <- which(strsplit(folder, "")[[1]] == "/")[2]

# define the tournament
tournament <- str_sub(folder, slash+1, nchar(folder))

# define the date
date <- as.Date(str_sub(tournament, 1, slash))


# 2.0 oad data ------------------------------------------------------------


# Read in the slate file
golfers <- readRDS(glue(folder, "/golfers.RData"))

# Read contest file and sepearte the linuep string into columns
contest <- read.csv(paste0(folder, "/",list.files(path = folder, pattern = "contest-standings")
                           )
                    ) %>% 
  separate(EntryName, into = c("username", "entry_count"), sep = " ") %>% 
  select(-c(8:12)) %>% 
  separate(Lineup, into = paste0("G", seq(1,6)), sep = " G ") %>% 
  separate(G1, into = c("G11", "G1"), sep = "G ") %>% 
  select(-G11)

# OWnership figures from contest file
ownership <- read.csv(paste0(folder, "/",list.files(path = folder, pattern = "contest-standings")
                             )
                      ) %>%
  select(8,10,11) %>% 
  rename(Name = Player, 
         own = X.Drafted, 
         fpts = FPTS) %>% 
  mutate(own = as.numeric(str_remove_all(own, "%")))


# 3.0 EDA -----------------------------------------------------------------

# group by number of entries per username
contest %>% 
  group_by(username) %>% 
  summarise(n=n(), 
            avg_points = mean(Points)) %>% 
  arrange(-n) %>% 
  count(n) %>% 
  ungroup() %>% 
  mutate(entries = n*nn, 
         entries_percent = round(entries / dim(contest)[1], digits = 3)
         ) %>% 
  janitor::adorn_totals()

# check exposure of the username with the winning lineup
exposure_wd <- contest %>% 
  filter(username == contest$username[1]) %>% 
  select(7:12) %>% 
  unlist() %>% 
  table() %>% 
  as_tibble() %>% 
  arrange(-n) %>% 
  mutate(expo = round(n*100/(sum(n)/6), digits = 2)) %>% 
  rename(Name = ".") %>% 
  left_join(ownership, by=c("Name")) %>% 
  left_join(golfers %>% select(-fpts), by=c("Name")) %>% 
  mutate(own_delta = round(expo - own, digits = 2), 
         fpts_delta = fpts - fpts_avg) %>% 
  select(Name, Salary, n, expo, own, own_delta, 
         proj_own_avg, fpts, fpts_avg, fpts_delta,
         course_fit, 
         sg_putt_rank, sg_arg_rank, sg_app_rank, sg_ott_rank, 
         distance_rank, accuracy_rank, age, make_cut, win, residuals)

# print username of winning lineup
contest$username[1]

# check my exposures vs the field
my_expo <- contest %>% 
  filter(username == "DeepSeaMike") %>% 
  select(7:12) %>% 
  unlist() %>% 
  table() %>% 
  as_tibble() %>% 
  arrange(-n) %>% 
  mutate(expo = n*100/(sum(n)/6)) %>% 
  rename(Name = ".") %>% 
  left_join(ownership, by=c("Name")) %>% 
  left_join(golfers %>% select(-fpts), by=c("Name")) %>% 
  mutate(own_delta = expo - own, 
         fpts_delta = fpts - fpts_avg) %>% 
  select(Name, Salary, n, expo, own, own_delta, 
         proj_own_avg, fpts, fpts_avg, fpts_delta,
         course_fit, 
         sg_putt_rank, sg_arg_rank, sg_app_rank, sg_ott_rank, 
         distance_rank, accuracy_rank, age, make_cut, win, residuals)

# check exposure of the top 100 lineups
top_100 <- contest %>% 
  filter(Rank %in% contest$Rank[1:100]) %>%
  select(7:12) %>% 
  unlist() %>% 
  table() %>% 
  as_tibble() %>% 
  arrange(-n) %>% 
  mutate(expo = n*100/100) %>% 
  rename(Name = ".") %>% 
  left_join(ownership, by=c("Name")) %>% 
  left_join(golfers %>% select(-fpts), by=c("Name")) %>% 
  mutate(delta = expo - own) %>% 
  select(Name, Salary, n, expo, own, delta, proj_own_avg, fpts, fpts_avg, course_fit, 
         sg_putt_rank, sg_arg_rank, sg_app_rank, sg_ott_rank, 
         distance_rank, accuracy_rank, age, make_cut, win, residuals)

test <- top_100 %>% select(-c(Name,expo)) %>% drop_na()
test <- as.data.frame(apply(test, 2, as.numeric))

# find correlation of stats with higher expo
correlation <- cor(test, 
                   test$expo)

cor(test$n, test$course_fit)
correlation <- as.data.frame(correlation)

# find correlation of stats with higher expo
correlation <- cor(top_100 %>% select(-c(Name,expo)) %>% drop_na(), 
                   top_100$expo)

# combined exposures of players with a top 10 finish
top_10_users <- contest %>% 
  filter(username %in% contest$username[1:10]) %>% 
  select(7:12) %>% 
  unlist() %>% 
  table() %>% 
  as_tibble() %>% 
  arrange(-n) %>% 
  rename(Name = ".") %>% 
  mutate(expo = round(n/(sum(n)/6), digits = 2))
