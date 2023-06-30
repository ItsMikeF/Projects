# merge contest files into 1 file

# load packages
library(dplyr)
library(purrr)
library(stringr)

# find the contest directories
directories <- list.dirs()

# find indices of the contests
indices <- grep("./01_data/", directories)

# get contests
contests_paths <- directories[indices]

# check how many files in each contest directory
file_count <- map_int(contests_paths, ~ length(list.files(.)))
file_count

grep("projections_draftkings_golf",list.files(contests_paths[118]))
grep("draftkings_main_projections",list.files(contests_paths[118]))
grep("draftkings_pga",list.files(contests_paths[118]))

# Import rotogrinders csv
rg <- read.csv(paste0(folder, "/", list.files(path = folder, pattern = "projections_draftkings_golf"))) %>% 
  select(name, salary, fpts, proj_own, ceil, floor, player_id) %>% 
  arrange(-salary)

# Import dg projections
dg <- read.csv(paste0(folder, "/", list.files(path = folder, pattern = "draftkings_main_projections"))) %>%
  select(dk_name, scoring_points, finish_points, total_points, value, projected_ownership) %>% 
  mutate(projected_ownership = round(projected_ownership, digits = 2))

# import milly ownership and points
milly <- list.files(path = folder, pattern = "draftkings_pga")


# gpt suggested -----------------------------------------------------------


# Function to check if the required files exist in the directory
check_files_exist <- function(dir){
  files <- list.files(dir)
  file1 <- any(grepl("projections_draftkings_golf", files))
  file2 <- any(grepl("draftkings_main_projections", files))
  file3 <- any(grepl("draftkings_pga", files))
  return(file1 & file2 & file3)
}

# Apply the function to each directory
files_exist <- map_lgl(contests_paths, check_files_exist)

# Print directories where all 3 files exist
indices_with_files <- which(files_exist)
contests_paths[indices_with_files]

# review below code  ------------------------------------------------------



# tbd if the below code is needed
contests <- map(1:length(contests_paths), function(x){
  unlist(str_split(contests_paths[x], "/"))[4]
})

contests <- na.omit(unlist(contests))

# count the number of files in each director
list.files(path = contests[1])



# remove contests without proper files
contests <- contests[-which(contests %in% c("2020-01-05 Sentry ToC",
                                                     "2020-01-12 Sony Open",
                                                     "2020-03-15 The Players", 
                                                     "2021-05-16 AT&T Byron Nelson",
                                                     "2021-06-13 Palmetto Champonship",
                                                     "2022-04-21 Zurich Classic", 
                                                     "2021-07-18 The Open Championship", 
                                                     "2021-08-01 Olympics Mens", 
                                                     "2021-08-29 BMW Championship", 
                                                     "2021-10-17 CJ Cup Summit", 
                                                     "2021-10-24 Zozo Championship", 
                                                     "2021-11-07 Technology Championship", 
                                                     "2021-12-05 Hero World Challenge"))]
#Add Functions
convert_ML <- function(odds) {
  breakeven <- if_else(odds > 0, 100 / (100 + odds), abs(odds) / (abs(odds) + 100))
  return(round(breakeven, digits = 4))
}

#Compile data
dg <- list()

for(i in c(1:(length(contests)-1))) {
  setwd(paste0("C://Users//",unlist(strsplit(getwd(), "/"))[3],"//Documents//GitHub//Projects//Golf/Training_data//",contests[i]))
  
  files <- length(list.files())
  
  dk <- read.csv(list.files(pattern = "draftkings_pga_"))
  dk <- dk %>% 
    select(event_name, 
           player_name, 
           salary, 
           ownership, 
           streak_pts,
           bogey_free_pts,
           hole_in_one_pts,
           sub_70_pts,
           hole_score_pts,
           finish_pts,
           total_pts)
  
  pn <- read.csv("pga_historical_outrights.csv")
  pn <- pn %>% 
    select(player_name, 
           open_odds, 
           close_odds)
  pn[,2:3] <- sapply(pn[,2:3], convert_ML)
  pn$odds_delta <- pn$close_odds - pn$open_odds
  pn$odds_delta_per <- round((pn$close_odds - pn$open_odds)/pn$open_odds, digits = 4)
  
  sg <- read.csv(list.files(pattern = "raw_pga_"))
  sg <- sg %>% 
    group_by(player_name, dg_id) %>% 
    summarize(round_score = mean(round_score), 
              sg_putt = sum(sg_putt, na.rm = T), 
              sg_arg = sum(sg_arg, na.rm = T), 
              sg_app = sum(sg_app, na.rm = T), 
              sg_ott = sum(sg_ott, na.rm = T), 
              sg_t2g = sum(sg_t2g, na.rm = T), 
              sg_total = sum(sg_total, na.rm = T), 
              driving_dist = mean(driving_dist, na.rm = T), 
              driving_acc = mean(driving_acc, na.rm = T), 
              gir = mean(gir, na.rm = T), 
              scrambling = mean(scrambling), 
              prox_rgh = mean(prox_rgh, na.rm = T), 
              prox_fw = mean(prox_fw, na.rm = T))
  
  cam <- read.csv(list.files(pattern = "_course-adjustment-model"))
  cam <- cam %>% 
    select(player_name, date, make_cut, top_20, top_10, top_5, top_3, win)
  
  dg[[i]] <- list(dk, pn, cam, sg) %>% 
    reduce(left_join, by = "player_name")
  
  print(contests[i])
}

setwd(paste0("C://Users//",unlist(strsplit(getwd(), "/"))[3],"//Documents//GitHub//Projects//Golf//Results"))

#Write dg csv
write.csv(dg[[1]], file = "dg.csv")

for(i in 2:length(dg)){
  write.table(tibble(dg[[i]]), file = "dg.csv", sep = ",", col.names = !file.exists("dg.csv"), append = T)
}
