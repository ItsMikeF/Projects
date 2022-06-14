#Load packages
library(tidyverse)

#find folder
folder <- list.dirs()[20]

#course
contest_course <- "Winged Foot GC"

#Import csv
course_rankings <- read.csv(paste0(folder, "/dg_course_table.csv"))
skill <- read.csv(paste0(folder, "/dg_skill_ratings.csv"))

#add course_rankings
course_rankings$yardage_rank <- rank(-course_rankings$yardage)
course_rankings$yardage_4_5_rank <- rank(-course_rankings$yardage_4_5)
course_rankings$yardage_3_rank <- rank(-course_rankings$yardage_3)

course_rankings$adj_score_to_par_rank <- rank(course_rankings$adj_score_to_par)
course_rankings$adj_par_3_score_rank <- rank(course_rankings$adj_par_3_score)
course_rankings$adj_par_4_score_rank <- rank(course_rankings$adj_par_4_score)
course_rankings$adj_par_5_score_rank <- rank(course_rankings$adj_par_5_score)

course_rankings$adj_driving_distance_rank <- rank(-course_rankings$adj_driving_distance)
course_rankings$adj_sd_distance_rank <- rank(course_rankings$adj_sd_distance)
course_rankings$adj_driving_accuracy_rank <- rank(-course_rankings$adj_driving_accuracy)

course_rankings$putt_sg_rank <- rank(-course_rankings$putt_sg)
course_rankings$arg_sg_rank <- rank(-course_rankings$arg_sg)
course_rankings$app_sg_rank <- rank(-course_rankings$app_sg)
course_rankings$ott_sg_rank <- rank(-course_rankings$ott_sg)

#filter for contest
contest <- course_rankings %>% 
  filter(course == contest_course) %>% 
  select(par,
         yardage, yardage_rank, 
         adj_score_to_par, adj_score_to_par_rank, 
         adj_driving_distance, adj_driving_distance_rank, 
         adj_driving_accuracy, adj_driving_accuracy_rank, 
         putt_sg, putt_sg_rank,
         arg_sg, arg_sg_rank, 
         app_sg, app_sg_rank, 
         ott_sg, ott_sg_rank)

#Salary filter value
contest$adj_score_to_par_rank / 75 * (7400 - 6300) + 6300
