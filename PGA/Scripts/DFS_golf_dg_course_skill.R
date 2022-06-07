#Load packages
library(tidyverse)

#Import csv
course <- read.csv("./Results/dg_course_table.csv")
skill <- read.csv("./Results/dg_skill_ratings.csv")

#Select Course

course$yardage_rank <- rank(-course$yardage)
course$yardage_4_5_rank <- rank(-course$yardage_4_5)
course$yardage_3_rank <- rank(-course$yardage_3)

course$adj_score_to_par_rank <- rank(course$adj_score_to_par)
course$adj_par_3_score_rank <- rank(course$adj_par_3_score)
course$adj_par_4_score_rank <- rank(course$adj_par_4_score)
course$adj_par_5_score_rank <- rank(course$adj_par_5_score)

course$adj_driving_distance_rank <- rank(-course$adj_driving_distance)
course$adj_sd_distance_rank <- rank(course$adj_sd_distance)
course$adj_driving_accuracy_rank <- rank(-course$adj_driving_accuracy)

course$putt_sg_rank <- rank(-course$putt_sg)
course$arg_sg_rank <- rank(-course$arg_sg)
course$app_sg_rank <- rank(-course$app_sg)
course$ott_sg_rank <- rank(-course$ott_sg)

course <- course %>% 
  filter(course == "TPC Craig Ranch") %>% 
  select(par,
         yardage, yardage_rank, 
         adj_score_to_par, adj_score_to_par_rank, 
         adj_driving_distance, adj_driving_distance_rank, 
         adj_driving_accuracy, adj_driving_accuracy_rank, 
         putt_sg, putt_sg_rank,
         arg_sg, arg_sg_rank, 
         app_sg, app_sg_rank, 
         ott_sg, ott_sg_rank)

course$adj_score_to_par_rank / 75 * (7400 - 6300) + 6300
