#load packages
suppressMessages({
  library(tidyverse, warn.conflicts = F) #metapackage
  library(stringr) #simple consistent wrappers for common string operations
})

#list tournaments in golfer results
tournaments <- {c("2022-04-10 The Masters", 
                 "2022-04-17 RBC Heritage", 
                 "2022-05-01 Mexico Open",
                 "2022-05-08 Wells Fargo", 
                 "2022-05-15 AT&T Byron Nelson", 
                 "2022-05-22 PGA Championship", 
                 "2022-05-29 Charles Schwab", 
                 "2022-06-05 Memorial Tournament", 
                 "2022-06-12 RBC Canadian Open", 
                 "2022-06-19 US Open", 
                 "2022-06-26 Travelers Championship", 
                 "2022-07-03 John Deere Classic", 
                 "2022-07-10 Genesis Scottish Open", 
                 "2022-07-17 Open Championship", 
                 "2022-07-24 3M Open", 
                 "2022-07-31 Rocket Mortgage Classic", 
                 "2022-08-07 Wyndham Championshio", 
                 "2022-08-14 FedEx St. Jude Championship",
                 "2022-08-21 BMW Championship")}

#deletes file if it exists
if (file.exists("./Results/golfers_results_no_odds.csv")) {
  unlink("./Results/golfers_results_no_odds.csv")
  cat("The file is deleted.")
} else {
  cat("The file was not found.")
}

#write training data
for (i in c(5,8:length(tournaments))) {
  print(paste("Start",tournaments[i]))
  #find latest training data folder
  folder <- paste0("./Training_data/",tournaments[i])
  
  #automated inputs
  date <- as.Date(str_sub(folder, nchar("./Training_data/")+1, nchar("./Training_data/")+10))
  tournament <- str_sub(folder, nchar("./Training_data/")+12, nchar(folder))
  
  #import salaries
  golf_salaries <- read.csv(paste0(folder, "/DKSalaries.csv")) %>% 
    select(Name, ID, Salary, AvgPointsPerGame)
  
  #import rotogrinders
  rg <- read.csv(paste0(folder, "/", list.files(path = folder, pattern = "projections_draftkings_golf"))) %>% 
    select(name, fpts, proj_own, ceil, floor)
  
  #import dg projections
  dg_proj <- read.csv(paste0(folder, "/", list.files(path = folder, pattern = "draftkings_main_projections"))) %>%
    select(dk_name, scoring_points, finish_points, total_points, value, projected_ownership)
  
  #import dg decomposition
  decomp <- read.csv(paste0(folder, "/", list.files(path = folder, pattern = "dg_decomposition"))) %>%
    select(player_name, baseline, age, age_adj, true_sg_adj, timing_adj, sg_category_adj, course_history_adj, driving_dist_adj, driving_acc_adj, 
           fit_other_adj, course_fit_total_adj, final_prediction) %>%
    separate(player_name, into = c("last", "first"), sep = ",") %>% 
    unite("Player", first:last, sep = " ") %>% 
    mutate(Player = trimws(Player))
  
  #import data golf model predictions
  cam <- read.csv(paste0(folder, "/", list.files(path = folder, pattern = "-model"))) %>%
    select(player_name, make_cut, first_round_lead, top_30, top_20, top_10, top_5, top_3, win, observed_finish) %>%
    separate(player_name, into = c("last", "first"), sep = ",") %>% 
    unite("Golfer", first:last, sep = " ") %>% 
    mutate(Golfer = trimws(Golfer), 
           observed_finish = gsub('T', '', observed_finish), 
           observed_finish = gsub('CU', '100', observed_finish), 
           observed_finish = gsub('WD', '', observed_finish)) %>% 
    select(Golfer, make_cut, first_round_lead, top_30, top_20, top_10, top_5, top_3, win, observed_finish) %>% 
    mutate(across(2:9, round, 4),
           observed_finish = as.numeric(observed_finish))
  
  #import dfs points, ownership, and salaries
  results <- read.csv(paste0(folder, "/", list.files(path = folder, pattern = "draftkings_pga_"))) %>%
    select(player_name, ownership, total_pts) %>%
    separate(player_name, into = c("last", "first"), sep = ",") %>% 
    unite("Player", first:last, sep = " ") %>% 
    mutate(Player = trimws(Player)) %>% 
    select(Player, ownership, total_pts)
  
  #Create golfer tibble
  own_multiplier <- 100/100
  
  golfers <- golf_salaries %>% 
    left_join(rg, by=c("Name" = "name")) %>% 
    left_join(dg_proj, by = c("Name" = "dk_name")) %>%
    left_join(decomp, by =c("Name" = "Player")) %>%
    left_join(cam, by=c("Name" = "Golfer")) %>% 
    left_join(results, by = c("Name" = "Player")) %>% 
    drop_na() %>% 
    mutate(odds_per_dollar = round(win / Salary * 10^6, digits = 2), 
           residuals = round(residuals(loess(odds_per_dollar ~ Salary)), digits = 2), 
           own_change = round((own_multiplier * 0.15 * residuals), digits = 2), 
           adj_own = case_when(proj_own + own_change <= 0 ~ 0,
                               proj_own + own_change < 40 ~ round((proj_own + own_change)/own_multiplier)*own_multiplier, 
                               proj_own + own_change >= 40 ~ 40), 
           date = date, 
           tournament = tournament)
  print(paste("End",tournaments[i]))
  
  write.table(golfers, file = "./Results/golfers_results_no_odds.csv", sep = ",", col.names = !file.exists("./Results/golfers_results_no_odds.csv"), append = T, row.names = F)
}
