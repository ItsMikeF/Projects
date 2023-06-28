# Aggregate pga dfs data

# Load packages
suppressMessages({
  library(tidyverse) 
  library(ggplot2)
  library(ggrepel) #positions non-overlapping text labels
  library(lubridate) #make dealing with dates a little easier
  library(filesstrings) #handy file and string manipulation
  library(lpSolve) #solver for general linear/integer problems
  library(stats) #R statistical functions
  library(XML) #tools for parsing and generating XML
  library(binr) #cut numeric values into evenly distributed bins
  library(janitor) #clearning dirty data
  library(stringr) #simple consistent wrappers for common string operations
  library(reshape2) #flexibly Reshape Data
  library(gt)
  library(gtExtras)
  library(glue)
})


# 1.0 Gather input values  ------------------------------------------------


# Set number of entries to generate
entries <- 20
own_multiplier <- 100/entries

# Gather folder and file information
folder <- list.dirs()[length(list.dirs())-4]

# find the index of the 'forward slash'
slash <- which(strsplit(folder, "")[[1]] == "/")[2]

# define the tournament
tournament <- str_sub(folder, slash+1, nchar(folder))

# define the date
date <- as.Date(str_sub(tournament, 1, slash))


# 2.0 Import data ---------------------------------------------------------


# Load and edit the DK Salaries csv
golf_salaries <- read.csv(paste0(folder, "/DKSalaries.csv")) %>% 
  select(Name, ID, Salary, AvgPointsPerGame)

# Import rotogrinders csv
rg <- read.csv(paste0(folder, "/", list.files(path = folder, pattern = "projections_draftkings_golf"))) %>% 
  select(name, salary, fpts, proj_own, ceil, floor, player_id) %>% 
  arrange(-salary)

# Check if all values in the proj own
if (all(rg$proj_own == rg$proj_own[1])) {
  print("Script stopped bc all RG proj own values are equal")
} else {
  print("script continues.")
}

# Add Functions
convert_ML <- function(odds) {
  breakeven <- if_else(odds > 0, 100 / (100 + odds), abs(odds) / (abs(odds) + 100))
  return(round(breakeven, digits = 4))
}

# Import dg projections
dg_proj <- read.csv(paste0(folder, "/", list.files(path = folder, pattern = "draftkings_main_projections"))) %>%
  select(dk_name, scoring_points, finish_points, total_points, value, projected_ownership) %>% 
  mutate(projected_ownership = round(projected_ownership, digits = 2))

# Check if all own values are equal
if (all(dg_proj$projected_ownership == dg_proj$projected_ownership[1])) {
  print("Script stopped because DG projected ownership column is all the same.")
} else {
  print("Script continues")
}

# Import dg decomposition
decomp <- read.csv(paste0(folder, "/", list.files(path = folder, pattern = "dg_decomposition"))) %>%
  select(player_name, baseline, age, age_adj, true_sg_adj, timing_adj, sg_category_adj, course_history_adj, driving_dist_adj, driving_acc_adj, 
         fit_other_adj, course_fit_total_adj, final_prediction) %>%
  separate(player_name, into = c("last", "first"), sep = ",") %>% 
  unite("Player", first:last, sep = " ") %>% 
  mutate(Player = trimws(Player), 
         Player = recode(Player, 
                         "Matthew Fitzpatrick" = "Matt Fitzpatrick",
                         "Kyounghoon Lee" = "Kyoung-Hoon Lee",
                         "Jordan Smith" = "Jordan L. Smith", 
                         "Haotong Li" = "Hao-Tong Li", 
                         "Dimitrios Papadatos" = "Dimi Papadatos"))

# Import data golf model predictions
cam <- read.csv(paste0(folder, "/", list.files(path = folder, pattern = "_model"))) %>%
  select(player_name, make_cut, top_20, top_10, top_5, win) %>%
  separate(player_name, into = c("last", "first"), sep = ",") %>% 
  unite("Golfer", first:last, sep = " ") %>% 
  mutate(Golfer = trimws(Golfer)) %>% 
  select(Golfer, make_cut, top_20, top_10, top_5, win) %>% 
  mutate(across(2:6, convert_ML), 
         across(2:6, round, 4), 
         Golfer = recode(Golfer, 
                         "Matthew Fitzpatrick" = "Matt Fitzpatrick",
                         "Kyounghoon Lee" = "Kyoung-Hoon Lee",
                         "Jordan Smith" = "Jordan L. Smith", 
                         "Haotong Li" = "Hao-Tong Li", 
                         "Dimitrios Papadatos" = "Dimi Papadatos"))

# Import skill ratings
dg_skill <- read.csv(paste0(folder, "/", "dg_skill_ratings.csv")) %>%
  select(player_name, sg_putt_pred, sg_putt_rank, sg_arg_pred, sg_arg_rank, sg_app_pred, sg_app_rank, sg_ott_pred, sg_ott_rank, sg_total_pred,
         sg_total_rank, distance_pred, distance_rank, accuracy_pred, accuracy_rank) %>%
  separate(player_name, into = c("last", "first"), sep = ",") %>% 
  unite("Golfer", first:last, sep = " ") %>% 
  mutate(Golfer = trimws(Golfer), 
         Golfer = recode(Golfer, 
                         "Matthew Fitzpatrick" = "Matt Fitzpatrick",
                         "Kyounghoon Lee" = "Kyoung-Hoon Lee",
                         "Jordan Smith" = "Jordan L. Smith", 
                         "Haotong Li" = "Hao-Tong Li", 
                         "Dimitrios Papadatos" = "Dimi Papadatos"))

# 3.0 Create course table -----------------------------------------------------

# these must be manually updated for every course
driv_dis <- 0.7
driv_acc <- 0.5
app <- 0.7
arg <- 0.4
putt <- 0.4
course <- data.frame(driv_dis, driv_acc, app, arg, putt) 
course[2,] <- round(course/rowSums(course), digits = 2)


# 4.0 Create golfer table -----------------------------------------------------

golfers <- golf_salaries %>% 
  left_join(rg %>% select(-c(salary, player_id)), by=c("Name" = "name")) %>% 
  left_join(dg_proj, by = c("Name" = "dk_name")) %>%
  left_join(dg_skill, by = c("Name" = "Golfer")) %>% 
  left_join(decomp, by =c("Name" = "Player")) %>%
  left_join(cam, by=c("Name" = "Golfer")) %>% 
  drop_na() %>% 
  mutate(odds_per_dollar = round(win / Salary * 10^6, digits = 2),
         residuals = round(residuals(loess(odds_per_dollar ~ Salary)), digits = 2))

# 4.1 Normalized golfer table -------------------------------------------------

# Define normalize function
normalize <- function(x){
  return( (x - min(x,na.rm = T))/( max(x, na.rm = T) - min(x, na.rm = T)) )
}

golfers_norm <- golfers %>%
  mutate(across(3:length(.), normalize))

row <- 2

golfers_norm <- golfers_norm %>% 
  mutate(proj_own_avg = round((0.5*projected_ownership) + (0.5*proj_own),digits=2), 
         course_fit = 
           (course$driv_dis[row] * distance_pred) +
           (course$driv_acc[row] * accuracy_pred) +
           (course$app[row] * sg_app_pred) +
           (course$arg[row] * sg_arg_pred) +
           (course$putt[row] * sg_putt_pred),
         own_change = round(((0.1 * residuals) + (0.9*course_fit))*5, digits = 2), 
         adj_own = case_when(proj_own_avg + own_change <= 0 ~ 0,
                             proj_own_avg + own_change < 40 ~ round((proj_own_avg + own_change)/own_multiplier)*own_multiplier, 
                             proj_own_avg + own_change >= 40 ~ 40), 
         date = date, 
         tournament = tournament)


# 4.2 Create final golfer tibble ----------------------------------------------


# Create golfer tibble
max_own = 40

golfers <- golfers %>% 
  mutate(proj_own_avg = round((0.5*projected_ownership) + (0.5*proj_own),digits = 2),
         fpts_avg = round(0.5*fpts + 0.5*total_points, digits = 2),
         course_fit = round((golfers_norm$course_fit - mean(golfers_norm$course_fit)) / sd(golfers_norm$course_fit),digits = 2), 
         own_change = round(((0.1 * residuals) + (0.9*course_fit))*5, digits = 2), 
         manual_change = 0)


# 5.0 Identify Values -----------------------------------------------------

golfers %>% 
  mutate(value = course_fit/Salary) %>% 
  arrange(-value) %>% 
  select(Name, Salary, value, fpts_avg, proj_own_avg)

# 5.1 Manual Changes ------------------------------------------------------


#golfers$manual_change[which(golfers$Name == "Mark Hubbard")] = 5


# 6.0 Adjusted own  -------------------------------------------------------

golfers <- golfers %>% 
  mutate(adj_own = case_when(proj_own_avg + 2*own_change + manual_change <= 0 ~ 0,
                             proj_own_avg + 2*own_change + manual_change < max_own ~ round((rowMeans(golfers[,c(6,13)]) + own_change)/own_multiplier + manual_change/own_multiplier)*own_multiplier, 
                             proj_own_avg + 2*own_change + manual_change >= max_own ~ max_own),
         one = 1, 
         date = date, 
         tournament = tournament)

golfers %>% 
  select(Name, own_change, manual_change, adj_own)

golfers %>% 
  mutate(metric = fpts_avg / Salary) %>% 
  arrange(-metric) %>% 
  select(Name, Salary, fpts_avg, proj_own_avg)

# 7.0 Data presentations ----------------------------------------------------------

# value plot
golfer_plot <- function() {golfers %>%
    ggplot(aes(x = Salary , y = odds_per_dollar)) +
    geom_hline(yintercept = mean(golfers$odds_per_dollar), color = "red", linetype = "dashed", alpha=0.5) +
    geom_vline(xintercept =  mean(golfers$Salary), color = "red", linetype = "dashed", alpha=0.5) +
    geom_smooth(method=loess, se=F) +
    geom_point(aes(color = residuals), alpha = 0.7, cex = 3) +
    scale_color_gradient(low = "red", high = "green", guide = "colourbar") +
    geom_text_repel(aes(label=Name), box.padding = 0.25) +
    labs(x = "Salary",
         y = "odds_per_dollar",
         title = paste(tournament, "Golfers"),
         caption = "Twitter: Its_MikeF | Data: DraftKings") +
    theme_bw() +
    theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold")) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
}
#golfer_plot()

# create gt table
golfers_gt <- function() {golfers %>% 
  select(Name, Salary, fpts, total_points, projected_ownership, proj_own, proj_own_avg,
         sg_putt_rank, sg_arg_rank, sg_app_rank, sg_ott_rank, sg_total_rank, 
         distance_rank, accuracy_rank, course_fit_total_adj, 
         make_cut, top_20, win, residuals, course_fit, adj_own) %>% 
  gt() %>% 
  gt_theme_dark() %>%
  tab_header(tournament) %>% 
  data_color(columns = fpts, colors = scales::col_numeric(
    palette = c("red", "green"), 
    domain = range(golfers$fpts)
  )) %>% 
  data_color(columns = proj_own, colors = scales::col_numeric(
    palette = c("red", "green"), 
    domain = range(golfers$proj_own)
  )) %>% 
  data_color(columns = sg_putt_rank, colors = scales::col_numeric(
    palette = c("green", "red"), 
    domain = range(golfers$sg_putt_rank)
  )) %>% 
  data_color(columns = sg_arg_rank, colors = scales::col_numeric(
    palette = c("green", "red"), 
    domain = range(golfers$sg_arg_rank)
  )) %>% 
  data_color(columns = sg_app_rank, colors = scales::col_numeric(
    palette = c("green", "red"), 
    domain = range(golfers$sg_app_rank)
  )) %>% 
  data_color(columns = sg_ott_rank, colors = scales::col_numeric(
    palette = c("green", "red"), 
    domain = range(golfers$sg_ott_rank)
  )) %>% 
  data_color(columns = distance_rank, colors = scales::col_numeric(
    palette = c("green", "red"), 
    domain = range(golfers$distance_rank)
  )) %>% 
  data_color(columns = win, colors = scales::col_numeric(
    palette = c("red", "green"), 
    domain = range(golfers$win)
  )) %>% 
  data_color(columns = residuals, colors = scales::col_numeric(
    palette = c("red", "green"), 
    domain = range(golfers$residuals)
  )) %>% 
  data_color(columns = course_fit, colors = scales::col_numeric(
    palette = c("red", "green"), 
    domain = range(golfers$course_fit)
  )) %>% 
  data_color(columns = accuracy_rank, colors = scales::col_numeric(
    palette = c("red", "green"), 
    domain = range(golfers$accuracy_rank)
  )) %>% 
  gtsave(.,filename = paste0(folder, "/golfer_table.html"))
  
}

# Cor plot function
cor_plot <- function() {
  
  # Generate correlation plot of the variables
  num_cols <- unlist(lapply(golfers_norm, is.numeric))
  golfers_norm_cor <- golfers_norm[,num_cols]
  
  golfers_norm_cor %>%
    cor() %>%
    melt() %>%
    ggplot(aes(Var1, Var2, fill=value)) +
    geom_tile(color='white') +
    scale_fill_distiller(palette = 'GnBu', direction = 1) +
    geom_text(aes(label=paste(round(value,2)*100,'%')), size=2.5, color='black') +
    labs(x='',y='',fill='correlations', title='Relationship between golfer variables') +
    theme(axis.text.x = element_text(angle = 90, vjust = .5))
}

# 9.0 Save golfers object as Rdata------------------------------------------------


saveRDS(golfers, file = glue("{folder}/golfers.RData"))
saveRDS(golfers, file = glue("./03_results/golfers.Rdata"))
