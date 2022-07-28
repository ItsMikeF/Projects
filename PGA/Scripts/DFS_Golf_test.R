#load packages
suppressMessages({
  library(tidyverse) #metapackage
  library(ggrepel) #positions non-overlapping text labels
  library(lubridate) #make dealing with dates a little easier
  library(utils) #R utility functions
  library(filesstrings) #handy file and string manipulation
  library(xtable) #export tables to latex or HTML
  library(lpSolve) #solver for general linear/integer problems
  library(stats) #R statistical functions
  library(XML) #tools for parsing and generating XML
  library(binr) #cut numeric values into evenly distributed bins
  library(officer) #manipulation of word and pptx 
  library(janitor) #clearning dirty data
  library(stringr) #simple consistent wrappers for common string operations
  library(reshape2) #flexibly Reshape Data
})

#entries
entries <- 20

#automated values
folder <- list.dirs()[20]
date <- as.Date(str_sub(folder, 3, 12))
tournament <- str_sub(folder, 14, nchar(folder))

#import salaries
golf_salaries <- read.csv(paste0(folder, "/DKSalaries.csv")) %>% 
  select(Name, ID, Salary, AvgPointsPerGame)

#import rotogrinders
rg <- read.csv(paste0(folder, "/", list.files(path = folder, pattern = "projections_draftkings_golf"))) %>% 
  select(name, fpts, proj_own, ceil, floor)

#Add Functions
convert_ML <- function(odds) {
  breakeven <- if_else(odds > 0, 100 / (100 + odds), abs(odds) / (abs(odds) + 100))
  return(round(breakeven, digits = 4))
}

#import dg projections
dg_proj <- read.csv(paste0(folder, "/", list.files(path = folder, pattern = "draftkings_main_projections"))) %>%
  select(dk_name, scoring_points, finish_points, total_points, value, projected_ownership) %>% 
  mutate(projected_ownership = round(projected_ownership, digits = 2))

#import dg decomposition
decomp <- read.csv(paste0(folder, "/", list.files(path = folder, pattern = "dg_decomposition"))) %>%
  select(player_name, baseline, age, age_adj, true_sg_adj, timing_adj, sg_category_adj, course_history_adj, driving_dist_adj, driving_acc_adj, 
         fit_other_adj, course_fit_total_adj, final_prediction) %>%
  separate(player_name, into = c("last", "first"), sep = ",") %>% 
  unite("Player", first:last, sep = " ") %>% 
  mutate(Player = trimws(Player))
{
  decomp$Player <- replace(decomp$Player, decomp$Player == "Matthew Fitzpatrick", "Matt Fitzpatrick")
  decomp$Player <- replace(decomp$Player, decomp$Player == "Kyounghoon Lee", "Kyoung-Hoon Lee")
  decomp$Player <- replace(decomp$Player, decomp$Player == "Jordan Smith", "Jordan L. Smith")
  decomp$Player <- replace(decomp$Player, decomp$Player == "Haotong Li", "Hao-Tong Li")
  decomp$Player <- replace(decomp$Player, decomp$Player == "Dimitrios Papadatos", "Dimi Papadatos")
  }

#import data golf model predictions
cam <- read.csv(paste0(folder, "/", list.files(path = folder, pattern = "_model"))) %>%
  select(player_name, make_cut, top_20, top_10, top_5, win) %>%
  separate(player_name, into = c("last", "first"), sep = ",") %>% 
  unite("Golfer", first:last, sep = " ") %>% 
  mutate(Golfer = trimws(Golfer)) %>% 
  select(Golfer, make_cut, top_20, top_10, top_5, win) %>% 
  mutate(across(2:6, convert_ML), 
         across(2:6, round, 4))
{
  cam$Golfer <- replace(cam$Golfer, cam$Golfer == "Matthew Fitzpatrick", "Matt Fitzpatrick")
  cam$Golfer <- replace(cam$Golfer, cam$Golfer == "Kyounghoon Lee", "Kyoung-Hoon Lee")
  cam$Golfer <- replace(cam$Golfer, cam$Golfer == "Jordan Smith", "Jordan L. Smith")
  cam$Golfer <- replace(cam$Golfer, cam$Golfer == "Haotong Li", "Hao-Tong Li")
  cam$Golfer <- replace(cam$Golfer, cam$Golfer == "Dimitrios Papadatos", "Dimi Papadatos")
  }

dg_skill <- read.csv(paste0(folder, "/", "dg_skill_ratings.csv")) %>%
  select(player_name, sg_putt_pred, sg_putt_rank, sg_arg_pred, sg_arg_rank, sg_app_pred, sg_app_rank, sg_ott_pred, sg_ott_rank, sg_total_pred,
         sg_total_rank, distance_pred, distance_rank, accuracy_pred, accuracy_rank) %>%
  separate(player_name, into = c("last", "first"), sep = ",") %>% 
  unite("Golfer", first:last, sep = " ") %>% 
  mutate(Golfer = trimws(Golfer))
{
  dg_skill$Golfer <- replace(dg_skill$Golfer, dg_skill$Golfer == "Matthew Fitzpatrick", "Matt Fitzpatrick")
  dg_skill$Golfer <- replace(dg_skill$Golfer, dg_skill$Golfer == "Kyounghoon Lee", "Kyoung-Hoon Lee")
  dg_skill$Golfer <- replace(dg_skill$Golfer, dg_skill$Golfer == "Jordan Smith", "Jordan L. Smith")
  dg_skill$Golfer <- replace(dg_skill$Golfer, dg_skill$Golfer == "Haotong Li", "Hao-Tong Li")
  dg_skill$Golfer <- replace(dg_skill$Golfer, dg_skill$Golfer == "Dimitrios Papadatos", "Dimi Papadatos")
  }

#Create golfer tibble
own_multiplier <- 100/entries
driv_dis <- 0.4
driv_acc <- 0.5
app <- 0.2
arg <- 0.4
putt <- 0.5
course <- data.frame(driv_dis, driv_acc, app, arg, putt) 
course[2,] <- round(course/rowSums(course), digits = 2)

golfers <- golf_salaries %>% 
  left_join(rg, by=c("Name" = "name")) %>% 
  left_join(dg_proj, by = c("Name" = "dk_name")) %>%
  left_join(dg_skill, by = c("Name" = "Golfer")) %>% 
  left_join(decomp, by =c("Name" = "Player")) %>%
  left_join(cam, by=c("Name" = "Golfer")) %>% 
  drop_na() %>% 
  mutate(odds_per_dollar = round(win / Salary * 10^6, digits = 2),
         residuals = round(residuals(loess(odds_per_dollar ~ Salary)), digits = 2))

normalize <- function(x){
  return( (x - min(x,na.rm = T))/( max(x, na.rm = T) - min(x, na.rm = T)) )
}

golfers_norm <- golfers

for(i in 3:length(golfers_norm)){
  golfers_norm[,i] = normalize(golfers_norm[,i])
}

row <- 2

golfers_norm <- golfers_norm %>% 
  mutate(proj_own_avg = round((0.6*projected_ownership) + (0.4*proj_own),digits=2), 
         course_fit = 
           (course$driv_dis[row] * distance_pred) +
           (course$driv_acc[row] * accuracy_pred) +
           (course$app[row] * sg_app_pred) +
           (course$arg[row] * sg_arg_pred) +
           (course$putt[row] * sg_putt_pred),
         own_change = course_fit, 
         adj_own = case_when(proj_own_avg + own_change <= 0 ~ 0,
                             proj_own_avg + own_change < 40 ~ round((proj_own_avg + own_change)/own_multiplier)*own_multiplier, 
                             proj_own_avg + own_change >= 40 ~ 40), 
         date = date, 
         tournament = tournament)

golfers <- golfers %>% 
  mutate(proj_own_avg = round((0.6*projected_ownership) + (0.4*proj_own),digits = 2),
         course_fit = round((golfers_norm$course_fit - mean(golfers_norm$course_fit)) / sd(golfers_norm$course_fit),digits = 2), 
         own_change = round(((0.1 * residuals) + (0.9*course_fit))*5, digits = 2), 
         adj_own = case_when(proj_own_avg + own_change <= 0 ~ 0,
                             proj_own_avg + own_change < 40 ~ round((rowMeans(golfers[,c(6,13)]) + own_change)/own_multiplier)*own_multiplier, 
                             proj_own_avg + own_change >= 40 ~ 40), 
         one = 1, 
         date = date, 
         tournament = tournament)

#value plot
{golfers %>%
    ggplot(aes(x = Salary , y = odds_per_dollar)) +
    geom_hline(yintercept = mean(golfers$odds_per_dollar), color = "red", linetype = "dashed", alpha=0.5) +
    geom_vline(xintercept =  mean(golfers$Salary), color = "red", linetype = "dashed", alpha=0.5) +
    geom_smooth(method=loess, se=F) +
    geom_point(aes(color = residuals), alpha = 0.7, cex = 3) +
    scale_color_gradient(low = "red", high = "green", guide = "colourbar") +
    geom_text_repel(aes(label=Name)) +
    labs(x = "Salary",
         y = "odds_per_dollar",
         title = paste(tournament, "Golfers"),
         caption = "Twitter: Its_MikeF | Data: DraftKings") +
    theme_bw() +
    theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold")) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
  }

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

#Write
write.csv(golfers, file = paste0(folder, "/golfers_",entries,".csv"))
write.csv(golfers, file = paste0("./Results/golfers_",entries,".csv"))
