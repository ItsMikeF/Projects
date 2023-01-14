#lets take a look at pff season long sos metrics

#load packages
suppressMessages({
  library(tidyverse) #metapackage
  library(nflverse) #functions to efficiently access NFL pbp data
  library(fs) #Cross-Platform File System Operations Based on 'libuv'
})

#manually load each file
#lets avoid doing this
{
  #qbs_sos <- read.csv(paste0("./season_projections/",list.files(path = "./season_projections", pattern = "qb")))
  #rbs_sos <- read.csv(paste0("./season_projections/",list.files(path = "./season_projections", pattern = "rb")))
  #wrs_sos <- read.csv(paste0("./season_projections/",list.files(path = "./season_projections", pattern = "wr")))
  #tes_sos <- read.csv(paste0("./season_projections/",list.files(path = "./season_projections", pattern = "te")))
  #dst_sos <- read.csv(paste0("./season_projections/",list.files(path = "./season_projections", pattern = "dst")))
}

#load the file paths, not the file names
file_paths <- dir_ls(path = "./season_projections/")

#read all csv files and merge into a list
file_contents <- file_paths %>% 
  map(function (path){
    read_csv(path)
  })

#remove the defense and player projections
file_contents[c(1,2)] <- NULL

#merge all data from the list
sos <- file_contents %>% 
  reduce(left_join, by="Offense")

#change all column names
colnames(sos) <- paste0("a",1:dim(sos)[2])

#set weights for weighted avg by positional value
wts <- c(0.4, 0.2, 0.1, 0.3)
if_else(sum(wts) == 1, print("Proceed"), print("Check your weights "))

#example to print
if (file.exists("./Results/golfers_results_no_odds.csv")) {
  unlink("./Results/golfers_results_no_odds.csv")
  cat("The file is deleted.")
} else {
  cat("The file was not found.")
}

#create row means for each position
sos$qb <- rowMeans(sos[2:19], na.rm = T) *wts[1]
sos$rb <- rowMeans(sos[26:43], na.rm = T) *wts[2]
sos$te <- rowMeans(sos[50:67], na.rm = T) *wts[3]
sos$wr <- rowMeans(sos[74:91], na.rm = T) *wts[4]

#create smaller data frame with sos wtd avg
sos <- sos %>% 
  select(a1, qb, rb, te, wr) %>% 
  mutate(str = rowMeans(sos[,2:5], na.rm = T))
  
#write normalize function where N(0,1)
normalize <- function(x){
  return( (x - min(x,na.rm = T))/( max(x, na.rm = T) - min(x, na.rm = T)) )
}

#add normalized column
sos$str_norm <- normalize(sos$str)*10 

#change pff team names to nflverse team names
sos <- replace(sos, sos == 'ARZ', 'ARI')
sos <- replace(sos, sos == 'BLT', 'BAL')
sos <- replace(sos, sos == 'CLV', 'CLE')
sos <- replace(sos, sos == 'HST', 'HOU')

#plot the teams sos with logos
ggplot2::ggplot(sos, aes(x = a1, y = str_norm)) +
  ggplot2::geom_col(aes(color = a1, fill = a1), width = 0.5) +
  nflplotR::scale_color_nfl(type = "secondary") +
  nflplotR::scale_fill_nfl(alpha = 0.5) +
  ggplot2::labs(
    title = "2022/2023 NFL Strength of Schedule",
    subtitle = "Positional wts: 0.4 QB, 0.2 RB, 0.1 TE, 0.3 WR \n 
    The strength of the opponent is rated from 0 to 10, the higher the value, the more favorable the matchup.",
    caption = "Twitter: @Its_MikeF \n Data from PFF", 
    y = "Normalized SoS"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    plot.title.position = "plot",
    # it's obvious what the x-axis is so we remove the title
    axis.title.x = ggplot2::element_blank(),
    # this line triggers the replacement of team abbreviations with logos
    axis.text.x = element_nfl_logo(size = 1.5)
  )
