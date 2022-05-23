library(httr)
library(tidyverse)
library(gt)
library(stats)

sport_key <- "baseball_mlb_world_series_winner"
url <- paste0("https://odds.p.rapidapi.com/v4/sports/",sport_key,"/odds")

queryString <- list(
  regions = "us",
  oddsFormat = "american",
  dateFormat = "iso"
)

response <- VERB("GET", url, add_headers('X-RapidAPI-Host' = 'odds.p.rapidapi.com', 
                                         'X-RapidAPI-Key' = 'b34680a5eamsh5c39b8cf066dd0ap1e8d93jsnc9e5097b3c8c'), 
                 query = queryString, content_type("application/octet-stream"))

content <- content(response, "parsed")

bookmakers <- data.frame()

for (i in 1:length(content[[1]][[8]])) {
  bookmakers[i,1] <- content[[1]][[8]][[i]]$key
  bookmakers[i,2] <- length(content[[1]][[8]][[i]][[4]][[1]][[2]])
}

books <- c("draftkings", "fanduel", "foxbet", "williamhill_us", "twinspires", "unibet", "betfair")

### DK

ws_winner <- data.frame()

for (i in 1:length(content[[1]][[8]][[3]][[4]][[1]][[2]])) {
  ws_winner[i,1] <- content[[1]][[8]][[3]][[4]][[1]][[2]][[i]]$name
  ws_winner[i,2] <- content[[1]][[8]][[3]][[4]][[1]][[2]][[i]]$price
}

### FD

ws_winner_fd <- data.frame()

for (i in 1:length(content[[1]][[8]][[5]][[4]][[1]][[2]])) {
  ws_winner_fd[i,1] <- content[[1]][[8]][[5]][[4]][[1]][[2]][[i]]$name
  ws_winner_fd[i,2] <- content[[1]][[8]][[5]][[4]][[1]][[2]][[i]]$price
}

ws_winner <- ws_winner %>% 
  left_join(ws_winner_fd, by=c("V1"))

### FB

ws_winner_fb <- data.frame()

for (i in 1:length(content[[1]][[8]][[2]][[4]][[1]][[2]])) {
  ws_winner_fb[i,1] <- content[[1]][[8]][[2]][[4]][[1]][[2]][[i]]$name
  ws_winner_fb[i,2] <- content[[1]][[8]][[2]][[4]][[1]][[2]][[i]]$price
}

ws_winner <- ws_winner %>% 
  left_join(ws_winner_fb, by=c("V1"))

### SH

ws_winner_sh <- data.frame()

for (i in 1:length(content[[1]][[8]][[8]][[4]][[1]][[2]])) {
  ws_winner_sh[i,1] <- content[[1]][[8]][[8]][[4]][[1]][[2]][[i]]$name
  ws_winner_sh[i,2] <- content[[1]][[8]][[8]][[4]][[1]][[2]][[i]]$price
}

ws_winner <- ws_winner %>% 
  left_join(ws_winner_sh, by=c("V1"))

### WH

ws_winner_wh <- data.frame()

for (i in 1:length(content[[1]][[8]][[4]][[4]][[1]][[2]])) {
  ws_winner_wh[i,1] <- content[[1]][[8]][[4]][[4]][[1]][[2]][[i]]$name
  ws_winner_wh[i,2] <- content[[1]][[8]][[4]][[4]][[1]][[2]][[i]]$price
}

ws_winner <- ws_winner %>% 
  left_join(ws_winner_wh, by=c("V1"))

### UB

ws_winner_ub <- data.frame()

for (i in 1:length(content[[1]][[8]][[7]][[4]][[1]][[2]])) {
  ws_winner_ub[i,1] <- content[[1]][[8]][[7]][[4]][[1]][[2]][[i]]$name
  ws_winner_ub[i,2] <- content[[1]][[8]][[7]][[4]][[1]][[2]][[i]]$price
}

ws_winner <- ws_winner %>% 
  left_join(ws_winner_ub, by=c("V1"))

### Odds Columns ###

for (i in 1:dim(ws_winner)[1]) {
  ws_winner$best_odds[i] <- max(ws_winner[i,2:7])
}

ws_winner$mean_odds <- round(rowMeans(ws_winner[,2:7]), digits = -1)
ws_winner$delta <- ws_winner$best_odds - ws_winner$mean_odds

for (i in 1:dim(ws_winner)[1]) {
  ws_winner$sd[i] <- round(sd(ws_winner[i,2:7], na.rm = T), digits = -1)
}

### Getting the Date & Time ###

date_time <- gsub("Z","",unlist(strsplit(content[[1]][[8]][[1]]$last_update,"T")))
time <- format(as.POSIXct(strptime(paste(date_time[1], date_time[2]), format = "%Y-%m-%d %H:%M:%S") - 14400), format = "%H:%M:%S")

### Table

rm(list = ls(pattern = "ws_winner_"))
names(ws_winner) <- c("Team", "Draftkings", "FanDuel", "FoxBET", "SugarHouse", "WilliamHill", "Unibet", "BestOdds", "AvgOdds", "Delta", "sd")
view(ws_winner, title = paste(time, "WS Winner"))
