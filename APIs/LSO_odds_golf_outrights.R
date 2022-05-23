library(httr)
library(tidyverse)
library(stats)
library(lubridate, warn.conflicts = F)

sport_key <- "golf_pga_championship_winner"
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
names(bookmakers) <- c("book", "bets")

books <- c("draftkings", "fanduel", "foxbet", "barstool", "twinspires", "unibet", "betfair")

### DK

golfer_names_dk <- data.frame()

for (i in 1:length(content[[1]][[8]][[which(bookmakers$book == books[1])]][[4]][[1]][[2]])) {
  golfer_names_dk[i,1] <- content[[1]][[8]][[which(bookmakers$book == books[1])]][[4]][[1]][[2]][[i]]$name
  golfer_names_dk[i,2] <- content[[1]][[8]][[which(bookmakers$book == books[1])]][[4]][[1]][[2]][[i]]$price
}

### FD

golfer_names_fd <- data.frame()

for (i in 1:length(content[[1]][[8]][[which(bookmakers$book == books[2])]][[4]][[1]][[2]])) {
  golfer_names_fd[i,1] <- content[[1]][[8]][[which(bookmakers$book == books[2])]][[4]][[1]][[2]][[i]]$name
  golfer_names_fd[i,2] <- content[[1]][[8]][[which(bookmakers$book == books[2])]][[4]][[1]][[2]][[i]]$price
}

golfer_odds <- golfer_names_dk %>% 
  left_join(golfer_names_fd, by=c("V1"))

### FB

golfer_names_fb <- data.frame()

for (i in 1:length(content[[1]][[8]][[which(bookmakers$book == books[3])]][[4]][[1]][[2]])) {
  golfer_names_fb[i,1] <- content[[1]][[8]][[which(bookmakers$book == books[3])]][[4]][[1]][[2]][[i]]$name
  golfer_names_fb[i,2] <- content[[1]][[8]][[which(bookmakers$book == books[3])]][[4]][[1]][[2]][[i]]$price
}

golfer_odds <- golfer_odds %>% 
  left_join(golfer_names_fb, by=c("V1"))

### BS

golfer_names_bs <- data.frame()

for (i in 1:length(content[[1]][[8]][[which(bookmakers$book == books[4])]][[4]][[1]][[2]])) {
  golfer_names_bs[i,1] <- content[[1]][[8]][[which(bookmakers$book == books[4])]][[4]][[1]][[2]][[i]]$name
  golfer_names_bs[i,2] <- content[[1]][[8]][[which(bookmakers$book == books[4])]][[4]][[1]][[2]][[i]]$price
}

golfer_odds <- golfer_odds %>% 
  left_join(golfer_names_bs, by=c("V1"))

### TS

golfer_names_ts <- data.frame()

for (i in 1:length(content[[1]][[8]][[which(bookmakers$book == books[5])]][[4]][[1]][[2]])) {
  golfer_names_ts[i,1] <- content[[1]][[8]][[which(bookmakers$book == books[5])]][[4]][[1]][[2]][[i]]$name
  golfer_names_ts[i,2] <- content[[1]][[8]][[which(bookmakers$book == books[5])]][[4]][[1]][[2]][[i]]$price
}

golfer_odds <- golfer_odds %>% 
  left_join(golfer_names_ts, by=c("V1"))

### UB

golfer_names_ub <- data.frame()

for (i in 1:length(content[[1]][[8]][[which(bookmakers$book == books[6])]][[4]][[1]][[2]])) {
  golfer_names_ub[i,1] <- content[[1]][[8]][[which(bookmakers$book == books[6])]][[4]][[1]][[2]][[i]]$name
  golfer_names_ub[i,2] <- content[[1]][[8]][[which(bookmakers$book == books[6])]][[4]][[1]][[2]][[i]]$price
}

golfer_odds <- golfer_odds %>% 
  left_join(golfer_names_ub, by=c("V1"))

### BF

golfer_names_bf <- data.frame()

for (i in 1:length(content[[1]][[8]][[which(bookmakers$book == books[7])]][[4]][[1]][[2]])) {
  golfer_names_bf[i,1] <- content[[1]][[8]][[which(bookmakers$book == books[7])]][[4]][[1]][[2]][[i]]$name
  golfer_names_bf[i,2] <- content[[1]][[8]][[which(bookmakers$book == books[7])]][[4]][[1]][[2]][[i]]$price
}

golfer_odds <- golfer_odds %>% 
  left_join(golfer_names_bf, by=c("V1"))

### Odds Columns ###

for (i in 1:dim(golfer_odds)[1]) {
  golfer_odds$best_odds[i] <- max(golfer_odds[i,2:8], na.rm = T)
}

golfer_odds$mean_odds <- round(rowMeans(golfer_odds[,2:8], na.rm = T), digits = -1)
golfer_odds$delta <- golfer_odds$best_odds - golfer_odds$mean_odds

for (i in 1:dim(golfer_odds)[1]) {
  golfer_odds$sd[i] <- round(sd(golfer_odds[i,2:7], na.rm = T), digits = -1)
}

### Getting the Date & Time ###

date_time <- gsub("Z","",unlist(strsplit(content[[1]][[8]][[1]]$last_update,"T")))
time <- format(as.POSIXct(strptime(paste(date_time[1], date_time[2]), format = "%Y-%m-%d %H:%M:%S") - 14400), format = "%H:%M:%S")

### Table ###

rm(list = ls(pattern = "golfer_names_"))
names(golfer_odds)[1:8] <- c("Golfer", "Draftkings", "FanDuel", "FoxBET", "Barstool", "TwinSpires", "Unibet", "BetFair")
view(unique(golfer_odds), title = paste(time, "Golfer Odds"))
