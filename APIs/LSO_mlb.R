library(httr)
library(tidyverse)

sport <- "baseball_mlb"
url <- paste0("https://odds.p.rapidapi.com/v4/sports/",sport,"/odds")

queryString <- list(
  regions = "us",
  oddsFormat = "american",
  dateFormat = "iso"
)

response <- VERB("GET", url, add_headers('X-RapidAPI-Host' = 'odds.p.rapidapi.com', 
                                         'X-RapidAPI-Key' = 'b34680a5eamsh5c39b8cf066dd0ap1e8d93jsnc9e5097b3c8c'), 
                 query = queryString, content_type("application/octet-stream"))

content <- content(response, "parsed")

### Get Books

bookmakers <- data.frame()
for (i in 1:length(content[[1]][[length(content[[1]])]])) {
  bookmakers[i,1] <- content[[1]][[length(content[[1]])]][[i]]$title
}

### Get Teams

df <- data.frame()
for (i in 1:length(content)) {
  df[1 + (2*(i-1)),1] <- content[[i]]$away_team
  df[2 + (2*(i-1)),1] <- content[[i]]$home_team
}

### Find game with most books

most_odds <- data.frame()
for (i in 1:length(content)) {
  most_odds[i,1] <- length(content[[i]][[7]])
}

content[[min(which(most_odds == max(most_odds)))]]

most_books <- data.frame()
for (i in 1:length(content[[min(which(most_odds == max(most_odds)))]][[7]])) {
  most_books[i,1] <- content[[min(which(most_odds == max(most_odds)))]][[7]][[i]]$key
}

### Find element with least books 

min_books <- data.frame()
for (i in 1:length(content[[min(which(most_odds == min(most_odds)))]][[7]])) {
  min_books[i,1] <- content[[min(which(most_odds == min(most_odds)))]][[7]][[i]]$key
}

min(min_odds$V1)

### Get Odds
for (j in 1:length(content)) {
  for (i in 1:min(most_odds)) {
    df[1 + (2*(j-1)),1 + (i)] <- content[[j]][[7]][[i]][[4]][[1]][[2]][[1]]$price
    df[2 + (2*(j-1)),1 + (i)] <- content[[j]][[7]][[i]][[4]][[1]][[2]][[2]]$price
  }
}

names(df)[2:(length(min_books$V1)+1)] <- min_books$V1

{
### Odds Columns ###

for (i in 1:dim(df)[1]) {
  df$best_odds[i] <- max(df[i,2:(dim(df)[2]-4)], na.rm = T)
}

df$mean_odds <- round(rowMeans(df[,2:dim(df)[2]], na.rm = T), digits = -1)
df$delta <- df$best_odds - df$mean_odds

for (i in 1:dim(df)[1]) {
  df$sd[i] <- round(sd(df[i,2:dim(df)[2]], na.rm = T), digits = -1)
}

### Getting the Date & Time ###

date_time <- gsub("Z","",unlist(strsplit(content[[1]][[7]][[1]]$last_update,"T")))
time <- format(as.POSIXct(strptime(paste(date_time[1], date_time[2]), format = "%Y-%m-%d %H:%M:%S") - 14400), format = "%H:%M:%S")
}

view(df, title = paste(time, "MLB"))

df %>% select(V1, mean_odds) %>% view(title = "Mean Odds")
