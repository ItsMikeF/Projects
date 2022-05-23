library(httr)

url <- "https://odds.p.rapidapi.com/v4/sports/mma_mixed_martial_arts/odds"

queryString <- list(
  regions = "us",
  oddsFormat = "american",
  markets = "h2h,spreads",
  dateFormat = "iso"
)

response <- VERB("GET", url, add_headers('X-RapidAPI-Host' = 'odds.p.rapidapi.com',
                                         'X-RapidAPI-Key' = 'b34680a5eamsh5c39b8cf066dd0ap1e8d93jsnc9e5097b3c8c'),
                 query = queryString, content_type("application/octet-stream"))

content <- content(response, "parse")

most_odds <- data.frame()
for (i in 1:length(content)) {
  most_odds[i,1] <- length(content[[i]][[7]])
}

### Get Odds
for (j in 1:length(content)) {
  for (i in 1:min(most_odds)) {
    df[1 + (2*(j-1)),1 + (i)] <- content[[j]][[7]][[i]][[4]][[1]][[2]][[1]]$price
    df[2 + (2*(j-1)),1 + (i)] <- content[[j]][[7]][[i]][[4]][[1]][[2]][[2]]$price
  }
}