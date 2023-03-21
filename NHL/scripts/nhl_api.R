#get nhl odds via lso api

library(httr)
library(tidyverse)
library(glue)
library(rlist)
library(pipeR)

key <- "icehockey_nhl_championship_winner"
url <- glue("https://odds.p.rapidapi.com/v4/sports/{key}/odds")

queryString <- list(
  regions="us", oddsFormat="american", dateFormat="iso"
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

which(sapply(content, function(x) "" %in% x))

list.search(content, "draftkings" %in% .)

match("draftkings",content[[1]][[8]])

lapply(content[[1]][[8]], function(x) mean(x))
