library(tidyverse)
library(httr)

url <- "https://odds.p.rapidapi.com/v4/sports"

queryString <- list(all = "true")

response <- VERB("GET", url, add_headers('X-RapidAPI-Host' = 'odds.p.rapidapi.com', 
                                         'X-RapidAPI-Key' = 'b34680a5eamsh5c39b8cf066dd0ap1e8d93jsnc9e5097b3c8c'), 
                 query = queryString, content_type("application/octet-stream"))

content <- content(response, "parsed")

sports_keys <- tibble()

for (i in 1:length(content)) {
  for (j in 1:length(content[[1]])) {
    sports_keys[i,j] <- content[[i]][j]
  }
}

view(sports_keys, title = "Sports Keys")