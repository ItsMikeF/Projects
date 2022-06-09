#load packages
library(tidyverse)
library(httr)

#inputs
url <- "https://odds.p.rapidapi.com/v4/sports"

queryString <- list(all = "true")

#api response
response <- VERB("GET", url, add_headers('X-RapidAPI-Host' = 'odds.p.rapidapi.com', 
                                         'X-RapidAPI-Key' = 'b34680a5eamsh5c39b8cf066dd0ap1e8d93jsnc9e5097b3c8c'), 
                 query = queryString, content_type("application/octet-stream"))

content <- content(response, "parsed")

#create tibble with all sports keys 
sports_keys <- tibble()

for (i in 1:length(content)) {
  for (j in 1:length(content[[1]])) {
    sports_keys[i,j] <- content[[i]][j]
  }
}

#view all keys
view(sports_keys, title = "Sports Keys")