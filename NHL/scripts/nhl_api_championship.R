#get nhl odds via lso api

# Load packages
library(httr)
library(tidyverse)
library(glue)
library(rlist)
#library(pipeR)

# Key and URL
key <- "icehockey_nhl_championship_winner"
url <- glue("https://odds.p.rapidapi.com/v4/sports/{key}/odds")

# Define list of parameters for the API request
queryString <- list(
  regions="us",
  oddsFormat="american",
  dateFormat="iso"
)

# Perform HTTP GET request, includes necessary headers and content type
response <- VERB("GET", url, add_headers('X-RapidAPI-Host' = 'odds.p.rapidapi.com', 
                                         'X-RapidAPI-Key' = 'b34680a5eamsh5c39b8cf066dd0ap1e8d93jsnc9e5097b3c8c'),
                 query = queryString, content_type("application/octet-stream"))

# Extract the content of the response and parsed it into an R object
content <- content(response, "parsed")

bookmakers <- data.frame()

for (i in 1:length(content[[1]]$bookmakers)) {
  for (j in 1:5) {
    bookmakers[i,1] <- content[[1]]$bookmakers[[i]]$key
    
    bookmakers[i,2*j] <- content[[1]]$bookmakers[[i]]$markets[[1]]$outcomes[[j]]$name
    bookmakers[i,2*j+1] <- content[[1]]$bookmakers[[i]]$markets[[1]]$outcomes[[j]]$price
    
  }
}
length(content[[1]]$bookmakers[[i]]$markets[[1]]$outcomes)


which(sapply(content, function(x) "" %in% x))

list.search(content, "draftkings" %in% .)

match("draftkings",content[[1]][[8]])

lapply(content[[1]][[8]], function(x) mean(x))

