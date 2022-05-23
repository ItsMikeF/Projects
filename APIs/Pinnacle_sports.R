library(httr)

url <- "https://pinnacle-odds.p.rapidapi.com/kit/v1/sports"

pinnacle_sports <- VERB("GET", url, add_headers('X-RapidAPI-Host' = 'pinnacle-odds.p.rapidapi.com', 
                                         'X-RapidAPI-Key' = 'b34680a5eamsh5c39b8cf066dd0ap1e8d93jsnc9e5097b3c8c'), 
                 content_type("application/octet-stream"))

pinnacle_sports <- content(pinnacle_sports, "parsed")

pinnacle_sports_names <- data.frame()

for (i in 1:length(pinnacle_sports)) {
  pinnacle_sports_names[i,1] <- pinnacle_sports[[i]]$name
}

view(pinnacle_sports_names, title = "Pinnacle Sports")
