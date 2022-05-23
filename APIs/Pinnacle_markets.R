library(httr)

url <- "https://pinnacle-odds.p.rapidapi.com/kit/v1/markets"

queryString <- list(
  sport_id = "9",
  event_type = "prematch",
  is_have_odds = "true"
)

response <- VERB("GET", url, add_headers('X-RapidAPI-Host' = 'pinnacle-odds.p.rapidapi.com', 'X-RapidAPI-Key' = 'b34680a5eamsh5c39b8cf066dd0ap1e8d93jsnc9e5097b3c8c'), query = queryString, content_type("application/octet-stream"))

content <- content(response, "parsed")
