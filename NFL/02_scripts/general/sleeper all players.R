# read sleeper api

# Load necessary libraries
library(httr)
library(jsonlite)
library(glue)
library(purrr)
library(dplyr)
library(openxlsx)


url <- "https://api.sleeper.app/v1/players/nfl" # define endpoint URL
request <- GET(url) # request the api endpoint

content <- content(request, "parsed") # parse response
df <- do.call(rbind, lapply(content, as.data.frame))

content$'6786'$search_rank
content$'6786'$full_name
content$'6786'$position

# Modified safe extraction function with conditional check
safe_extract <- function(element, field) {
  if (is.null(element[[field]])) {
    return(NA)
  } else {
    return(element[[field]])
  }
}

# Apply the modified safe extraction to each sublist within 'content'
df <- map_df(content, ~data.frame(
  search_rank = safe_extract(.x, "search_rank"),
  full_name = safe_extract(.x, "full_name"),
  position = safe_extract(.x, "position"),
  stringsAsFactors = FALSE
)) %>% 
  arrange(search_rank)

write.xlsx(df, file = "./05_outputs/sleeper/rankings.xlsx")
