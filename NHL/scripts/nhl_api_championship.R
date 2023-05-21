#get nhl odds via lso api

# Load packages
suppressMessages({
  library(httr)
  library(dplyr)
  library(purrr)
  library(glue)
  library(rlist)
})


# 1.0 Red the LSO API -----------------------------------------------------

# Define the key and URL
key <- "icehockey_nhl_championship_winner"
url <- glue("https://odds.p.rapidapi.com/v4/sports/{key}/odds")

# Define list of parameters for the API request
queryString <- list(
  regions="us",
  oddsFormat="american",
  dateFormat="iso"
)

# Perform HTTP GET request, includes necessary headers and content type
response <- VERB("GET", 
                 url,
                 add_headers('X-RapidAPI-Host' = 'odds.p.rapidapi.com',
                             'X-RapidAPI-Key' = 'b34680a5eamsh5c39b8cf066dd0ap1e8d93jsnc9e5097b3c8c'),
                 query = queryString, content_type("application/octet-stream"))

# Extract the content of the response and parsed it into an R object
content <- content(response, "parsed")

# 2.0 Make odds table -----------------------------------------------------

# Load American odds converter function
convert_ML <- function(odds) {
  breakeven <- if_else(odds > 0, 100 / (100 + odds), abs(odds) / (abs(odds) + 100))
  return(round(breakeven, digits = 4))
}

# Calculate the lengths of the nested elements once and store it in variables
len_bookmakers <- length(content[[1]]$bookmakers)
len_outcomes <- length(content[[1]]$bookmakers[[1]]$markets[[1]]$outcomes)

# Create bookmakers dataframe for futures
bookmakers <- data.frame()

for (i in 1:len_bookmakers) {
  for (j in 1:len_outcomes) {
    bookmakers[i,1] <- content[[1]]$bookmakers[[i]]$key
    
    bookmakers[i,2*j] <- content[[1]]$bookmakers[[i]]$markets[[1]]$outcomes[[j]]$name
    bookmakers[i,2*j+1] <- content[[1]]$bookmakers[[i]]$markets[[1]]$outcomes[[j]]$price
    
  }
}

# Pre allocate memory space
bookmakers_cols <- vector(mode = "character", length = len_outcomes*2)

for (h in seq_len(len_outcomes)) {
  bookmakers_cols[2*h - 1] <- paste0("team",h)
  bookmakers_cols[2*h] <- glue("team{h}_odds")
}

bookmakers_cols <- c("bookmaker", bookmakers_cols)
colnames(bookmakers) <- bookmakers_cols

bookmakers <- bookmakers %>% 
  mutate(hold = 
           ((convert_ML(team1_odds) + convert_ML(team2_odds) +
           convert_ML(team3_odds) + convert_ML(team4_odds)
           )-1)*100
         )

view(bookmakers)

# 3.0 Code Suggestions ---------------------------------------------------


# Code improvement suggestion from GPT4
# Preallocate the bookmakers matrix
bookmakers <- matrix(nrow = len_bookmakers, ncol = 2*len_outcomes + 1)

# Populate the bookmakers matrix
for (i in seq_len(len_bookmakers)) {
  bookmakers[i, 1] <- content[[1]]$bookmakers[[i]]$key
  
  for (j in seq_len(len_outcomes)) {
    idx <- 2*j # Calculate index once
    bookmakers[i, idx] <- content[[1]]$bookmakers[[i]]$markets[[1]]$outcomes[[j]]$name
    bookmakers[i, idx+1] <- content[[1]]$bookmakers[[i]]$markets[[1]]$outcomes[[j]]$price
  }
}


#purrr suggestions
# Load the required library
library(purrr)

# Create a function to map over outcomes
outcome_func <- function(outcome) {
  c(outcome$name, outcome$price)
}

# Create a function to map over bookmakers
bookmaker_func <- function(bookmaker) {
  c(bookmaker$key, flatten_chr(map(bookmaker$markets[[1]]$outcomes, outcome_func)))
}

# Apply the map function to content
bookmakers <- map(content[[1]]$bookmakers, bookmaker_func)
