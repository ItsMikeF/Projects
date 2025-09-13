library(httr)
library(jsonlite)

# Configuration
api_key <- Sys.getenv("LSO_KEY")  # Replace with your Odds API key
base_url <- "https://api.the-odds-api.com/v4"
endpoint <- "/sports/"

# Construct the request URL with API key
request_url <- paste0(base_url, endpoint, "?apiKey=", api_key)

# Make the GET request
response <- GET(request_url)

# Check the status code
if (status_code(response) == 200) {
  # Extract usage headers
  requests_used <- headers(response)$`x-requests-used`
  requests_remaining <- headers(response)$`x-requests-remaining`
  requests_last <- headers(response)$`x-requests-last`
  
  # Print usage information
  cat("Requests Used (this month):", requests_used, "\n")
  cat("Requests Remaining (this month):", requests_remaining, "\n")
  cat("Requests Cost of Last Call:", requests_last, "\n")
  
  # Optional: Parse and print the response content (list of sports)
  content <- content(response, as = "parsed", type = "application/json")
  cat("\nSports Data (sample):\n")
  print(head(content, 2))  # Display first two sports for brevity
} else if (status_code(response) == 429) {
  cat("Error: Rate limit exceeded (429). Try spacing out requests.\n")
} else {
  cat("Error:", status_code(response), "-", content(response, as = "text"), "\n")
}
