# live sports odds 

# Load necessary libraries
library(httr)
library(jsonlite)

# Function to get live sports odds
get_live_sports_odds <- function() {
  # Your API key
  api_key <- "114ea856077620641393f6fcf173cf11"
  
  # Base URL for the API (replace with the actual API endpoint)
  url <- "https://api.the-odds-api.com"  # Replace with your API URL
  
  # Make the GET request with the API key in the headers
  response <- GET(url, add_headers("Authorization" = paste("bearer", api_key)))
  
  # Check if the request was successful
  if (status_code(response) == 200) {
    # Parse the JSON content
    content <- content(response, "text")
    data <- fromJSON(content)
    
    # Return the data
    return(data)
    
  } else {
    # Print error message if request failed
    print(paste("Failed to retrieve data. Status code:", status_code(response)))
    return(NULL)
  }
}

# Call the function to get live sports odds
odds_data <- get_live_sports_odds()

# Print the retrieved data
print(odds_data)
