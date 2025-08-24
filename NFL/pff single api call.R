# Load required libraries
suppressPackageStartupMessages({
  library(tidyverse)
  library(httr)
  library(jsonlite)
  library(dplyr)
  library(readr)
  library(glue)
})

# Function to parse cookies from browser Cookie header
parse_cookies <- function(cookie_string) {
  if (is.null(cookie_string) || cookie_string == "") {
    stop("Cookie string is empty or NULL")
  }
  
  # Split by semicolons to get individual cookies
  cookies <- strsplit(cookie_string, ";\\s*")[[1]]
  
  # Parse each cookie
  cookie_pairs <- lapply(cookies, function(cookie) {
    if (grepl("=", cookie, fixed = TRUE)) {
      parts <- strsplit(cookie, "=", fixed = TRUE)[[1]]
      name <- parts[1]
      value <- paste(parts[-1], collapse = "=")
      return(c(name = value))
    }
    return(NULL)
  })
  
  # Combine into named vector
  cookie_pairs <- cookie_pairs[!sapply(cookie_pairs, is.null)]
  cookies_vector <- do.call(c, cookie_pairs)
  
  return(cookies_vector)
}

# Manually paste your Cookie header here (replace with your actual cookies from browser Network tab)
# Example cookie string (update with your own)
cookie_header <- "_ga=GA1.1.1234567890.1698765432; _gid=GA1.1.9876543210.1698765432; _fbp=fb.1.1234567890.1698765432; _premium_key=SFMyNTY.g3QAAAABbQAAABZndWFyZGlhbl9kZWZhdWx0X3Rva2VubQAA...; c_groot_access_token=eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9...; c_groot_access_ts=2025-08-23T22:22:00Z"
cookies_pff <- parse_cookies(cookie_header)

# Read the team abbreviation file (for a single team ID)
cfbd_to_pff_abbrev_df <- read.csv("./01_data/reference/cfbd_to_pff_abbrev.csv")
# Select a single team ID (e.g., Air Force with franchise_id 101)
team_id <- 101  # Replace with the desired team_id from your CSV

# Function to fetch data for a single team, week, and year
fetch_pff_data <- function(team_id, week, year) {
  # Construct the URL using glue
  url <- glue("https://premium.pff.com/api/v1/facet/receiving/summary?league=ncaa&season={year}&franchise_id={team_id}&week={week}&division=fbs")
  
  # Make the request
  response <- GET(url, set_cookies(cookies_pff))
  
  # Check if request is successful
  if (status_code(response) == 200) {
    # Parse JSON response
    json_data <- fromJSON(content(response, "text", encoding = "UTF-8"))
    
    # Check if receiving_summary exists and has data
    if (!is.null(json_data$receiving_summary) && length(json_data$receiving_summary) > 0) {
      # Extract and normalize data
      df <- as.data.frame(json_data$receiving_summary)
      df$week <- week
      df$year <- year
      df$team_id <- team_id
      
      # Print progress
      cat("Fetched data for year:", year, "week:", week, "team_id:", team_id, "✓", nrow(df), "rows\n")
      
      return(df)
    } else {
      cat("No data in receiving_summary for year:", year, "week:", week, "team_id:", team_id, "∅\n")
      return(data.frame())
    }
  } else {
    cat("Failed request for year:", year, "week:", week, "team_id:", team_id, "✗ Status:", status_code(response), "\n")
    return(data.frame())
  }
}

# Define parameters for the single call
year <- 2024    # Adjust to the desired season
week <- 6       # Adjust to the desired week

# Make the single API call
master_df <- fetch_pff_data(team_id, week, year)

# Save as RDS file
if (nrow(master_df) > 0) {
  filename <- glue("pff_receiving_data_{year}_week_{week}_team_{team_id}_{format(Sys.time(), '%Y%m%d_%H%M%S')}.rds")
  saveRDS(master_df, file = filename)
  cat("Data saved to:", filename, "\n")
} else {
  cat("No data to save. Check cookies, team_id, or parameters.\n")
}

# Display the resulting data frame
print(master_df)