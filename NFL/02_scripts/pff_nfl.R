# Load required libraries
suppressPackageStartupMessages({
  library(tidyverse)
  library(httr)
  library(jsonlite)
  library(dplyr)
  library(readr)
  library(glue)
})

# Read the team abbreviation file (assumed to contain NFL team IDs; update if necessary)
cfbd_to_pff_abbrev_df <- read.csv("./01_data/reference/cfbd_to_pff_abbrev.csv") # Replace with NFL team ID file if needed
idlist <- cfbd_to_pff_abbrev_df$pff_team_id

# Define cookies as a named list
cookies_pff <- list(
  `_fbp` = "fb.1.1661369695596.714656027",
  `userty.core.p.f30f6b` = "__2VySWQiOiI0ZjA2YzdlMzQ2YTFhNTkyNWYzM2MwZTM2NGZjZTc3NiJ9eyJ1c",
  `prism_651055355` = "abbac514-b698-441b-bd8c-fc13375b34dc",
  `_hp2_id.714504125` = "%7B%22userId%22%3A%222561932635602237%22%2C%22pageviewId%22%3A%225598103829199161%22%2C%22sessionId%22%3A%228533597249350167%22%2C%22identity%22%3Anull%2C%22trackerVersion%22%3A%224.0%22%7D; _gcl_au=1.1.860069148.1685914249",
  `c_groot_access_token` = "Lh6sgNQ8BKHdvSMuAfiYmWzgR7Y6cZLvo1595R6S3GcYqCjn200UH-L40K0CoW2I",
  `c_groot_access_ts` = "2023-08-10T20:12:19Z",
  `c_groot_refresh_token` = "mv3N03KVl4Q2rcFqk-sLQPzCCgSiWDQUOpHyaIGxe_ILdHOZp0zCXzEMVxFmzN4K",
  `_gid` = "GA1.2.1424382025.1688324204; _clck=th1j2l|2|fcy|0|1230",
  `_hp2_ses_props.2100373990` = "%7B%22ts%22%3A1688324203597%2C%22d%22%3A%22www.pff.com%22%2C%22h%22%3A%22%2F%22%7D",
  `_premium_key` = "SFMyNTY.g3QAAAABbQAAABZndWFyZGlhbl9kZWZhdWx0X3Rva2VubQAAAmhleUpoYkdjaU9pSklVelV4TWlJc0luUjVjQ0k2SWtwWFZDSjkuZXlKaGRXUWlPaUpRY21WdGFYVnRJaXdpWlhod0lqb3hOelUxT0RJek1qUTBMQ0pwWVhRaU9qRTNOVFU0TWpFME5EUXNJbWx6Y3lJNklsQnlaVzFwZFcwaUxDSnFkR2tpT2lJMFpqZ3dORE16TWkxbU5tTmxMVFJqTnpNdE9XTXhPQzB4WXpjeU1tTXhPVFkxTjJVaUxDSnVZbVlpT2pFM05UVTRNakUwTkRNc0luQmxiU0k2ZXlKaFlXWWlPakVzSW01allXRWlPakVzSW01bWJDSTZNU3dpZFdac0lqb3hmU3dpYzNWaUlqb2llMXdpWlcxaGFXeGNJanBjSW0xcFkyaGhaV3hmYWw5bWNtRnVZMmx6UUc5MWRHeHZiMnN1WTI5dFhDSXNYQ0ptWldGMGRYSmxjMXdpT2x0ZExGd2labWx5YzNSZmJtRnRaVndpT201MWJHd3NYQ0JzWVhOMFgyNWhiV1VjSWpwdWRXeHNMRndpZFdsa1hDSTZYQ0pqWXpkaFptTTFNeTFpWlRVekxUUmpabU10WVRReE1TMDNNREUyWVRNMFpqRmhPRFZjSWl4Y0luWmxjblJwWTJGc1hDSTZYQ0pEYjI1emRXMWxjbHdpZlNJc0luUjVjQ0k2SW1GalkyVnpjeUo9LjhvbnBJc2FsNWh5YlYzekhIaW9vc0lkaTRDYkZIN0lwaVg5RmdxZkxNUGItLUUwSDVXZ0tUUGE0Vkg1WFp3UGVpRGtVaDdqVEk5blJEdWQ0bVZvYnZB.hQOhKlJzOlsVj4TT-COZEr0yI4ZJWgkNZFmRRNTSjFQ",
  `_ga` = "GA1.1.1182956122.1661369695",
  `_dc_gtm_UA-21858063-1` = "1",
  `_ga_5VBG8Q786X` = "GS1.2.1688324203.5.1.1688324908.60.0.0",
  `_clsk` = "m3bwpr|1688324910475|9|1|v.clarity.ms/collect",
  `_ga_8Y6RN784SW` = "GS1.1.1688324203.309.1.1688324961.7.0.0"
)

# Initialize an empty data frame to store the results
master_df <- data.frame()

# Function to fetch data for a given team_id, week, and year
fetch_pff_data <- function(team_id, week, year) {
  # Construct the URL using glue
  url <- glue("https://premium.pff.com/api/v1/facet/position/summary?league=nfl&season={year}&franchise_id={team_id}&week={week}&position=qb")
  
  # Make the request
  response <- GET(url, set_cookies(.cookies = cookies_pff))
  
  # Check if request is successful
  if (status_code(response) == 200) {
    # Parse JSON response
    json_data <- fromJSON(content(response, "text", encoding = "UTF-8"))
    
    # Extract and normalize data
    df <- as.data.frame(json_data$position_summary)
    df$week <- week
    df$year <- year
    df$team_id <- team_id
    
    # Print progress
    cat(year, week, team_id, "\n")
    try({
      cat(nrow(df), "rows retrieved\n")
    }, silent = TRUE)
    
    return(df)
  } else {
    cat("Failed request for year:", year, "week:", week, "team_id:", team_id, "Status:", status_code(response), "\n")
    return(data.frame()) # Return empty data frame if request fails
  }
}

# Use lapply to iterate over weeks and team IDs
weeks <- 6 # Define weeks (can be a vector like c(1, 2, 3) for multiple weeks)
year <- 2024 # Define the year

results <- lapply(weeks, function(week) {
  # Iterate over team IDs for the current week
  team_results <- lapply(idlist, function(team_id) {
    fetch_pff_data(team_id, week, year)
  })
  # Combine results for the current week
  bind_rows(team_results)
})

# Combine all results into master_df
master_df <- bind_rows(results)

# Display the master data frame
print(master_df)
