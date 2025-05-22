# 1.0 Load Packages and Data ----------------------------------------------

suppressMessages({
  library(furrr)
  library(purrr)
  library(baseballr)
  library(mlbplotR)
  library(tidyverse)
  library(glue)
  library(progress)
  library(progressr)
})

# Define years 
years <- 2023:2025

# 2.0 Batters -------------------------------------------------------------

# 2.1 Define All Batter IDs -----------------------------------------------

# Get batter data for each year
batter_data_list <- lapply(years, function(year) {
  tryCatch({
    fg_batter_leaders(startseason = year, endseason = year, qual = "0") %>%
      mutate(year = year) # Add year to track source
  }, error = function(e) {
    message(glue("Error fetching batter leaders for year {year}: {e$message}"))
    return(NULL)
  })
})

# Remove NULL entries (failed years)
batter_data_list <- batter_data_list[!sapply(batter_data_list, is.null)]

# Combine data frames, filling missing columns with NA
batter_data <- do.call(rbind, c(batter_data_list, fill = TRUE))

# Check for missing data
if (nrow(batter_data) == 0) {
  stop("No batter data retrieved. Check API or internet connection.")
}

# Extract unique batter IDs, filter for players with at least 10 PA
batter_ids <- batter_data %>%
  select(PlayerName, playerid, Bats) %>%
  arrange(playerid) %>%
  distinct(playerid, .keep_all = TRUE)

batter_ids_vec <- as.vector(batter_ids$playerid)

# Log number of batters
message(glue("Retrieved {length(batter_ids_vec)} unique batter IDs after filtering for PA >= 10."))

# Save batter IDs for reference
save(batter_ids, file = "./01_data/batter_ids.Rdata")

#load 
load("./01_data/batter_ids.Rdata")


# 2.2 Game Logs -----------------------------------------------------------

# Define safe function to get batter game logs for a single year
safe_batter_logs_single_year <- function(batter_id, year) {
  tryCatch({
    data <- fg_batter_game_logs(batter_id, year)
    
    # Check if data is empty
    if (is.null(data) || nrow(data) == 0) {
      return(tibble(batter_id = batter_id, year = year, error = "No game logs available"))
    }
    
    data %>%
      select(PlayerName, playerid, Date, Team, Opp, season, Age, BatOrder, Pos,
             G, AB, H, `1B`, `2B`, `3B`, HR, R, RBI,
             BB, SO, SB, HBP, Pitches, Balls, Strikes,
             wOBA, `wRC+`, Events, EV, maxEV, LA, Barrels, HardHit) %>%
      mutate(
        home_away = as.integer(grepl("@", Opp)),
        home = if_else(home_away == 0, Team, Opp),
        away = if_else(home_away == 1, Team, Opp),
        home = gsub("@", "", home),
        away = gsub("@", "", away),
        game_id = paste(Date, away, home, sep = "_"),
        year = year(as.Date(Date)),
        month = month(as.Date(Date)),
        wOBA = round(wOBA, digits = 2),
        LA = round(LA, digits = 2),
        EV = round(EV, digits = 2),
        maxEV = round(maxEV, digits = 2),
        fpts = H * 3 + HR * 10 + BB * 3 + HBP * 3 + RBI * 2 + R * 2 + SB * 4
      ) %>%
      left_join(batter_ids %>% select(playerid, Bats), by = "playerid") %>%
      relocate(Bats, .after = "PlayerName") %>%
      relocate(fpts, .after = "Pos") %>%
      arrange(fpts) %>%
      mutate(batter_id = batter_id) # Add batter_id for tracking
  }, error = function(e) {
    return(tibble(batter_id = batter_id, year = year, error = e$message))
  })
}

# Wrapper function to handle multiple years for a single batter
safe_batter_logs <- function(batter_id, years) {
  # Map over years individually and combine results
  result <- map_df(years, ~ safe_batter_logs_single_year(batter_id, .x))
  
  # If no data was retrieved for any year, return a tibble with error info
  if (nrow(result) == 0 || all(!is.na(result$error))) {
    return(tibble(batter_id = batter_id, year = NA_integer_, error = "No game logs retrieved for any year"))
  }
  
  # Return only successful results (rows without errors)
  return(result %>% filter(is.na(error)))
}

# Define function to get all batter game logs with parallel processing
all_batter_game_logs <- function(batter_ids_vec, years, cores) {
  # Set up parallel processing
  plan(multisession, workers = cores)
  
  # Set up progress bar
  handlers(global = TRUE)
  handlers("progress")
  p <- progressor(along = batter_ids_vec)
  
  # Parallel map with progress bar
  result <- future_map_dfr(batter_ids_vec, function(batter_id) {
    p(glue("Processing batter ID {batter_id}"))
    safe_batter_logs(batter_id, years)
  }, .options = furrr_options(seed = TRUE))
  
  # Clean up parallel plan
  plan(sequential)
  
  # Separate successful results from errors
  errors <- result %>% filter(!is.na(error))
  success <- result %>% filter(is.na(error))
  
  # Log errors
  if (nrow(errors) > 0) {
    message(glue("{nrow(errors)} batter-year combinations had errors or no game logs. Check 'batter_game_log_errors.csv'."))
    write.csv(errors, "batter_game_log_errors.csv", row.names = FALSE)
  }
  
  return(success)
}

# Define cores
cores <- max(1, availableCores() - 2) # Ensure at least 1 core

# Call the function, save output, and time execution
system.time({
  batter_logs <- all_batter_game_logs(batter_ids_vec, years, cores)
})

batter_logs <- do.call(rbind, c(batter_data_list, fill = TRUE))

# Save the output
save(batter_logs, "batter_game_logs.Rdata")

