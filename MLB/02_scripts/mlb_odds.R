# Get nhl odds via lso api

# Load packages
suppressMessages({
  library(httr)
  library(tidyverse)
  library(glue)
  library(rlist)
  library(pipeR)
})


# 1.0 Read LSO API --------------------------------------------------------


# Write key and url
key <- "baseball_mlb"
url <- glue("https://odds.p.rapidapi.com/v4/sports/{key}/odds")

# Define list of parameters for the API request
queryString <- list(
  regions="us", 
  oddsFormat="american", 
  dateFormat="iso"
)

# Perform get request with headers and content type
response <- VERB("GET", 
                 url, 
                 add_headers('X-RapidAPI-Host' = 'odds.p.rapidapi.com', 
                             'X-RapidAPI-Key' = 'b34680a5eamsh5c39b8cf066dd0ap1e8d93jsnc9e5097b3c8c'),
                 query = queryString, content_type("application/octet-stream"))

# Extract the content from the response and parse it into an R object
content <- content(response, "parsed")


# 2.0 Create odds table ---------------------------------------------------


#Add Functions
convert_ML <- function(odds) {
  breakeven <- if_else(odds > 0, 100 / (100 + odds), abs(odds) / (abs(odds) + 100))
  return(round(breakeven, digits = 4))
}

#gpt suggested code
df_list <- lapply(content, function(x) {
  data.frame(
    book = sapply(x$bookmakers, function(y) y$title),
    away_team = sapply(x$bookmakers, function(y) y$markets[[1]]$outcomes[[1]]$name),
    away_odds_ml = sapply(x$bookmakers, function(y) y$markets[[1]]$outcomes[[1]]$price),
    away_odds_imp = sapply(x$bookmakers, function(y) convert_ML(y$markets[[1]]$outcomes[[1]]$price)),
    home_team = sapply(x$bookmakers, function(y) y$markets[[1]]$outcomes[[2]]$name),
    home_odds_ml = sapply(x$bookmakers, function(y) y$markets[[1]]$outcomes[[2]]$price),
    home_odds_imp = sapply(x$bookmakers, function(y) convert_ML(y$markets[[1]]$outcomes[[2]]$price)),
    last_update = sapply(x$bookmakers, function(y) y$last_update)
  ) %>%
    mutate(hold = (away_odds_imp + home_odds_imp - 1) * 100)
})

df <- bind_rows(df_list)
view(df)

df %>% 
  group_by(away_team) %>% 
  summarise(avg_odds = mean(away_odds_ml), 
            min = min(away_odds_ml), 
            max = max(away_odds_ml))

df %>% 
  group_by(home_team) %>% 
  summarise(avg_odds = mean(home_odds_ml), 
            min = min(home_odds_ml), 
            max = max(home_odds_ml))


# 3.0 Convert to Don Best Format ------------------------------------------


# Load bookmaers for column names
bookmakers <- read.csv("./data/bookmakers.csv", header = F)

# Create empty list
df_list2 <- list()

# Transpose the data under unified row names so we can rowbind
for (i in 1:length(df_list)) {
  test <- bookmakers %>% left_join(df_list[[i]], by=c("V1"="book"))
  test2 <- as_tibble(t(test))
  test2 <- test2 %>% 
    mutate(team = V1[2]) %>% 
    relocate(team) 
  
  # add away team name
  test2$team[1] = "team"
  test2$team[6] = test2$V1[5]
  
  # Remove a unnecessary rows
  test2 <- test2 %>% filter(!row_number() %in% c(2,4,5,7,8,9))
  
  df_list2[[i]] <- test2
}

# Combine list of tibbles
combined_df <- do.call(rbind, df_list2)

# Define column names
new <- combined_df[1,]

# Apply column names
colnames(combined_df) <- new

# Remove duplicate rows
#combined_df <- distinct(combined_df)
combined_df <- combined_df %>% slice(-c(seq(from = 1, to = n(), by = 3)))
view(combined_df)

