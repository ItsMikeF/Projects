#web scrape depth charts from espn
#made via https://stackoverflow.com/questions/56739863/web-scraping-image-url-for-a-series-of-events-in-espn-play-by-play


#load packages
suppressMessages({
  library(dplyr) #ggplot2 dplyr tidyr readr stringr forcats purrr tibble
  library(purrr)
  library(stringr)
  library(nflverse) #nflfastr nflseedr nfl4th nflreadr nflplotr
  library(rvest) #easily harvest (scrape) web pages
  library(janitor) #simple little tools for examining and cleaning dirty data
  library(ggrepel) #automatically position non-overlapping text labels with ggplot2
})

# 1.0 scrape and clean depth charts -------------------------------------------------

espn_depth_chart_scraper <- function() {
  
  library(nflverse) #nflfastr nflseedr nfl4th nflreadr nflplotr
  library(rvest) #easily harvest (scrape) web pages
  
  #css tags to be used
  css1 <- ".fw-medium .AnchorLink" #starters
  css2 <- ".Table__TD:nth-child(2) .AnchorLink" #second string
  css3 <- ".Table--fixed-left span" #positions
  #css4 <- ".Table__TD span , .Table__TD .AnchorLink" #all text
  
  #write css tags to a vector
  css <- c(css1, css2, css3)
  #css <- c(css4)
  
  #nfl team abbrev table
  teams <- teams_colors_logos %>% 
    select(team_abbr, team_name) %>% 
    filter(!team_abbr %in% c("LA", "OAK", "SD", "STL"))
  
  #replace spaces with dashes
  teams$team_name <- gsub(" ", "-", teams$team_name)
  
  #fix Washington names
  teams$team_abbr[32] <- "WSH"
  teams$team_name[32] <- "Washington-Commanders"
  
  #purrr map every offensive roster into a list
  i <- 1:32
  
  depth_off <- i %>% 
    map(function (i) {
      print(teams$team_abbr[i])
      url <- paste0("https://www.espn.com/nfl/team/depth/_/name/",teams$team_abbr[i],"/",teams$team_name[i])
      
      #scrape webpage with rvest
      webpage <- read_html(url)
      
      depth_chart <- css %>% 
        map(function (css){
          html <- html_nodes(webpage,css)
          text <- trimws(html_text(html)) %>% as_tibble()
        })
      
      # Get length of the position list element
      len <- length(depth_chart[[3]][[1]])
      
      # Extract the positions
      positions <- depth_chart[[3]][[1]][seq(2, len, 2)]
      positions <- tibble(positions[1:12])
      
      # Extract the first string players
      off1 <- depth_chart[[1]][c(1:12),]
      
      # Extract second string players
      off2 <- depth_chart[[2]][c(1:12),]
      
      # column bind all
      cbind(positions, off1, off2, teams$team_abbr[i])
    } )
  
  depth_off <- setNames(depth_off, teams$team_abbr)
  
  # Create a data frame of the rosters from the list
  nfl_depth <- bind_rows(depth_off)
  
  # Assign column names
  names(nfl_depth) <- c("pos", "first_string", "second_string","team")
  
  # Remove FBs and blanks
  nfl_depth <- nfl_depth %>% filter(!(pos %in% c("", "FB")))
  
  # test code to get wr3s
  test <- nfl_depth %>% filter(pos == "WR")
  test <- test[seq(3,96,3),]
  
  # create a second dataframe
  nfl_depth2 <- nfl_depth %>% select(pos, second_string, team) 
  
  # Extract pos values and assign depth chart numbers
  pos2 <- paste0(nfl_depth2$pos[1:11],2)
  
  pos2[3] <- "WR4"
  pos2[4] <- "WR5"
  pos2[5] <- "WR6"
  nfl_depth2$pos <- pos2
  
  # Extract pos values and assign depth chart numbers
  pos <- paste0(nfl_depth$pos[1:11],1)
  pos[4] <- "WR2"
  pos[5] <- "WR3"
  
  nfl_depth$pos <- rep(pos, 32)
  
  nfl_depth_full <<- bind_rows(nfl_depth %>% select(pos, first_string, team) %>% rename(player = first_string), 
                              nfl_depth2 %>% rename(player = second_string))
  
  nfl_depth_full %>% filter(team == "ARI") %>% arrange(pos)
  
  
}
espn_depth_chart_scraper()

# save the R object
save(nfl_depth_full, file = "./01_data/depth_chart/espn_depth_chart_2025.Rdata")

# gpt5 code
espn_depth_chart_scraper <- function(save_path = NULL, assign_global = TRUE, progress = TRUE) {
  suppressPackageStartupMessages({
    library(dplyr)
    library(purrr)
    library(stringr)
    library(rvest)
    library(tidyr)
    library(nflreadr)
    library(httr)
    library(xml2)
    library(progress)
  })
  
  # ---------- helpers ----------
  make_espn_slug <- function(team_name) {
    team_name |>
      str_replace_all("[^A-Za-z0-9 ]", "") |>
      str_squish() |>
      str_replace_all(" ", "-") |>
      (\(s) ifelse(s == "Washington", "Washington-Commanders", s))()
  }
  
  read_html_retry <- function(url, tries = 3, sleep = 1) {
    ua <- httr::user_agent("nfl-depth-chart-scraper (contact: you@example.com)")
    for (i in seq_len(tries)) {
      resp <- tryCatch({ xml2::read_html(httr::GET(url, ua)) }, error = function(e) NULL)
      if (!is.null(resp)) return(resp)
      Sys.sleep(sleep)
    }
    stop("Failed to read: ", url)
  }
  
  parse_team_depth <- function(team_abbr, espn_slug) {
    url <- paste0("https://www.espn.com/nfl/team/depth/_/name/", team_abbr, "/", espn_slug)
    page <- read_html_retry(url)
    
    starters <- page %>% html_elements(".fw-medium .AnchorLink") %>% html_text2()
    second   <- page %>% html_elements(".Table__TD:nth-child(2) .AnchorLink") %>% html_text2()
    pos_raw  <- page %>% html_elements(".Table--fixed-left span") %>% html_text2()
    
    keep_pos <- c("QB","RB","WR","TE","LT","LG","C","RG","RT","FB","OL")
    positions <- pos_raw[pos_raw %in% keep_pos]
    
    n <- min(length(starters), length(second), length(positions))
    if (n == 0) return(tibble())
    
    tibble(
      team = toupper(team_abbr),
      pos  = positions[seq_len(n)],
      first_string  = starters[seq_len(n)],
      second_string = second[seq_len(n)]
    ) %>% filter(pos %in% keep_pos)
  }
  
  # ---------- team list (current) ----------
  teams <- nflreadr::load_teams() %>%
    select(team_abbr, team_name) %>%
    distinct() %>%
    mutate(
      team_abbr = if_else(team_abbr %in% c("WAS", "WSH"), "WSH", team_abbr),  # ESPN uses WSH
      team_abbr = if_else(team_abbr == "LA", "LAR", team_abbr),               # ESPN uses LAR (not LA)
      team_abbr_espn = tolower(team_abbr),
      espn_slug = make_espn_slug(team_name)
    ) %>%
    arrange(team_abbr)
  
  # ---------- scrape with progress ----------
  start_time <- Sys.time()
  if (progress) {
    pb <- progress_bar$new(
      format = "Scraping ESPN depth charts [:bar] :percent ETA: :eta",
      total = nrow(teams), clear = FALSE, width = 70
    )
  }
  
  # Keep results aligned in a single list-column
  scrape <- vector("list", nrow(teams))
  for (i in seq_len(nrow(teams))) {
    scrape[[i]] <- safely(parse_team_depth)(teams$team_abbr_espn[i], teams$espn_slug[i])
    if (progress) pb$tick()
  }
  
  elapsed_sec <- as.integer(difftime(Sys.time(), start_time, units = "secs"))
  message(sprintf("\nScraping completed in %d seconds.", elapsed_sec))
  
  scrape_tbl <- teams %>%
    mutate(scrape = scrape,
           result = map(scrape, "result"),
           error  = map(scrape, "error"))
  
  failed <- scrape_tbl %>% filter(!map_lgl(error, is.null)) %>% pull(team_abbr)
  if (length(failed) > 0) warning("Failed teams: ", paste0(failed, collapse = ", "))
  
  depth_raw <- scrape_tbl$result %>%
    discard(is.null) %>%
    list_rbind()
  
  if (nrow(depth_raw) == 0) stop("No depth data scraped — site structure may have changed.")
  
  # ---------- tidy to single long table ----------
  depth_long <- depth_raw %>%
    pivot_longer(c(first_string, second_string), names_to = "string", values_to = "player") %>%
    mutate(string = if_else(string == "first_string", 1L, 2L)) %>%
    arrange(team, pos, string)
  
  nfl_depth_full <- depth_long %>%
    group_by(team, pos) %>%
    mutate(depth = row_number()) %>%
    ungroup() %>%
    transmute(team, pos = paste0(pos, depth), player) %>%
    arrange(team, pos)
  
  if (!is.null(save_path)) {
    dir.create(dirname(save_path), recursive = TRUE, showWarnings = FALSE)
    save(nfl_depth_full, file = save_path)
  }
  
  if (assign_global) assign("nfl_depth_full", nfl_depth_full, envir = .GlobalEnv)
  
  nfl_depth_full
}

# --- Example ---
nfl_depth_full <- espn_depth_chart_scraper(
  save_path = "./01_data/depth_chart/espn_depth_chart_2025.Rdata",
  assign_global = TRUE
 )
