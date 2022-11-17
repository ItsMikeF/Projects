#lets play some cfb dfs

# 0.0 Load required packages ----------------------------------------------

#load packages
suppressMessages({
  library(tidyverse) #ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, forcats
  library(glue) #interpreted string literals
  library(rvest) #easily harvest (scrape) web pages
  library(httr) #tools for working with URLs and HTTP
  library(cfbfastR) #access cfb pbp data
  library(ggimage) #use image in ggplot2
  library(gt) #easiyl create presentation ready display tables
})

week <- 11

# 1.0 Scrape DraftKings odds ----------------------------------------------

dk_scraper <- function(url) {
  webpage <- read_html(url)
  
  css1 <- ".event-cell__name-text" #teams
  css2 <- ".no-label .sportsbook-outcome-cell__line" #lines
  css3 <- "span+ .sportsbook-outcome-cell__line" #totals
  
  teams <- html_text(html_elements(webpage, css1))
  lines <- html_text(html_elements(webpage, css2)) %>% as.numeric()
  totals<- html_text(html_elements(webpage, css3)) %>% as.numeric()
  
  dk_odds <<- as.data.frame(cbind(teams, lines, totals)) %>% 
    mutate(lines = as.numeric(lines),
           totals = as.numeric(totals))
  
  time = Sys.time() %>% as.character() %>% str_replace_all(.,":","") %>% substr(1,15)
  write.csv(dk_odds, file = glue("./contests/2022_w{week}/odds/{time}_cfb_dk_odds.csv"))
}

dk_scraper("https://sportsbook.draftkings.com/leagues/football/ncaaf")

# 1.1 Odds change ---------------------------------------------------------

list.files(path = glue("./contests/2022_w{week}/odds/"))
a <- length(list.files(path = glue("./contests/2022_w{week}/odds/")))

odds_delta <- read.csv(paste0(glue("./contests/2022_w{week}/odds/"),list.files(path = glue("./contests/2022_w{week}/odds/"))[1])) %>% 
  left_join(read.csv(paste0(glue("./contests/2022_w{week}/odds/"),list.files(path = glue("./contests/2022_w{week}/odds/"))[2])), by=c("teams")) %>% 
  mutate(diff = lines.y-lines.x)

# 2.0 Scrape cfb injuries -------------------------------------------------

injury_report <- function(url) {
  #toggle this on and off for testing
  #url <- "https://www.covers.com/sport/football/ncaaf/injuries"
  
  css1 <- ".covers-CoversMatchups-Table a" #players names
  webpage <- read_html(url)
  
  players <- html_text(html_elements(webpage, css1)) %>% as.character() %>% 
    str_replace_all("[\r\n]" , "")
  players <- gsub(" ","",players)
  players <- players[-c(which(players == ""))]
  
  css2 <- "b" #player injury status
  status <- html_text(html_elements(webpage, css2)) %>% as.character()
  
  injuries <<- as_tibble(cbind(players, status))
}

injury_report("https://www.covers.com/sport/football/ncaaf/injuries")

# 3.0 Slate eda -----------------------------------------------------------

slate <- function(week) {
  dks <<- read.csv(glue("./contests/2022_w{week}/DKSalaries.csv")) %>% 
    left_join(read.csv("./data/cfb_schools.csv"), by=c("TeamAbbrev"="dk_abbrev")) %>% 
    left_join(read_csv(paste0(glue("./contests/2022_w{week}/"), list.files(pattern = "projections_draftkings_cfb_", path = glue("./contests/2022_w{week}")))), by=c("Name"="name"))
  
  #determine what schools are on the slate and combine with cfb schools file
  slate_schools <- as_tibble(unique(dks$TeamAbbrev)) %>% 
    rename("dk_abbrev"="value") %>% 
    left_join(read.csv("./data/cfb_schools.csv"), by=c("dk_abbrev")) %>% 
    select(dk_abbrev, draftkings)
  
  #remove games from next week
  dk_odds <- dk_odds[c(1:72),]
  
  #filter dk_odds to the slate
  dk_odds <- dk_odds[which(dk_odds$teams %in% slate_schools$draftkings),]
  
  #add dk odds
  dks <- dks %>% 
    left_join(dk_odds, by=c("draftkings"="teams")) %>% 
    mutate(inj_name = paste(substr(Name,1,1),str_extract(Name, '[^ ]+$'),sep = ".")) %>% 
    left_join(injuries, by=c("inj_name"="players")) %>% 
    mutate(status = replace_na(status, "Healthy")) %>%
    separate(Game.Info, c("Away", "String"), sep = "@") %>%
    separate(String, c("Home", "Date", "Time"), sep = " ") 
  
  dks$opponent <- if_else(dks$Home == dks$TeamAbbrev, dks$Away, dks$Home)
  
  dks <<- dks %>% 
    left_join(read.csv("./data/cfb_schools.csv"), by=c("opponent"="dk_abbrev"))
  
  #write how many games on the slate
  print(glue("{length(unique(dks$TeamAbbrev))/2} game slate"))
}

slate(week)

# 4.0 Defenses ------------------------------------------------------------

defense <- function(week) {
  #load pff def files
  def <- read.csv(glue("./contests/2022_w{week}/pff/defense_summary.csv"))
  
  #sort pff def
  def <<- def %>% 
    group_by(team_name) %>% 
    summarise(
      def = round(weighted.mean(grades_defense, snap_counts_defense, na.rm = T), digits = 2),
      rdef = round(weighted.mean(grades_run_defense, snap_counts_run_defense, na.rm = T), digits = 2),
      tack = round(weighted.mean(grades_tackle, snap_counts_defense, na.rm = T), digits = 2),
      prsh = round(weighted.mean(grades_pass_rush_defense, snap_counts_defense, na.rm = T), digits = 2),
      cov = round(weighted.mean(grades_coverage_defense, snap_counts_coverage, na.rm = T), digits = 2)
    ) %>% 
    mutate(def_rank = round(rank(-def), digits = 0), 
           def_sd = round((def - mean(def)) / sd(def, na.rm = T), digits = 2),
           
           rdef_rank = round(rank(-rdef), digits = 0), 
           rdef_sd = round((rdef - mean(rdef)) / sd(rdef, na.rm = T), digits = 2),
           
           tack_rank = round(rank(-tack), digits = 0), 
           tack_sd = round((tack - mean(tack)) / sd(tack, na.rm = T), digits = 2),
           
           prsh_rank = round(rank(-prsh), digits = 0), 
           prsh_sd = round((prsh - mean(prsh)) / sd(prsh, na.rm = T), digits = 2),
           
           cov_rank = round(rank(-cov), digits = 0),
           cov_sd = round((cov - mean(cov)) / sd(cov, na.rm = T), digits = 2))
}

defense(week)

#normalize pff defense grade table
normalize <- function(x){
  return( (x - min(x,na.rm = T))/( max(x, na.rm = T) - min(x, na.rm = T)) )
}

#for(i in 2:length(def)){
 # def[,i] = round(normalize(def[,i]), digits = 3)*100
#}

#add defense data to the dks
dks <- dks %>% 
  left_join(def, by=c("School.y"="team_name"))

# 4.2 offense team table --------------------------------------------------

offense <- function(week){
  
  off <<- off %>% 
    group_by(team_name) %>% 
    summarise(
      off = round(weighted.mean(grades_offense, snap_counts_offense, na.rm = T), digits = 2),
      pass = round(weighted.mean(grades_pass, snap_counts_pass, na.rm = T), digits = 2),
      run = round(weighted.mean(grades_run, snap_counts_run, na.rm = T), digits = 2)
    )
}

# 5.0 Qbs  ----------------------------------------------------------------

qbs <- dks %>% filter(Position=="QB") %>% 
  left_join(read.csv(glue("./contests/2022_w{week}/pff/passing_summary.csv")), 
                          by=c("Name"="player"))

qbs_select <- qbs %>% 
  filter(Salary > min(Salary)) %>% 
  mutate(btt_twp = round(btt_rate/twp_rate, digits = 1),
         ttt_run_p2s = round(avg_time_to_throw*grades_run/pressure_to_sack_rate, digits = 1), 
         yards_game = round(yards/player_game_count, digits = 1), 
         att_game = round(attempts/player_game_count, digits = 1)) %>% 
  select(Name, TeamAbbrev, Salary, fpts, proj_own, status, lines, totals, 
         grades_pass, att_game, yards_game, 
         opponent, def_rank, rdef_rank, prsh_rank, cov_rank, 
         btt_twp, avg_depth_of_target,  
         ttt_run_p2s, avg_time_to_throw, grades_run, pressure_to_sack_rate) %>% 
  drop_na(fpts) %>% 
  arrange(-proj_own) %>% 
  view(title = "QBs")

write.csv(qbs_select, file = glue("./contests/2022_w{week}/pos/qbs.csv"))

# 6.0 Rbs -----------------------------------------------------------------

rbs <- dks %>% filter(Position=="RB") %>% 
  left_join(read.csv(glue("./contests/2022_w{week}/pff/rushing_summary.csv")), 
            by=c("Name"="player"))

rbs_select <- rbs %>% 
  filter(Salary > min(Salary)) %>% 
  mutate(#mtf_per_att = round(elu_rush_mtf/attempts, digits = 2),
    breakaway_attempts = round(breakaway_attempts/attempts),
    explosive = round(explosive/attempts, digits = 1),
    elu_yco = round(elu_yco/attempts, digits = 1),
    first_downs = round(first_downs/attempts,  digits = 1),
    attempts = round(attempts/player_game_count,  digits = 1),
    designed_yards = round(designed_yards/player_game_count, digits = 1),
    rec_yards = round(rec_yards/player_game_count, digits = 1),
    targets = round(targets/player_game_count, digits = 1),
    total_touches = round(total_touches/player_game_count, digits = 1)) %>% 
  select(Name, TeamAbbrev, Salary, fpts, proj_own, status, lines, totals, 
         grades_offense, grades_run, total_touches, 
         opponent, def_rank, rdef_rank, tack_rank, 
         designed_yards, elusive_rating, breakaway_attempts, explosive, elu_yco, first_downs, attempts,
         designed_yards, rec_yards, targets, player_game_count) %>% 
  #drop_na() %>% 
  arrange(-proj_own) %>%
  view(title = "RBs")

write.csv(rbs_select, file = glue("./contests/2022_w{week}/pos/rbs.csv"))

# 7.0 Wrs -----------------------------------------------------------------

wrs <- dks %>% filter(Position=="WR") %>% 
  left_join(read.csv(glue("./contests/2022_w{week}/pff/receiving_summary.csv")), 
            by=c("Name"="player"))

wrs_select <- wrs %>% 
  select(Name, TeamAbbrev, Salary, fpts, proj_own, status, lines, totals, 
         pass_plays, grades_offense, yprr, 
         opponent, def_rank, cov_rank,) %>% 
  arrange(-Salary) %>% 
  #drop_na() %>% 
  arrange(-proj_own) %>%
  view(title = "WRs")

write.csv(wrs_select, file = glue("./contests/2022_w{week}/pos/wrs.csv"))
