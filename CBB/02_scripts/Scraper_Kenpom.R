# kenpom scraper for summary table

# load packages
suppressMessages({
  library(rvest)
  library(RCurl)
  library(dplyr)
  library(data.table)
  library(openxlsx)
  library(glue)
})

# kenpom years
years <- 2024:2024 # start at 2002

for(year in years) {
  cat("Getting", year,"\n")
  ### Pull Data
  url <- paste0("https://kenpom.com/index.php?y=", year)
  page <- read_html(url)
  tables <- page %>% html_nodes("table") %>% html_table()
  x <- as.data.frame(tables)
  
  ### Clean Data
  names(x) <- c("rank", "team", "conference", "record", "adj_em", "adj_o", 
                "adj_o_rank", "adj_d", "adj_d_rank", "adj_tempo", "adj_tempo_rank", 
                "luck", "luck_rank", "sos_adj_em", "sos_adj_em_rank", "sos_adj_o",
                "sos_adj_o_rank","sos_adj_d", "sos_adj_d_rank", "nc_sos_adj_em", 
                "nc_sos_adj_em_rank")
  
  x <- filter(x, !team %in% c("", "Team"))
  
  for(i in 5:ncol(x)) {
    x[,i] <- as.numeric(as.character(x[,i]))
  }
  
  x <- mutate(x, 
              "ncaa_seed" = sapply(team, function(arg) { as.numeric(gsub("[^0-9]", "", arg)) }),
              "team" = sapply(team, function(arg) { gsub("\\s[0-9]+", "", arg) }),
              "year" = year)
  
  ### Store Data
  if(year == 2002) {
    kenpom <- x
  }else {
    kenpom <- rbind(kenpom, x)
  }
}

####Clean Team Names so that they can be merged to NCAA data
# Replacing Southern with Southen Univ forces recorrecting TX Southern & Miss Southern
kenpom[,team:=gsub("\\.","",team)]
kenpom[,team:=gsub("Cal St","CS",team)]
kenpom[,team:=gsub("Albany","SUNY Albany",team)]
kenpom[,team:=gsub("Abilene Christian","Abilene Chr",team)]
kenpom[,team:=gsub("American","American Univ",team)]
kenpom[,team:=gsub("Arkansas Little Rock","Ark Little Rock",team)]
kenpom[,team:=gsub("Arkansas Pine Bluff","Ark Pine Bluff",team)]
kenpom[,team:=gsub("Boston University","Boston Univ",team)]
kenpom[,team:=gsub("Central Michigan","C Michigan",team)]
kenpom[,team:=gsub("Central Connecticut","Central Conn",team)]
kenpom[,team:=gsub("Coastal Carolina","Coastal Car",team)]
kenpom[,team:=gsub("East Carolina","E Kentucky",team)]
kenpom[,team:=gsub("Eastern Washington","E Washington",team)]
kenpom[,team:=gsub("East Tennessee St","ETSU",team)]
kenpom[,team:=gsub("Fairleigh Dickinson","F Dickinson",team)]
kenpom[,team:=gsub("Florida Atlantic","FL Atlantic",team)]
kenpom[,team:=gsub("Florida Gulf Coast","FL Gulf Coast",team)]
kenpom[,team:=gsub("George Washington","G Washington",team)]
kenpom[,team:=gsub("Illinois Chicago","IL Chicago",team)]
kenpom[,team:=gsub("Kent St","Kent",team)]
kenpom[,team:=gsub("Monmouth","Monmouth NJ",team)]
kenpom[,team:=gsub("Mississippi Valley St","MS Valley St",team)]
kenpom[,team:=gsub("Mount St Mary's","Mt St Mary's",team)]
kenpom[,team:=gsub("Montana St","MTSU",team)]
kenpom[,team:=gsub("Northern Colorado","N Colorado",team)]
kenpom[,team:=gsub("North Dakota St","N Dakota St",team)]
kenpom[,team:=gsub("Northern Kentucky","N Kentucky",team)]
kenpom[,team:=gsub("North Carolina A&T","NC A&T",team)]
kenpom[,team:=gsub("North Carolina Central","NC Central",team)]
kenpom[,team:=gsub("North Carolina St","NC State",team)]
kenpom[,team:=gsub("Northwestern St","Northwestern LA",team)]
kenpom[,team:=gsub("Prairie View A&M","Prairie View",team)]
kenpom[,team:=gsub("South Carolina St","S Carolina St",team)]
kenpom[,team:=gsub("South Dakota St","S Dakota St",team)]
kenpom[,team:=gsub("Southern Illinois","S Illinois",team)]
kenpom[,team:=gsub("Southeastern Louisiana","SE Louisiana",team)]
kenpom[,team:=gsub("Stephen F Austin","SF Austin",team)]
kenpom[,team:=gsub("Southern","Southern Univ",team)]
kenpom[,team:=gsub("Southern Univ Miss","Southern Miss",team)]
kenpom[,team:=gsub("Saint Joseph's","St Joseph's PA",team)]
kenpom[,team:=gsub("Saint Louis","St Louis",team)]
kenpom[,team:=gsub("Saint Mary's","St Mary's CA",team)]
kenpom[,team:=gsub("Saint Peter's","St Peter's",team)]
kenpom[,team:=gsub("Texas A&M Corpus Chris","TAM C. Christi",team)]
kenpom[,team:=gsub("Troy St","Troy",team)]
kenpom[,team:=gsub("Texas Southern Univ","TX Southern",team)]
kenpom[,team:=gsub("Louisiana Lafayette","Louisiana",team)]
kenpom[,team:=gsub("UTSA","UT San Antonio",team)]
kenpom[,team:=gsub("Western Michigan","W Michigan",team)]
kenpom[,team:=gsub("Green Bay","WI Green Bay",team)]
kenpom[,team:=gsub("Milwaukee","WI Milwaukee",team)]
kenpom[,team:=gsub("Western Kentucky","WKU",team)]
kenpom[,team:=gsub("College of Charleston","Col Charleston",team)]
kenpom[,team:=gsub("Loyola Chicago","Loyola-Chicago",team)]

###### Validate match by all team-year combinations ####
#Load teams & merge to seeds for all tourney combinations
teams <- fread("../input/ncaam-march-mania-2021/MDataFiles_Stage2/MTeams.csv")
tourney_seeds <- fread("../input/ncaam-march-mania-2021/MDataFiles_Stage2/MNCAATourneySeeds.csv")
teamyr <- merge(teams,tourney_seeds,by="TeamID")
teamyr

#Merge Team-Years to Kenpom and check for any missing seeds
teamKenpom <- merge(teamyr,kenpom,by.x=c("TeamName","Season"),by.y=c("TeamName","year"),all.x = T)

#Should be zero length data table (meaning no unmatched NCAA tournament teams) or display unmatched team-year combinations
teamKenpom[Season>=2002 & is.na(Seed),.(TeamName,Season)]

#write out file
write.xlsx(kenpom, file = glue("./03_outputs/kenpom_summary.xlsx"))
