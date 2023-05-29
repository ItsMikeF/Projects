#lets analyze the pff cfb preview magazie

#load packages
suppressMessages({
  library(tidyverse) #ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, forcats
  library(pdftools) #text extraction, rendering, and converting of pdf documents
  library(tidytext) #Text Mining using 'dplyr', 'ggplot2', and Other Tidy Tools
  library(cfbfastR) #access cfb pbp data
  library(wordcloud) #word clouds
  library(tidytext) #text mining using dplyr, ggplot2, and other tidy tools
  library(gt) #easily create presentation ready display tables
})

files <- list.files(pattern = ".pdf", path = "./data/")
keywords <- c("QB", "RB", "WR", "EDGE", "BREAKOUT PLAYER", "Head Coach")

cfb_mag <- pdf_text(pdf = "./data/2022-23 College Football Preview.pdf")
length(cfb_mag)
head(cfb_mag[11])

rows <- scan(textConnection(cfb_mag),what = "character", sep = "\n")

test <- function(i) {
  print(cfb_mag[i])
}
sapply(6, test)

file_length <- length(files)
word_length <- length(keywords)

word_count <- seq(1,file_length*word_length)
dim(word_count) <- c(file_length, word_length)

cfb_mag <- pdf_text(pdf = "./data/2022-23 College Football Preview.pdf") %>% 
  str_replace_all("\n", " ") %>% 
  str_trim()

for (i in 1:length(keywords)) {
  word_count[1,i] <- cfb_mag %>% 
    str_count(keywords[i]) %>% 
    sum()
}

#find all break out players
rows1 <- as.data.frame(rows)
data1 <- rows[str_detect(rows, "BREAKOUT PLAYER")]
data1 <- which(rows == "BREAKOUT PLAYER")
data1 <- data1 + 1

breakout <- trimws(rows[data1])
bo1 <- trimws(rows[data1+1])
bo2 <- trimws(rows[data1+2])
bo3 <- trimws(rows[data1+3])
bo4 <- trimws(rows[data1+4])
bo5 <- trimws(rows[data1+5])
bo6 <- trimws(rows[data1+6])
bo7 <- trimws(rows[data1+7])
bo8 <- trimws(rows[data1+8])
bo9 <- trimws(rows[data1+9])

bo <- as.data.frame(cbind(breakout, paste0(bo1, bo2, bo3, bo4, bo5, bo6, bo7, bo8)))
bo %>% 
  gt()

#now find their teams
data2 <- which(rows == "  - ")

cfb_mag_list <- as.list(cfb_mag)
cfb_mag_list[[6]]

#find the offensive player spotlight
data3 <- which(rows == "OFFENSIVE PLAYER SPOTLIGHT")

highlight <- trimws(rows[data3+1])
highlight_desc <- trimws(rows[data3+2])
highlight_desc2 <- trimws(rows[data3+3])
highlight_desc3 <- trimws(rows[data3+4])
highlight_desc4 <- trimws(rows[data3+5])
highlight_desc5 <- trimws(rows[data3+6])
highlight_desc6 <- trimws(rows[data3+7])
highlight_desc7 <- trimws(rows[data3+8])
highlight_desc8 <- trimws(rows[data3+9])

spotlight <- as.data.frame(cbind(highlight, paste(highlight_desc,highlight_desc2, highlight_desc3, highlight_desc4, highlight_desc5, highlight_desc6, highlight_desc7, highlight_desc8)))

spotlight %>% 
  gt()

#define the teams
cfb_teams <- cfbd_team_info()
cfb_teams_merge <- cfb_teams %>% 
  select(school, mascot, abbreviation, alt_name2) %>% 
  mutate(school_mascot = paste(school, mascot)) %>% 
  select(school_mascot)

#find the teams
class(cfb_mag_list[[6]][1])
str(cfb_mag_list[[6]][1])
seq(6,286,by=4)

#find most common words
words <- tm::tm_map(rows, tm::removePunctuation)
head(stop_words("english", lexicon="onix"),n=10)

#look at returning production
cfbd_player_returning <- cfbd_player_returning(year = 2022, team = "Florida State")
cfbd_player_returning
