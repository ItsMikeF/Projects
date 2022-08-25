#lets analyze the pff cfb preview magazie

#load packages
suppressMessages({
  library(tidyverse) #ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, forcats
  library(pdftools) #text extraction, rendering, and converting of pdf documents
  library(tidytext) #Text Mining using 'dplyr', 'ggplot2', and Other Tidy Tools
})

files <- list.files(pattern = ".pdf")
keywords <- c("QB", "RB", "WR", "EDGE", "Head Coach")

cfb_mag <- pdf_text(pdf = "2022-23 College Football Preview.pdf")
length(cfb_mag)
head(cfb_mag[10])

rows <- scan(textConnection(cfb_mag),what = "character", sep = "\n")

test <- function(i) {
  print(cfb_mag[i])
}
sapply(6, test)

file_length <- length(files)
word_length <- length(keywords)

word_count <- seq(1,file_length*word_length)
dim(word_count) <- c(file_length, word_length)

cfb_mag <- pdf_text(pdf = "2022-23 College Football Preview.pdf") %>% 
  str_replace_all("\n", " ") %>% 
  str_trim()

for (i in 1:length(keywords)) {
  word_count[1,i] <- cfb_mag %>% 
    str_count(keywords[i]) %>% 
    sum()
}

cfb_mag %>% 
  unnest_tokens(cfb_mag, keywords)
