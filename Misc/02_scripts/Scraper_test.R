library(XML)
library(RCurl)
library(data.table)
library(tictoc)
library(tidyverse)
library(lubridate)
library(stats)
library(seqminer)
library(rvest)

url <- 'http://www.imdb.com/search/title?count=100&release_date=2016,2016&title_type=feature'
webpage <- read_html(url)

#Using CSS selectors to scrape the rankings section
rank_data_html <- html_nodes(webpage,'.text-primary')

#Converting the ranking data to text
rank_data <- html_text(rank_data_html)

#Let's have a look at the rankings
head(rank_data)

#Data-Preprocessing: Converting rankings to numerical
rank_data<-as.numeric(rank_data)

#Let's have another look at the rankings
head(rank_data)

#Using CSS selectors to scrape the title section
title_data_html <- html_nodes(webpage,'.lister-item-header a')

#Converting the title data to text
title_data <- html_text(title_data_html)

#Let's have a look at the title
head(title_data)

#Using CSS selectors to scrape the description section
description_data_html <- html_nodes(webpage,'.ratings-bar+ .text-muted')

#Converting the description data to text
description_data <- html_text(description_data_html)

#Let's have a look at the description data
head(description_data)

###

url <- "https://www.ncaa.com/news/basketball-men/article/2020-05-06/2017-ncaa-tournament-bracket-scores-stats-records?utm_campaign=links-bundle-asset"

link.scrap <- htmlParse(url)
link.scrap <- readHTMLList(url)

all.list.items <- sapply(getNodeSet(doc = link.scrap, path = "//li"), xmlValue)

url <- "https://www.ncaa.com/history/basketball-men/d1"

x <- as.data.frame(readHTMLTable(getURL(url))[[1]])

link.scrap <- htmlParse(url)
link.scrap <- htmlTable::htmlTable(url)
link.scrap <- readHTMLList(url)

all.list.items <- sapply(getNodeSet(doc = link.scrap, path = "//li"), xmlValue)


###Script

library(XML)

# --------
# Read the web page
# --------
link <- "http://www.adirondacklakessurvey.org/alscrpt.inc.php?alscpond=020225B&pname=ALLEGANY%20BROOK%20POND"
# NOTE: is ethical to store the page and not read it unnecessarily too many times, 
# overloading their server
link.scrap <- htmlParse(link)

# --------
# Read all list items from the page
# --------
all.list.items <- sapply(getNodeSet(doc = link.scrap, path = "//li"), xmlValue)
all.list.items[1:5]
## [1] "Location/General"
## [2] "Pond Name: ALLEGANY BROOK POND"
## [3] "Pond #: 020225B"
## [4] "Town: Black Brook"
## [5] "County: Clinton"

# --------
# Read a particular list item by using its XPath selector
# --------
pond.name.node <- getNodeSet(doc  = link.scrap, 
                             path = '//*[@id="historic_report_location"]/ul/ul/li[1]')
pond.name.node
## [[1]]
## <li style="list-style-type:none">Pond Name: ALLEGANY BROOK POND</li> 
## attr(,"class")
## [1] "XMLNodeSet"
pond.name <- xmlValue(pond.name.node[[1]])
pond.name
## [1] "Pond Name: ALLEGANY BROOK POND"

# This is the same as:
all.list.items[2]
## [1] "Pond Name: ALLEGANY BROOK POND"

# --------
# Select multiple list item via their XPath selectors
# --------
# Use a vector of XPath selectors in path argument of xpathApply().
# Note the usage of xpathApply() together with a call to xmlValue() function.
# This is an alternative to using getNodeSet() + xmlValue() above
items.lst <- xpathApply(doc  = link.scrap, 
                        path = c('//*[@id="historic_report_location"]/ul/ul/li[1]',
                                 '//*[@id="historic_report_location"]/ul/ul/li[3]'),
                        fun  = xmlValue,
                        trim = TRUE)
items.lst
## [[1]]
##  [1] "Pond Name: ALLEGANY BROOK POND"
## [[2]]
##  [1] "Town: Black Brook"

# Transform to a character vector
items.chr.vect <- unlist(items.lst)
items.chr.vect
## [1] "Pond Name: ALLEGANY BROOK POND" "Town: Black Brook"

# Or transform to a data.frame object
items.df <- as.data.frame(items.lst, 
                          col.names = c("Pond Name", "Town"),
                          stringsAsFactors = FALSE)
items.df
##                        Pond.Name              Town
## 1 Pond Name: ALLEGANY BROOK POND Town: Black Brook