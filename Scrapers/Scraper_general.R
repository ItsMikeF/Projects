# load packages
suppressMessages({
  library(tidyverse)
  library(rvest) # Easily Harvest (Scrape) Web Pages
  library(XML) # Tools for Parsing and Generating XML Within R and S-Plus
  library(httr) # Tools for Working with URLs and HTTP
  library(RCurl) # General Network (HTTP/FTP/...) Client Interface for R
  library(data.table) # Extension of 'data.frame'
  
  library(RSelenium) #R Bindings for 'Selenium WebDriver'
  library(netstat) #Retrieve Network Statistics Including Available TCP Ports
})

rs_driver_object <- rsDriver(browser = "chrome", 
                             chromever = "103.0.5060.134", 
                             verbose = F, 
                             port = freeport())

# inputs
url <- "https://sportsbook.draftkings.com/leagues/soccer/england---premier-league?category=player-futures&subcategory=top-goalscorer"
#url <- "https://dataquestio.github.io/web-scraping-pages/simple.html"
css1 <- ".sportsbook-outcome-cell__label"
css2 <- ".default-color"

# read html page
webpage <- read_html(url)
webpage

webpage %>%
  html_nodes("div") %>%
  html_text()


#find nodes
webpage %>% 
  html_elements(css1)

#Read text nodes
words <- html_text(html_nodes(webpage,css1))

#Read numeric nodes
numbers <- as.numeric(html_text(html_nodes(webpage,css2)))

#Read HTML table
df <- as.data.frame(readHTMLTable(getURL(url)))

#Create data table
table <- data.frame(cbind(words, numbers))

#Option to Write csv
#write.csv(table, file = "table.csv")