rm(list = ls())  # reset global variables
#https://www.cryptodatadownload.com/blog/posts/use-R-to-download-coinbase-api-price-data/

#import the libraries we need
suppressMessages({
  library(tidyverse) #ggplot2 dplyr readr stringr tidyr tibble purrr forcats
  library(jsonlite) #Convert R objects to/from JSON
  library(glue) #Format and interpolate a string
  library(lubridate) #make dealing with dates a little easier
})

# create a function to retrieve daily data
retreive_daily_data <- function(pair, filename) {
  url = glue("https://api.pro.coinbase.com/products/{pair}/candles?granularity=86400")
  mydata <- fromJSON(url)
  df <- as.data.frame(mydata)
  
  columnNames <- c('unix', 'low', 'high', 'open', 'close', glue('{pair} volume'))
  colnames(df) <- columnNames  # rename the columns
  
  write.csv(df, file = filename)
}

#define variables for function
newPair <- "BTC-USD"
fileName <- glue("dailyData{newPair}.csv")

#run function
runFunc <- retreive_daily_data(newPair, filename = fileName)

#read file 
cb <- read.csv("dailyDataBTC-USD.csv") %>% 
  mutate(unix = as_datetime(unix)) %>% 
  rename(date="unix")

cb %>% 
  ggplot(aes(x=date,y=close)) +
  geom_line() +
  #geom_smooth(se=F) +
  labs(
    title = "BTC-USD Price",
    caption = "Data from Coinbase"
  )

max(cb$close)
cb$close[1]
ath <- round((max(cb$close)-cb$close[1])*100/max(cb$close),digits = 1)
print(glue("Down {ath} percent from all time highs"))
