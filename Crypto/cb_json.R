rm(list = ls())  # reset global variables
#https://www.cryptodatadownload.com/blog/posts/use-R-to-download-coinbase-api-price-data/

#import the libraries we need
library(jsonlite) #Convert R objects to/from JSON
library(glue) #Format and interpolate a string

# create a function to retrieve daily data
retreive_daily_data <- function(pair, filename) {
  url = glue("https://api.pro.coinbase.com/products/{pair}/candles?granularity=86400")
  columnNames <- c('unix', 'low', 'high', 'open', 'close', glue('{pair} volume'))
  mydata <- fromJSON(url)
  df <- as.data.frame(mydata)
  colnames(df) <- columnNames  # rename the columns
  write.csv(df, file = filename)
}

newPair <- "BTC-USD"
fileName <- glue("dailyData{newPair}.csv")
runFunc <- retreive_daily_data(newPair, filename = fileName)
runFunc
