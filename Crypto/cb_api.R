#lets read the coinbase api
#https://docs.cloud.coinbase.com/exchange/reference/exchangerestapi_getcurrencies-1

#load packages
suppressMessages({
  library(tidyverse)
  library(httr)
  library(jsonlite)
})

#api info
api_url <- "https://api.pro.coinbase.com/"
api_key <- "32238cfdd0d5f253df7563c558369239"
api_secret <- "bfSKL67rAIbm8ya2kdLGb0iBD6tm4ss/CE8G1I6c/zD08edDHQLgYfEyj8nn7CzbXU9Tk4CavjcPmZ6yFJxKzg=="
api_passphrase <- "jo0geecqg"

#copied code, status 200!
url <- "https://api.exchange.coinbase.com/currencies"

response <- VERB("GET", url, content_type("application/octet-stream"), accept("application/json"))

content <- content(response, "parse")

data <- fromJSON(content, flatten = TRUE)

#work with the data
df <- list()

for (i in 1:length(content)) {
  df[i] <- content[[i]][[1]]
}
cryptos <- as.data.frame(unlist(df))
