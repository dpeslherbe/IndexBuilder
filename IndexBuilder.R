
library(data.table)
library(tidyr)
library(quantmod)
library(plyr)
library(rvest)
library(stringr)
library(magrittr)
library(tidyverse)

#Working example

principal <- 5000
tickers <- c('RY-T', 'TD-T', 'BNS-T', 'BAM-A-T', 'BMO-T', 'CM-T', 'SLF-T', 'MFC-T')
marketcaptable <- c()
for (i in 1:length(tickers)) {
  url <- paste0('https://www.theglobeandmail.com/investing/markets/stocks/', tickers[i])
  page <- read_html(url)
  nodes <- page %>% html_nodes('.barchart-overview-field:nth-child(8) barchart-field')
  value <- nodes[1] %>% html_attr('value')
  name <- nodes[1] %>% html_attr('symbol')
  new.nodes <- page %>% html_nodes('#fBsZNE1qvBE4os .open-price-value') %>% html_text()
  price <- as.numeric(new.nodes)
  value <- as.numeric(gsub(',','', value))
  if(length(marketcaptable) != 0){
    marketcaptable <- rbind(marketcaptable, c(name, value, price))
  }
  if(length(marketcaptable) == 0){
    marketcaptable <- c(marketcaptable, name, value, price)
  }
  
}
marketcaptable <- data.frame(marketcaptable)
marketcaptable[,2] <- as.numeric(marketcaptable[,2])
marketcaptable[,3] <- as.numeric(marketcaptable[,3])
marketcaptable <- cbind(marketcaptable, as.numeric(c(marketcaptable[,2]/sum(marketcaptable[,2]))))
rownames(marketcaptable) <- tickers
colnames(marketcaptable) <- c('Ticker', 'Market Cap in M', 'Price at last Open', 'Capped Weight')
shares <- rep(0, (dim(marketcaptable)[1]))
value <- rep(0, (dim(marketcaptable)[1]))
for (i in 1:(dim(marketcaptable)[1])) {
  ticker.principal <- principal*marketcaptable$`Capped Weight`[i]
  shares[i] <- ticker.principal/marketcaptable$`Price at last Open`[i]
  shares[i] <- floor(shares[i])
  value[i] <- shares[i]*marketcaptable$`Price at last Open`[i]
}
marketcaptable <- cbind(marketcaptable, shares, value)
marketcaptable <-  cbind(marketcaptable, c(marketcaptable$value/sum(marketcaptable$value)))
colnames(marketcaptable) <- c('Ticker', 'Market Cap in M', 'Price at last Open', 'Capped Weight', 'Shares to own', 'Value of shares', 'Actual Weight')
print(marketcaptable)

## Function building
## Principal is a numeric value of total amount to be invested
## Tickers is a list of stock tickers in the form XX-T or XXX-T or XXX-UN-T
## Note this function is defined for TSX listings only
## TSX Venture stocks should be noted as XX-X or XXX-X

IndexBuilder <- function(principal, tickers){
  marketcaptable <- c()
  for (i in 1:length(tickers)) {
    url <- paste0('https://www.theglobeandmail.com/investing/markets/stocks/', tickers[i])
    page <- read_html(url)
    nodes <- page %>% html_nodes('.barchart-overview-field:nth-child(8) barchart-field')
    value <- nodes[1] %>% html_attr('value')
    name <- nodes[1] %>% html_attr('symbol')
    new.nodes <- page %>% html_nodes('#fBsZNE1qvBE4os .open-price-value') %>% html_text()
    price <- as.numeric(new.nodes)
    value <- as.numeric(gsub(',','', value))
    if(length(marketcaptable) != 0){
      marketcaptable <- rbind(marketcaptable, c(name, value, price))
    }
    if(length(marketcaptable) == 0){
      marketcaptable <- c(marketcaptable, name, value, price)
    }
    
  }
  marketcaptable <- data.frame(marketcaptable)
  marketcaptable[,2] <- as.numeric(marketcaptable[,2])
  marketcaptable[,3] <- as.numeric(marketcaptable[,3])
  marketcaptable <- cbind(marketcaptable, as.numeric(c(marketcaptable[,2]/sum(marketcaptable[,2]))))
  rownames(marketcaptable) <- tickers
  colnames(marketcaptable) <- c('Ticker', 'Market Cap in M', 'Price at last Open', 'Capped Weight')
  shares <- rep(0, (dim(marketcaptable)[1]))
  value <- rep(0, (dim(marketcaptable)[1]))
  for (i in 1:(dim(marketcaptable)[1])) {
    ticker.principal <- principal*marketcaptable$`Capped Weight`[i]
    shares[i] <- ticker.principal/marketcaptable$`Price at last Open`[i]
    shares[i] <- floor(shares[i])
    value[i] <- shares[i]*marketcaptable$`Price at last Open`[i]
  }
  marketcaptable <- cbind(marketcaptable, shares, value)
  marketcaptable <-  cbind(marketcaptable, c(marketcaptable$value/sum(marketcaptable$value)))
  colnames(marketcaptable) <- c('Ticker', 'Market Cap in M', 'Price at last Open', 'Capped Weight', 'Shares to own', 'Value of shares', 'Actual Weight')
  print(marketcaptable)
}

## Example of the function with Canadian Telco companies

IndexBuilder(5000, c('SJR-B-T', 'QBR-B-T', 'BCE-T', 'RCI-B-T', 'T-T'))





