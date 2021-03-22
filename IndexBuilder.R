
library(data.table)
library(tidyr)
library(quantmod)
library(plyr)
library(rvest)
library(stringr)
library(magrittr)
library(tidyverse)

## Working example

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

## Example of the function with Canadian Communications Companies from the S&P/TSX Capped Communications Services Index (^TTTS)

Communications <- c('T-T', 'RCI-B-T', 'BCE-T', 'SJR-B-T', 'QBR-B-T', 'CCA-T', 'CJR-B-T')
IndexBuilder(5000, Communications)

## Example of the function with Canadian REITs (^TTRE or ^RTRE depending on choice - here we choose ^TTRE due to larger market cap and more consituents)

RealEstate <- c('AP-UN-T', 'AIF-T', 'AX-UN-T', 'BEI-UN-T', 'BPY-UN-T', 'CAR-UN-T', 'CHP-UN-T', 'CIGI-T', 'CUF-UN-T', 'CRR-UN-T', 'CRT-UN-T', 'DIR-UN-T', 'D-UN-T',
                'FCR-UN-T', 'FSV-T', 'GRT-UN-T', 'HR-UN-T', 'IIP-UN-T', 'KMP-UN-T', 'NWH-UN-T', 'REAL-T', 'REI-UN-T', 'SRU-UN-T', 'SMU-UN-T', 'TCN-T', 'WIR-UN-T')
REIT <- c('AP-UN-T', 'AX-UN-T', 'BEI-UN-T', 'CAR-UN-T', 'CHP-UN-T', 'CUF-UN-T', 'CRR-UN-T', 'CRT-UN-T', 'DIR-UN-T', 'D-UN-T',
          'FCR-UN-T', 'GRT-UN-T', 'HR-UN-T', 'IIP-UN-T', 'KMP-UN-T', 'NWH-UN-T', 'REI-UN-T', 'SRU-UN-T', 'SMU-UN-T', 'WIR-UN-T')
IndexBuilder(1500, RealEstate)
IndexBuilder(1500, REIT)

## Example of the function with Canadian Financials (^TTFS)

Financials <- c('BMO-T', 'BNS-T', 'BAM-A-T', 'CM-T', 'CWB-T', 'CIX-T', 'ECN-T', 'EFN-T', 'EQB-T', 'FFH-T', 'MIC-T', 'GWO-T', 'HCG-T', 'IAG-T',
                'IGM-T', 'IFC-T', 'LB-T', 'MFC-T', 'NA-T', 'ONEX-T', 'POW-T', 'RY-T', 'SII-T', 'SLF-T', 'X-T', 'TD-T')
IndexBuilder(3000, Financials)

## Note that investing based on Market Capitalization is mostly a risk-minimizing approach, as Market Capitalization represents the the worth
## of the company in the eyes of the market, and the underlying assumption is that a larger company will be more stable and less risky (note 
## this also implies that it may not outperform the market, but that can also depend on the chosen sector; Energy for example is usually more
## volatile than Financials).

## Then, we create a second version of the previous IndexBuilder function: it will give us more information about each constituent of the Index
## used. Here, we use the 36-month Beta, the Forward P/E Ratio (or TTM if it is unavailable), as well as the Dividend Ratio. We also create an
## evaluator function to reflect the capped values for these metrics.

IndexBuilder2.0 <- function(principal, tickers){
  marketcaptable <- c()
  for (i in 1:length(tickers)) {
    url <- paste0('https://www.theglobeandmail.com/investing/markets/stocks/', tickers[i])
    page <- read_html(url)
    nodes <- page %>% html_nodes('#fBsZNE1qvBE4os .barchart-overview-field:nth-child(8) barchart-field , #fundamentals .barchart-overview-field:nth-child(3) .bc-percent-change , .col-lg-6+ .col-lg-6 .barchart-overview-field:nth-child(2) barchart-field , #fundamentals .col-lg-6:nth-child(1) .barchart-overview-field:nth-child(3) barchart-field')
    Symbol <- nodes[1] %>% html_attr('symbol')
    MarketCap <- nodes[1] %>% html_attr('value')
    MarketCap <- as.numeric(gsub(',', '', MarketCap))
    Beta <- nodes[2] %>% html_attr('value')
    Beta <- as.numeric(Beta)
    PERatio <- nodes[3] %>% html_attr('value')
    PERatio <- as.numeric(PERatio)
    if(is.na(PERatio) == TRUE){
      extra.node <- page %>% html_nodes('.col-lg-6+ .col-lg-6 .barchart-overview-field:nth-child(1) barchart-field')
      PERatio <- extra.node %>% html_attr('value')
      PERatio <- as.numeric(PERatio)
    }
    DividendRatio <- nodes[4] %>% html_attr('value')
    DividendRatio <- as.numeric(gsub('%', '', DividendRatio))
    if(is.na(DividendRatio) == TRUE){
      DividendRatio <- 0
    }
    Price <- page %>% html_nodes('#fBsZNE1qvBE4os .open-price-value') %>% html_text()
    Price <- as.numeric(Price)
    if(length(marketcaptable) != 0){
      marketcaptable <- rbind(marketcaptable, c(Symbol, MarketCap, Beta, PERatio, DividendRatio, Price))
    }
    if(length(marketcaptable) == 0){
      marketcaptable <- c(marketcaptable, Symbol, MarketCap, Beta, PERatio, DividendRatio, Price)
    }
  }
  marketcaptable <- data.frame(marketcaptable)
  for (j in 2:(dim(marketcaptable)[2])) {
    marketcaptable[,j] <- as.numeric(marketcaptable[,j])
  }
  marketcaptable <- cbind(marketcaptable, as.numeric(c(marketcaptable[,2]/sum(marketcaptable[,2]))))
  rownames(marketcaptable) <- tickers
  colnames(marketcaptable) <- c('Ticker', 'Market Cap in M', '36M Beta', 'P/E Ratio', 'Dividend Ratio', 'Price at last Open', 'Capped Weight')
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
  colnames(marketcaptable) <- c('Ticker', 'Market Cap in M', '36M Beta', 'P/E Ratio', 'Dividend Ratio', 'Price at last Open', 'Capped Weight', 'Shares to own', 'Value of shares', 'Actual Weight')
  Data.Index <<- marketcaptable
  print(marketcaptable)
}

IndexEvaluator2.0 <- function(){
  IndexDividendRatio <- sum(Data.Index$`Actual Weight`*Data.Index$`Dividend Ratio`)
  IndexBeta <- sum(Data.Index$`36M Beta`*Data.Index$`Actual Weight`)
  IndexPERatio <- sum(Data.Index$`P/E Ratio`*Data.Index$`Actual Weight`)
  IndexDividend <- IndexDividendRatio*sum(Data.Index$`Value of shares`)/100
  print(c('Index Beta is ', IndexBeta))
  print(c('Index P/E Ratio is ', IndexPERatio))
  print(c('Index Dividend Ratio is ', IndexDividendRatio, '%, and Annual Dividend based on current values is ', IndexDividend, '$'))
}

## Example with Financials

IndexBuilder2.0(3000, Financials)
IndexEvaluator2.0()


## US side

## General S&P 500 Scraping

url <- 'https://en.wikipedia.org/wiki/List_of_S%26P_500_companies'
page <- read_html(url)
nodes <- page %>% html_nodes('#constituents td:nth-child(4) , td:nth-child(1) .text')
SP500 <- c()
for (i in 1:(length(nodes)/2)) {
  if(length(SP500) != 0){
    SP500 <- rbind(SP500, c(html_text(nodes[(2*i)-1]), html_text(nodes[2*i])))
  }
  if(length(SP500) == 0){
    SP500 <- c(SP500, html_text(nodes[(2*i)-1]), html_text(nodes[2*i]))
  }
}
SP500 <- data.frame(SP500)
colnames(SP500) <- c('Ticker', 'Sector')
rownames(SP500) <- SP500$Ticker
SP500$Ticker <- gsub('\\.', '-', SP500$Ticker)

## S&P 500 Financials working example

FinIndex <- which(SP500$Sector == 'Financials')
SP500Financials <- SP500[FinIndex, 1]

principal <- 10000
marketcaptable <- c()
for (i in 1:length(SP500Financials)) {
  url <- c(paste0('https://finviz.com/quote.ashx?t=', SP500Financials[i]))
  page <- read_html(url)
  nodes <- page %>% html_nodes('.snapshot-td2 b')
  InsiderOwnership <- nodes[4] %>% html_text()
  InsiderOwnership <- as.numeric(gsub('%', '', InsiderOwnership))
  MarketCap <- nodes[7] %>% html_text()
  if(substr(MarketCap, nchar(MarketCap), nchar(MarketCap)) != 'B'){
    MarketCap <- as.numeric(substr(MarketCap, 1, nchar(MarketCap)-1))
  }
  if(substr(MarketCap, nchar(MarketCap), nchar(MarketCap)) == 'B'){
    MarketCap <- 1000 * as.numeric(substr(MarketCap, 1, nchar(MarketCap)-1))
  }
  PERatio <- nodes[8] %>% html_text()
  PERatio <- as.numeric(PERatio)
  if(is.na(PERatio) == TRUE){
    PERatio <- nodes[2] %>% html_text()
    PERatio <- as.numeric(PERatio)
  }
  Beta <- nodes[42] %>% html_text()
  if(Beta == '-'){
    Beta <- 1
  }
  Beta <- as.numeric(Beta)
  DividendRatio <- nodes[43] %>% html_text()
  if(DividendRatio == '-'){
    DividendRatio <- 0
  }
  DividendRatio <- as.numeric(gsub('%', '', DividendRatio))
  Price <- nodes[66] %>% html_text()
  Price <- as.numeric(Price)
  if(length(marketcaptable) != 0){
    marketcaptable <- rbind(marketcaptable, c(SP500Financials[i], MarketCap, Beta, PERatio, DividendRatio, Price, InsiderOwnership))
  }
  if(length(marketcaptable) == 0){
    marketcaptable <- c(marketcaptable, SP500Financials[i], MarketCap, Beta, PERatio, DividendRatio, Price, InsiderOwnership)
  }
}
marketcaptable <- data.frame(marketcaptable)
for (j in 2:(dim(marketcaptable)[2])) {
  marketcaptable[,j] <- as.numeric(marketcaptable[,j])
}
marketcaptable <- cbind(marketcaptable, as.numeric(c(marketcaptable[,2]/sum(marketcaptable[,2]))))
rownames(marketcaptable) <- SP500Financials
colnames(marketcaptable) <- c('Ticker', 'Market Cap in M', 'Beta', 'P/E Ratio', 'Dividend Ratio', 'Price', 'Insider Ownership', 'Capped Weight')
shares <- rep(0, (dim(marketcaptable)[1]))
value <- rep(0, (dim(marketcaptable)[1]))
for (i in 1:(dim(marketcaptable)[1])) {
  ticker.principal <- principal*marketcaptable$`Capped Weight`[i]
  shares[i] <- ticker.principal/marketcaptable$`Price`[i]
  shares[i] <- floor(shares[i])
  value[i] <- shares[i]*marketcaptable$`Price`[i]
}
marketcaptable <- cbind(marketcaptable, shares, value)
marketcaptable <-  cbind(marketcaptable, c(marketcaptable$value/sum(marketcaptable$value)))
colnames(marketcaptable) <- c('Ticker', 'Market Cap in M', 'Beta', 'P/E Ratio', 'Dividend Ratio', 'Price', 'Insider Ownership', 'Capped Weight', 'Shares to own', 'Value of shares', 'Actual Weight')
print(marketcaptable)

## Note that due to the sheer size of the index, many low weight tickers are not taken into account in our portfolio
## We then refine our table to only show the stocks selected by the random portfolio, and also calculate the total weight
## of the index actually invested in:

print(marketcaptable[which(marketcaptable$`Shares to own` != 0),])
sum(marketcaptable$`Capped Weight`[which(marketcaptable$`Shares to own` != 0)])

## We can now build IndexBuilder 3.0 which allows us to scrape from both US & Canadian Markets
## (Notation for US Stocks is simple tickers, CAN Stocks are either ticker-T or ticker-X, depending
## on whether they are listed on the TSX or the TSX-Ventures respectively)

IndexBuilder3.0 <- function(principal, tickers, market){
  if(market == 'CAN'){
    marketcaptable <- c()
    for (i in 1:length(tickers)) {
      url <- paste0('https://www.theglobeandmail.com/investing/markets/stocks/', tickers[i])
      page <- read_html(url)
      nodes <- page %>% html_nodes('#fBsZNE1qvBE4os .barchart-overview-field:nth-child(8) barchart-field , #fundamentals .barchart-overview-field:nth-child(3) .bc-percent-change , .col-lg-6+ .col-lg-6 .barchart-overview-field:nth-child(2) barchart-field , #fundamentals .col-lg-6:nth-child(1) .barchart-overview-field:nth-child(3) barchart-field')
      Symbol <- nodes[1] %>% html_attr('symbol')
      MarketCap <- nodes[1] %>% html_attr('value')
      MarketCap <- as.numeric(gsub(',', '', MarketCap))
      Beta <- nodes[2] %>% html_attr('value')
      Beta <- as.numeric(Beta)
      PERatio <- nodes[3] %>% html_attr('value')
      PERatio <- as.numeric(PERatio)
      if(is.na(PERatio) == TRUE){
        extra.node <- page %>% html_nodes('.col-lg-6+ .col-lg-6 .barchart-overview-field:nth-child(1) barchart-field')
        PERatio <- extra.node %>% html_attr('value')
        PERatio <- as.numeric(PERatio)
      }
      DividendRatio <- nodes[4] %>% html_attr('value')
      DividendRatio <- as.numeric(gsub('%', '', DividendRatio))
      if(is.na(DividendRatio) == TRUE){
        DividendRatio <- 0
      }
      Price <- page %>% html_nodes('#fBsZNE1qvBE4os .open-price-value') %>% html_text()
      Price <- as.numeric(Price)
      if(length(marketcaptable) != 0){
        marketcaptable <- rbind(marketcaptable, c(Symbol, MarketCap, Beta, PERatio, DividendRatio, Price))
      }
      if(length(marketcaptable) == 0){
        marketcaptable <- c(marketcaptable, Symbol, MarketCap, Beta, PERatio, DividendRatio, Price)
      }
    }
    marketcaptable <- data.frame(marketcaptable)
    for (j in 2:(dim(marketcaptable)[2])) {
      marketcaptable[,j] <- as.numeric(marketcaptable[,j])
    }
    marketcaptable <- cbind(marketcaptable, as.numeric(c(marketcaptable[,2]/sum(marketcaptable[,2]))))
    rownames(marketcaptable) <- tickers
    colnames(marketcaptable) <- c('Ticker', 'Market Cap in M', 'Beta', 'P/E Ratio', 'Dividend Ratio', 'Price', 'Capped Weight')
    shares <- rep(0, (dim(marketcaptable)[1]))
    value <- rep(0, (dim(marketcaptable)[1]))
    for (i in 1:(dim(marketcaptable)[1])) {
      ticker.principal <- principal*marketcaptable$`Capped Weight`[i]
      shares[i] <- ticker.principal/marketcaptable$`Price`[i]
      shares[i] <- floor(shares[i])
      value[i] <- shares[i]*marketcaptable$`Price`[i]
    }
    marketcaptable <- cbind(marketcaptable, shares, value)
    marketcaptable <-  cbind(marketcaptable, c(marketcaptable$value/sum(marketcaptable$value)))
    colnames(marketcaptable) <- c('Ticker', 'Market Cap in M', 'Beta', 'P/E Ratio', 'Dividend Ratio', 'Price', 'Capped Weight', 'Shares to own', 'Value of shares', 'Actual Weight')
    print(marketcaptable)
    print(marketcaptable[which(marketcaptable$`Shares to own` != 0),])
    sum(marketcaptable$`Capped Weight`[which(marketcaptable$`Shares to own` != 0)])
    Data.Index <<- marketcaptable[which(marketcaptable$`Shares to own` != 0),]
  }
  if(market == 'USD'){
    marketcaptable <- c()
    for (i in 1:length(tickers)) {
      url <- c(paste0('https://finviz.com/quote.ashx?t=', tickers[i]))
      page <- read_html(url)
      nodes <- page %>% html_nodes('.snapshot-td2 b')
      InsiderOwnership <- nodes[4] %>% html_text()
      InsiderOwnership <- as.numeric(gsub('%', '', InsiderOwnership))
      MarketCap <- nodes[7] %>% html_text()
      if(substr(MarketCap, nchar(MarketCap), nchar(MarketCap)) != 'B'){
        MarketCap <- as.numeric(substr(MarketCap, 1, nchar(MarketCap)-1))
      }
      if(substr(MarketCap, nchar(MarketCap), nchar(MarketCap)) == 'B'){
        MarketCap <- 1000 * as.numeric(substr(MarketCap, 1, nchar(MarketCap)-1))
      }
      PERatio <- nodes[8] %>% html_text()
      PERatio <- as.numeric(PERatio)
      if(is.na(PERatio) == TRUE){
        PERatio <- nodes[2] %>% html_text()
        PERatio <- as.numeric(PERatio)
      }
      Beta <- nodes[42] %>% html_text()
      if(Beta == '-'){
        Beta <- 1
      }
      Beta <- as.numeric(Beta)
      DividendRatio <- nodes[43] %>% html_text()
      if(DividendRatio == '-'){
        DividendRatio <- 0
      }
      DividendRatio <- as.numeric(gsub('%', '', DividendRatio))
      Price <- nodes[66] %>% html_text()
      Price <- as.numeric(Price)
      if(length(marketcaptable) != 0){
        marketcaptable <- rbind(marketcaptable, c(tickers[i], MarketCap, Beta, PERatio, DividendRatio, Price, InsiderOwnership))
      }
      if(length(marketcaptable) == 0){
        marketcaptable <- c(marketcaptable, tickers[i], MarketCap, Beta, PERatio, DividendRatio, Price, InsiderOwnership)
      }
    }
    marketcaptable <- data.frame(marketcaptable)
    for (j in 2:(dim(marketcaptable)[2])) {
      marketcaptable[,j] <- as.numeric(marketcaptable[,j])
    }
    marketcaptable <- cbind(marketcaptable, as.numeric(c(marketcaptable[,2]/sum(marketcaptable[,2]))))
    rownames(marketcaptable) <- tickers
    colnames(marketcaptable) <- c('Ticker', 'Market Cap in M', 'Beta', 'P/E Ratio', 'Dividend Ratio', 'Price', 'Insider Ownership', 'Capped Weight')
    shares <- rep(0, (dim(marketcaptable)[1]))
    value <- rep(0, (dim(marketcaptable)[1]))
    for (i in 1:(dim(marketcaptable)[1])) {
      ticker.principal <- principal*marketcaptable$`Capped Weight`[i]
      shares[i] <- ticker.principal/marketcaptable$`Price`[i]
      shares[i] <- floor(shares[i])
      value[i] <- shares[i]*marketcaptable$`Price`[i]
    }
    marketcaptable <- cbind(marketcaptable, shares, value)
    marketcaptable <-  cbind(marketcaptable, c(marketcaptable$value/sum(marketcaptable$value)))
    colnames(marketcaptable) <- c('Ticker', 'Market Cap in M', 'Beta', 'P/E Ratio', 'Dividend Ratio', 'Price', 'Insider Ownership', 'Capped Weight', 'Shares to own', 'Value of shares', 'Actual Weight')
    print(marketcaptable)
    print(marketcaptable[which(marketcaptable$`Shares to own` != 0),])
    sum(marketcaptable$`Capped Weight`[which(marketcaptable$`Shares to own` != 0)])
    Data.Index <<- marketcaptable[which(marketcaptable$`Shares to own` != 0),]
  }
}

IndexEvaluator3.0 <- function(){
  IndexDividendRatio <- sum(Data.Index$`Actual Weight`*Data.Index$`Dividend Ratio`)
  IndexBeta <- sum(Data.Index$`Beta`*Data.Index$`Actual Weight`)
  IndexPERatio <- sum(Data.Index$`P/E Ratio`*Data.Index$`Actual Weight`)
  IndexDividend <- IndexDividendRatio*sum(Data.Index$`Value of shares`)/100
  print(c('Index Beta is ', IndexBeta))
  print(c('Index P/E Ratio is ', IndexPERatio))
  print(c('Index Dividend Ratio is ', IndexDividendRatio, '%, and Annual Dividend based on current values is ', IndexDividend, '$'))
}

## Example with Canadian Financials

IndexBuilder3.0(10000, Financials, 'CAN')
IndexEvaluator3.0()

## Example with US Financials

IndexBuilder3.0(10000, SP500Financials, 'USD')
IndexEvaluator3.0()

## While this was not the goal of this project, looking at the difference between
## CAN & US Markets for Financials shows us that there is more diversity in the 
## US Market, but also that there is more opportunity for Growth, as well as Risk
## as the Beta is higher, and the Dividend Ratio is lower compared to CAN Market.

## IndexBuilder 3.1 is a simple upgrade to CAN Market Scraping: For the US Market,
## Builder3.0 also scraped Insider Ownership (this is useful when using free-float
## market cap weighting vs true market cap weighting). Now Builder3.1 also scrapes
## Insider Ownership percentage for CAN tickers. The Goal is for future Builder/Evaluator
## Functions to recommend portfolios based on other weighting methods

IndexBuilder3.1 <- function(principal, tickers, market){
  if(market == 'CAN'){
    marketcaptable <- c()
    for (i in 1:length(tickers)) {
      url <- paste0('https://www.theglobeandmail.com/investing/markets/stocks/', tickers[i])
      page <- read_html(url)
      nodes <- page %>% html_nodes('#fBsZNE1qvBE4os .barchart-overview-field:nth-child(8) barchart-field , #fundamentals .barchart-overview-field:nth-child(3) .bc-percent-change , .col-lg-6+ .col-lg-6 .barchart-overview-field:nth-child(2) barchart-field , #fundamentals .col-lg-6:nth-child(1) .barchart-overview-field:nth-child(3) barchart-field')
      Symbol <- nodes[1] %>% html_attr('symbol')
      MarketCap <- nodes[1] %>% html_attr('value')
      MarketCap <- as.numeric(gsub(',', '', MarketCap))
      Beta <- nodes[2] %>% html_attr('value')
      Beta <- as.numeric(Beta)
      PERatio <- nodes[3] %>% html_attr('value')
      PERatio <- as.numeric(PERatio)
      if(is.na(PERatio) == TRUE){
        extra.node <- page %>% html_nodes('.col-lg-6+ .col-lg-6 .barchart-overview-field:nth-child(1) barchart-field')
        PERatio <- extra.node %>% html_attr('value')
        PERatio <- as.numeric(PERatio)
      }
      DividendRatio <- nodes[4] %>% html_attr('value')
      DividendRatio <- as.numeric(gsub('%', '', DividendRatio))
      if(is.na(DividendRatio) == TRUE){
        DividendRatio <- 0
      }
      Price <- page %>% html_nodes('#fBsZNE1qvBE4os .open-price-value') %>% html_text()
      Price <- as.numeric(Price)
      extra.url <- paste0('https://finance.yahoo.com/quote/', Symbol, '/key-statistics?p=', Symbol)
      extra.page <- read_html(extra.url)
      extra.node2 <- html_table(extra.page)
      InsiderOwnership <- as.numeric(gsub('%', '', extra.node2[[3]][5,2]))
      if(is.na(InsiderOwnership) == TRUE){
        InsiderOwnership <- 0
      }
      if(length(marketcaptable) != 0){
        marketcaptable <- rbind(marketcaptable, c(Symbol, MarketCap, Beta, PERatio, DividendRatio, Price, InsiderOwnership))
      }
      if(length(marketcaptable) == 0){
        marketcaptable <- c(marketcaptable, Symbol, MarketCap, Beta, PERatio, DividendRatio, Price, InsiderOwnership)
      }
    }
    marketcaptable <- data.frame(marketcaptable)
    for (j in 2:(dim(marketcaptable)[2])) {
      marketcaptable[,j] <- as.numeric(marketcaptable[,j])
    }
    marketcaptable <- cbind(marketcaptable, as.numeric(c(marketcaptable[,2]/sum(marketcaptable[,2]))))
    rownames(marketcaptable) <- tickers
    colnames(marketcaptable) <- c('Ticker', 'Market Cap in M', 'Beta', 'P/E Ratio', 'Dividend Ratio', 'Price', 'Insider Ownership', 'Capped Weight')
    shares <- rep(0, (dim(marketcaptable)[1]))
    value <- rep(0, (dim(marketcaptable)[1]))
    for (i in 1:(dim(marketcaptable)[1])) {
      ticker.principal <- principal*marketcaptable$`Capped Weight`[i]
      shares[i] <- ticker.principal/marketcaptable$`Price`[i]
      shares[i] <- floor(shares[i])
      value[i] <- shares[i]*marketcaptable$`Price`[i]
    }
    marketcaptable <- cbind(marketcaptable, shares, value)
    marketcaptable <-  cbind(marketcaptable, c(marketcaptable$value/sum(marketcaptable$value)))
    colnames(marketcaptable) <- c('Ticker', 'Market Cap in M', 'Beta', 'P/E Ratio', 'Dividend Ratio', 'Price', 'Insider Ownership', 'Capped Weight', 'Shares to own', 'Value of shares', 'Actual Weight')
    print(marketcaptable)
    print(marketcaptable[which(marketcaptable$`Shares to own` != 0),])
    sum(marketcaptable$`Capped Weight`[which(marketcaptable$`Shares to own` != 0)])
    Data.Index <<- marketcaptable[which(marketcaptable$`Shares to own` != 0),]
  }
  if(market == 'USD'){
    marketcaptable <- c()
    for (i in 1:length(tickers)) {
      url <- c(paste0('https://finviz.com/quote.ashx?t=', tickers[i]))
      page <- read_html(url)
      nodes <- page %>% html_nodes('.snapshot-td2 b')
      InsiderOwnership <- nodes[4] %>% html_text()
      InsiderOwnership <- as.numeric(gsub('%', '', InsiderOwnership))
      if(is.na(InsiderOwnership) == TRUE){
        InsiderOwnership <- 0
      }
      MarketCap <- nodes[7] %>% html_text()
      if(substr(MarketCap, nchar(MarketCap), nchar(MarketCap)) != 'B'){
        MarketCap <- as.numeric(substr(MarketCap, 1, nchar(MarketCap)-1))
      }
      if(substr(MarketCap, nchar(MarketCap), nchar(MarketCap)) == 'B'){
        MarketCap <- 1000 * as.numeric(substr(MarketCap, 1, nchar(MarketCap)-1))
      }
      PERatio <- nodes[8] %>% html_text()
      PERatio <- as.numeric(PERatio)
      if(is.na(PERatio) == TRUE){
        PERatio <- nodes[2] %>% html_text()
        PERatio <- as.numeric(PERatio)
      }
      Beta <- nodes[42] %>% html_text()
      if(Beta == '-'){
        Beta <- 1
      }
      Beta <- as.numeric(Beta)
      DividendRatio <- nodes[43] %>% html_text()
      if(DividendRatio == '-'){
        DividendRatio <- 0
      }
      DividendRatio <- as.numeric(gsub('%', '', DividendRatio))
      Price <- nodes[66] %>% html_text()
      Price <- as.numeric(Price)
      if(length(marketcaptable) != 0){
        marketcaptable <- rbind(marketcaptable, c(tickers[i], MarketCap, Beta, PERatio, DividendRatio, Price, InsiderOwnership))
      }
      if(length(marketcaptable) == 0){
        marketcaptable <- c(marketcaptable, tickers[i], MarketCap, Beta, PERatio, DividendRatio, Price, InsiderOwnership)
      }
    }
    marketcaptable <- data.frame(marketcaptable)
    for (j in 2:(dim(marketcaptable)[2])) {
      marketcaptable[,j] <- as.numeric(marketcaptable[,j])
    }
    marketcaptable <- cbind(marketcaptable, as.numeric(c(marketcaptable[,2]/sum(marketcaptable[,2]))))
    rownames(marketcaptable) <- tickers
    colnames(marketcaptable) <- c('Ticker', 'Market Cap in M', 'Beta', 'P/E Ratio', 'Dividend Ratio', 'Price', 'Insider Ownership', 'Capped Weight')
    shares <- rep(0, (dim(marketcaptable)[1]))
    value <- rep(0, (dim(marketcaptable)[1]))
    for (i in 1:(dim(marketcaptable)[1])) {
      ticker.principal <- principal*marketcaptable$`Capped Weight`[i]
      shares[i] <- ticker.principal/marketcaptable$`Price`[i]
      shares[i] <- floor(shares[i])
      value[i] <- shares[i]*marketcaptable$`Price`[i]
    }
    marketcaptable <- cbind(marketcaptable, shares, value)
    marketcaptable <-  cbind(marketcaptable, c(marketcaptable$value/sum(marketcaptable$value)))
    colnames(marketcaptable) <- c('Ticker', 'Market Cap in M', 'Beta', 'P/E Ratio', 'Dividend Ratio', 'Price', 'Insider Ownership', 'Capped Weight', 'Shares to own', 'Value of shares', 'Actual Weight')
    print(marketcaptable)
    print(marketcaptable[which(marketcaptable$`Shares to own` != 0),])
    sum(marketcaptable$`Capped Weight`[which(marketcaptable$`Shares to own` != 0)])
    Data.Index <<- marketcaptable[which(marketcaptable$`Shares to own` != 0),]
  }
}

## Example

IndexBuilder3.1(10000, Financials, 'CAN')
IndexEvaluator3.0()

