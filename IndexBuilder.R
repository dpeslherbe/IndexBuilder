
library(data.table)
library(tidyr)
library(quantmod)
library(plyr)
library(rvest)
library(stringr)
library(magrittr)
library(tidyverse)
library(XML)

Communications <- c('T-T', 'RCI-B-T', 'BCE-T', 'SJR-B-T', 'QBR-B-T', 'CCA-T', 'CJR-B-T')
Financials <- c('BMO-T', 'BNS-T', 'BAM-A-T', 'CM-T', 'CWB-T', 'CIX-T', 'ECN-T', 'EFN-T', 'EQB-T', 'FFH-T', 'MIC-T', 'GWO-T', 'HCG-T', 'IAG-T',
                'IGM-T', 'IFC-T', 'LB-T', 'MFC-T', 'NA-T', 'ONEX-T', 'POW-T', 'RY-T', 'SII-T', 'SLF-T', 'X-T', 'TD-T')
REIT <- c('AP-UN.TO', 'AX-UN.TO', 'BEI-UN.TO', 'CAR-UN.TO', 'CHP-UN.TO', 'CUF-UN.TO', 'CRR-UN.TO', 'CRT-UN.TO', 'DIR-UN.TO', 'D-UN.TO',
          'FCR-UN.TO', 'GRT-UN.TO', 'HR-UN.TO', 'IIP-UN.TO', 'KMP-UN.TO', 'NWH-UN.TO', 'REI-UN.TO', 'SRU-UN.TO', 'SMU-UN.TO', 'WIR-UN.TO')

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
SP500FinIndex <- which(SP500$Sector == 'Financials')
SP500Financials <- SP500$Ticker[SP500FinIndex]

ticker.test <- c('CAR-UN.TO', 'REI-UN.TO', 'RY.TO')

IndexScraper <- function(tickers){
  marketcaptable <- c()
  for (i in 1:length(tickers)) {
    url.basic <- paste0('https://ca.finance.yahoo.com/quote/', tickers[i], '?p=', tickers[i])
    page.basic <- read_html(url.basic)
    nodes.basic <- html_table(page.basic)
    source.basic <- readLines(url.basic, encoding = 'UTF-8', warn = FALSE)
    parsed.basic <- htmlParse(source.basic, encoding = 'UTF-8')
    Price <- xpathSApply(parsed.basic, path = '/html/body/div[1]/div/div/div[1]/div/div[2]/div/div/div[4]/div/div/div/div[3]/div[1]/div/span[1]', xmlValue)
    Price <- as.numeric(Price)
    url.stats <- paste0('https://ca.finance.yahoo.com/quote/', tickers[i], '/key-statistics?p=', tickers[i])
    page.stats <- read_html(url.stats)
    nodes.stats <- html_table(page.stats)
    source.stats <- readLines(url.stats, encoding = 'UTF-8', warn = FALSE)
    parsed.stats <- htmlParse(source.stats, encoding = 'UTF-8')
    currency <- xpathSApply(parsed.stats, path = '/html/body/div[1]/div/div/div[1]/div/div[3]/div[1]/div/div[1]/div/div/section/div[1]/span', xmlValue)
    currency <- substr(currency, 13, 15)
    url.hist <- paste0('https://ca.finance.yahoo.com/quote/', tickers[i], '/history?p=', tickers[i])
    page.hist <- read_html(url.hist)
    nodes.hist <- html_table(page.hist)
    MarketCap <- nodes.basic[[2]][1,2]
    if(substr(MarketCap, nchar(MarketCap), nchar(MarketCap)) != 'B'){
      if(substr(MarketCap, nchar(MarketCap), nchar(MarketCap)) == 'T'){
        MarketCap <- 1000 * 1000 * as.numeric(substr(MarketCap, 1, nchar(MarketCap)-1))
      }
      if(substr(MarketCap, nchar(MarketCap), nchar(MarketCap)) != 'T'){
        MarketCap <- as.numeric(substr(MarketCap, 1, nchar(MarketCap)-1))
      }
    }
    if(substr(MarketCap, nchar(MarketCap), nchar(MarketCap)) == 'B'){
      MarketCap <- 1000 * as.numeric(substr(MarketCap, 1, nchar(MarketCap)-1))
    }
    Beta <- as.numeric(nodes.basic[[2]][2,2])
    PERatio <- as.numeric(nodes.basic[[2]][3,2])
    EPS <- as.numeric(nodes.basic[[2]][4,2])
    InsiderOwnership <- as.numeric(substr(nodes.stats[[3]][6,2], 1, nchar(nodes.stats[[3]][6,2])-1))
    DividendYield <- as.numeric(nodes.stats[[4]][5,2])
    if(is.na(DividendYield) == TRUE){
      DividendYield <- as.numeric(gsub('%', '', nodes.stats[[4]][4,2]))
    }
    Data <- c(tickers[i], currency, Price, MarketCap, Beta, PERatio, EPS, DividendYield, InsiderOwnership)
    marketcaptable <- data.frame(rbind(marketcaptable, Data))
  }
  marketcaptable <- marketcaptable
  colnames(marketcaptable) <- c('Ticker', 'Currency', 'Price', 'Market Cap', 'Beta', 'P/E Ratio', 'EPS', 'Dividend Yield', 'Insider Ownership')
  rownames(marketcaptable) <- tickers
  for (i in 3:9) {
    marketcaptable[,i] <- as.numeric(marketcaptable[,i])
  }
  DataTable <<- marketcaptable
}

IndexScraper(ticker.test)

CurrencyScaper<- function(){
  CADUSDurl <- paste0('https://ca.finance.yahoo.com/quote/CADUSD%3DX?p=CADUSD%3DX')
  CADUSDsource <- readLines(CADUSDurl, encoding = 'UTF-8', warn = FALSE)
  CADUSDparsed <- htmlParse(CADUSDsource, encoding = 'UTF-8')
  CADUSD <- xpathSApply(CADUSDparsed, path = '//*[@id="quote-header-info"]/div[3]/div[1]/div/span[1]', xmlValue)
  CADUSD <<- as.numeric(CADUSD)
  USDCADurl <- paste0('https://ca.finance.yahoo.com/quote/CAD%3DX?p=CAD%3DX')
  USDCADsource <- readLines(USDCADurl, encoding = 'UTF-8', warn = FALSE)
  USDCADparsed <- htmlParse(USDCADsource, encoding = 'UTF-8')
  USDCAD <- xpathSApply(USDCADparsed, path = '//*[@id="quote-header-info"]/div[3]/div[1]/div/span[1]', xmlValue)
  USDCAD <<- as.numeric(USDCAD)
}

CurrencyScaper()

IndexBuilder <- function(currency){
  if(currency == 'CAD'){
    for (i in 1:(dim(DataTable)[1])) {
      if(DataTable$Currency[i] != currency){
        DataTable[i, 3] <- DataTable[i, 3]*USDCAD
        DataTable[i, 4] <- DataTable[i, 4]*USDCAD
        DataTable[i, 2] <- currency
      }
    }
  }
  if(currency == 'USD'){
    for (i in 1:(dim(DataTable)[1])) {
      if(DataTable$Currency[i] != currency){
        DataTable[i, 3] <- DataTable[i, 3]*CADUSD
        DataTable[i, 4] <- DataTable[i, 4]*CADUSD
        DataTable[i, 2] <- currency
      }
    }
  }
  DataTable <<- DataTable
}

IndexScraper(c('AAPL', 'RY.TO'))
CurrencyScaper()
IndexBuilder('CAD')

IndexScraper(c('AAPL', 'RY.TO'))
CurrencyScaper()
IndexBuilder('USD')

