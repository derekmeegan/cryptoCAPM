#Get Treasury Rate Annual Returns <- Risk Free Rate
#Get Top 50 Crypto Currencies by Market Cap
#Get Annual Period Returns of Cryptos
#Average Returns of Cryptos Weighted by Mkt Cap <- Market Return
#Calculate Volatility of Cryptocurrencies <- Beta
#Apply the traditional CAPM Formula <- rE = rF + B(rM - rF)

library(quantmod)
library(tidyverse)
library(ggdark)
library(scales)
library(PerformanceAnalytics)

#Visualize 3 Month Treasury Returns 
getSymbols("DGS3MO", src = "FRED")

DGS3MO %>% 
  chartSeries(name = "3 Month Treasury Returns")

#Get Annual Period Returns for 3 Month Treasury
annualReturns <- DGS3MO %>% 
  na.omit %>% 
  periodReturn(period = "yearly") %>% 
  .[-c(length(.)),]

annualReturns %>% ggplot(aes(x = index(annualReturns), y= yearly.returns)) +
  geom_col(color = "lightGreen") +
  geom_smooth(method="loess", color="yellow") +
  dark_mode() + 
  ggtitle("3 Month Treasury Period Returns (Annual)") +
  scale_y_continuous(labels = percent) +
  labs(x = "Year", y = "Returns")

#Current Risk Free Rate
rF <- DGS3MO %>% 
  as.tibble %>% 
  tail(1) %>% 
  pull

#Get Crypto Market Caps
cryptoData <- read.csv(file = "data.csv")

getAnnualReturns <- function(symbols) {
  answer <- c()
  for (symbol in symbols) {
    answer[[symbol]] <- getSymbols(gsub(" ", "", paste(symbol, "-usd")), auto.assign = FALSE) %>% 
      Cl %>% 
      periodReturn('yearly') %>% 
      as.tibble %>% 
      pull %>% 
      mean
  }
   return(answer)
}

getAnnualReturns("btc") %>% tail

df <- cryptoData %>% 
  as.tibble %>% 
  .[c(2,3,5)] %>% 
  arrange(market_cap %>% desc) %>% 
  head(50) 

write.table(df , file = "top50_mktcap.csv")


portfolio_returns_xts_rebalanced_monthly <- 
  Return.portfolio(asset_returns_xts, weights = w, rebalance_on = "months") %>%
  `colnames<-`("returns")


  
