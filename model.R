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
library(zoo)

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
cryptoMktCap <- read.csv(file = "mkt_caps.csv")



#Get Annualized Returns for Cryptocurrencies
cryptoPricing <- read.csv(file = "pricing_data.csv")

names <- colnames(cryptoPricing)

dates <- as.Date(cryptoPricing[,1], format = "%Y-%m-%d")

cryptoPricing  <- cryptoPricing[,2:length(cryptoPricing),] %>% 
  xts(order.by = dates)

getReturn <- function(crypto) {
  crypto %>% 
    na.omit %>% 
    CalculateReturns %>% 
    Return.annualized
}

returns <- apply(cryptoPricing, 2, getReturn) 

#Visualize Top Normalized Returns by Market Cap
normalize <- function(x) {
  return(x/first(x))
}

cryptoPricing[,1:5] %>% 
  apply(2, normalize) %>% 
  as.xts %>% 
  plot.zoo(
    plot.type = "single",
    log="y",
    col = 1:5,
    ylab = "Normalized Log Returns",
    xlab = "Year",
    main = "Normalized Log Returns for Top 5 Crytpocurrencies by Market Cap"
    ) 
legend("topleft", 
       inset=c(0,0), 
       y.intersp = 1, 
       legend = names[1:5], 
       lty = 1, 
       bty = "n", 
       col = 1:5, 
       cex = 1)




  
