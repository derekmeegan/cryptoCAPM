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

getSymbols("DGS3MO", src = "FRED")

#Visualize 3 Month Treasury Returns 

DGS3MO %>% 
  chartSeries(name = "3 Month Treasury Returns")

#Get Annual Period Returns for 3 Month Treasury

annualReturns <- DGS3MO %>% 
  na.omit %>% 
  periodReturn(period = "yearly") %>% 
  as.tibble


annualReturns %>% ggplot(aes(x = index(annualReturns), y= yearly.returns)) +
  geom_line(color = "lightGreen") +
  geom_smooth(method="loess", color="yellow") +
  dark_mode() + 
  ggtitle("3 Month Treasury Period Returns (Annual)") +
  scale_y_continuous(labels = percent) +
  labs(
    x = "Returns", 
    y = "Year")

#Current Risk Free Rate

rF <- DGS3MO %>% 
  as.tibble %>% 
  tail(1) %>% 
  pull


