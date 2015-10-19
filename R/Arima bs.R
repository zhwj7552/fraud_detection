library(quantmod)
library(forecast)

getSymbols(c("T"))
ATNT <- T
ticker.lm = lm(T.Low ~ lag(T.High) + T.Open + lag(T.Low) + lag(T.Close)   , data = T)
summary(ticker.lm)
subset(ATNT, row.names(ATNT)>"2010-1-1")
subset='2007::2008-01'
predict.lm(ticker.lm)

ticker.lm = lm(T.Low ~ lag(T.High) + lag(T.Open) + lag(T.Low) + lag(T.Close)   , data = T)
predict.lm(ticker.lm)
predict.lm()

forecast(ticker.lm, h=1, level=90, fan=FALSE, robust=FALSE, lambda=NULL, find.frequency=FALSE, 
         allow.multiplicative.trend=FALSE)

lastday <- T[nrow(T),]
forecast.lm(ticker.lm, lastday, level=c(80,95))

Arima(T)
