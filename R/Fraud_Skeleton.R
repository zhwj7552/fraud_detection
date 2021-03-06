#To get the sample data I needed .arff 
library("rJava")
library("RWeka")
getwd()
setwd("C:/Users/rvane/Documents/fraud_detection/Data")
read.arff("credit_fruad.arff")
read.arff(url("http://weka.8497.n7.nabble.com/file/n23121/credit_fruad.arff", package = "RWeka"))

#Injest the data
library(RODBC)
read.csv() #select file (also save excel as csv)
odbcConnectExcel2007(xls.file='test.xlsx', readOnly=T)


#fill first one manually
for i in 2:nrow(DF) {
  DF$open[i] <- #open price
    DF$hold[i] <- DF$hold[i-1]
  
}

library(dplyr)
FraudTest <- as.data.frame.ts(ATNT)
FraudTest <- mutate(FraudTest, SD.Adjusted = (T.Adjusted-lag(T.Adjusted))/sd(T.Adjusted))
hist(FraudTest$SD.Adjusted)


#Hodrick Prescott Filter

hpfilter <- function(x, lambda=1600) {
  eye <- diag(length(x))
  result <- solve(eye+lambda*crossprod(diff(eye, lag=1, d=2)))
  return(result)
}

library(fracdiff)
fracdiff(y, nar=2, nma=1)

a <- arima(y,order = c(1,1,2))
predict(a, 5)  #predicts 5 periods forward

#better than hist
d <- density(y)
plot(d, main = "Kernal Density Estimate of Y")

library(stepfun)
d <- ecdf(y) #density function
plot(d, main = "Empirical Cumulative Density Function")

#make a function for predicted vs True

plot(
  lines(a.predict$pred, lty=2, type = "1"),
  lines(+stderr(a)),
  lines(-stderr(a)),
)

plot(
  lines(profit)
  arrows(from, to)
  points()
  jpg("mygraph.jpg")
)

proc.time() #figure out how fast we are running
