install.packages("fpp2")
library(fpp2)

#questionA

plot(auscafe,type="p")
plot(austourists,type = "p")

#questionB

acf(auscafe,lag.max=9)
acf(austourists,lag.max=9)

#questionC

install.packages("pastecs")
library(pastecs)

mad(auscafe)
median(auscafe)

stat.desc(auscafe)

#questionD
#partA

auscafe
cafe1 = window(auscafe,1982,2010)
cafe2 = naive(cafe1,12)
cafe3 = snaive(cafe1,12)
plot(cafe1,main = "forecast")
lines(cafe2$mean,col = 2)
lines(cafe3$mean,col = 3)
lines(auscafe)
legend("topright", lty=1, col=c(4,2,3),
       legend = c("naive method", "seasonal naive method"))
plot(cafe2)
plot(cafe3)

cafe5 = window(auscafe,start=2010)
accuracy(cafe2,cafe5)
accuracy(cafe3,cafe5)

#b part

austourists
dim(austourists)
cafe4 = window(auscafe,1999,2013)
cafe6 = naive(cafe4,12)
cafe7 = snaive(cafe4,12)
plot(cafe4,main = "forecast")
lines(cafe6$mean,col = 2)
lines(cafe7$mean,col = 3)
lines(austourists)
legend("topright", lty=1, col=c(4,2,3),
       legend = c("naive method", "seasonal naive method"))
plot(cafe6)
plot(cafe7)

cafe8 = window(auscafe,start=2013)
accuracy(cafe6,cafe8)
accuracy(cafe7,cafe8)

#QuestionF

#tarining and testing data

auscafe
auscafetraining <- window(auscafe,start = 1982,end=2010,12)
auscafeTest <- window(auscafe,start = 2010,end=2011)

length(auscafetraining)

# Training and testing ranges chart
plot(spy,main="SPY 2007-2015",ylab="Price",xlab="Month")
lines(spyt,col="blue")
lines(spyf,col="green")
legend("bottomright",col=c("blue","green"),lty=1,legend=c("Training","Testing"))

# brown Simple Exponential Smoothing (A, N, N)
brown1 <- ses(auscafetraining,h=12) 
plot(brown1,main="Brown Simple Exponential Smoothing ETS(A,N,N) Method 1",ylab="Price",xlab="Month")
lines(auscafe,lty=3)

# Holts Linear trend Method (A, A, N)
holt1 <- holt(auscafetraining,h=12)
plot(holt1,main="Holt Linear Trend ETS(A,A,N) Method 1",ylab="Price",xlab="Month")
lines(auscafe,lty=3)

# Holt-Winters Additive Method ETS(A,A,A)
hwa1 <- hw(auscafetraining,h=12,seasonal="additive")
plot(hwa1,main="Holt-Winters Additive ETS(A,A,A) Method 1",ylab="Price",xlab="Month")
lines(auscafe,lty=3)


# Exponential Trend Method ETS(A,M,N)
exp1 <- holt(auscafetraining,h=12,exponential=T)
plot(exp1,main="Exponential Trend ETS(A,M,N) Method 1",ylab="Price",xlab="Month")
lines(auscafe,lty=3)

# Holt-Winters Multiplicative Method ETS(A,A,M)

hwm2 <- hw(auscafetraining,h=12,seasonal="multiplicative")
plot(hwm2,main="Holt-Winters Multiplicative ETS(A,A,M) Method 1",ylab="Price",xlab="Month")
lines(auscafe,lty=3)

# Holt-Winters Damped Method ETS(A,A,M)

hwm3 <- hw(auscafetraining,h=12,damped = T)
plot(hwm3,main="Holt-Winters Multiplicative ETS(A,A,M) Method 1",ylab="Price",xlab="Month")
lines(auscafe,lty=3)

accuracy(brown1,auscafeTest)
accuracy(holt1, auscafeTest)
accuracy(hwa1, auscafeTest)
accuracy(exp1, auscafeTest)
accuracy(hwm2, auscafeTest)
accuracy(hwm3,auscafeTest)

#Question h

library(forecast)
library(tseries)
adf.test(auscafetraining,alternative= "stationary")
adf.test(auscafeTest,alternative= "stationary")

#question i
austouriststraining <- window(austourists, start = 1999, end=2013, 12)
austouristsTest <- window(austourists, start = 2013, end=2014)


adf.test(diff(austouriststraining),alternative= "stationary")
acf(diff(austouriststraining))
pacf(diff(austouriststraining))

