library(forecast)
mydata2 = read.csv("C:/Users/DELL/Desktop/HPQ.csv",header = TRUE)
View(mydata2)
spy = ts(mydata2[,2],frequency = 21)

#TESTING & TRAINING DATA

spy_train = window(spy,end=c(19,21))
View(spy_train)
spy_test = window(spy,start = c(20,1))
View(spy_test)

# Training and testing ranges chart

plot(spy,main=" HPQ - ADJUSTED CLOSE: 2019-2021",ylab="ADJUSTED CLOSE",xlab="MONTH")
lines(spy_train,col="red")
lines(spy_test,col="green")
legend("bottomright",col=c("red","green"),lty=1,legend=c("Training","Testing"))


#NAIVE METHOD
naive_ac = naive(spy_train,h=105)
plot(naive_ac)
accuracy(naive_ac,spy_test)

#SEASONAL NAIVE
snaive_ac = snaive(spy_train,h=105)
plot(snaive_ac)
accuracy(snaive_ac,spy_test)

#EXPONENTIAL SMOOTHING METHOD

brown1 <- ses(spy_train,h=105) 
plot(brown1,main=" HPQ - Brown Simple Exponential Smoothing ETS(A,N,N)",ylab="Price",xlab="Month")
lines(spy,lty=3)
accuracy(brown1,spy_test)

# Holts Linear trend Method (A, A, N)

holt1 <- holt(spy_train,h=105)
plot(holt1,main=" HPQ - Holt Linear Trend ETS(A,A,N)",ylab="Adjusted Close",xlab="Month")
lines(spy,lty=3)
accuracy (holt1, spy_test)

# Holt-Winters Additive Method ETS(A,A,A)

hwa1 <- hw(spy_train,h=105,seasonal="additive")
plot(hwa1,main=" HPQ - Holt-Winters Additive ETS(A,A,A) Method 1",ylab="Adjusted Close",xlab="Month")
lines(spy,lty=3)
accuracy (hwa1, spy_test)



# Holt-Winters Multiplicative Method ETS(A,A,M)

hwm1 <- hw(spy_train,h=105,seasonal="multiplicative")
plot(hwm1,main=" HPQ - Holt-Winters Multiplicative ETS(A,A,M) Method 1",ylab="Adjusted Close",xlab="Month")
lines(spy,lty=3)
accuracy (hwm1, spy_test)

# Gardner Additive Damped Trend Method ETS(A,Ad,N)

gardner1 <- holt(spy_train,h=105,damped=T)
plot(gardner1,main=" HPQ - Gardner Additive Damped Trend ETS(A,Ad,N) Method 1",ylab="Adjusted Close",xlab="Month")
lines(spy,lty=3)
accuracy (gardner1, spy_test)

# Taylor Multiplicative Damped Trend Method ETS(A,Md,N)


taylor1 <- holt(spy_train,h=105,exponential=T,damped=T)
plot(taylor1,main=" HPQ - Taylor Multiplicative Damped Trend ETS(A,Md,N) Method 1",ylab="Adjusted Close",xlab="Month")
lines(spy,lty=3)
accuracy (taylor1, spy_test)

# Holt-Winters Damped Method ETS(A,Ad,M)

hwm3 <- hw(spy_train,h=105,damped = T)
plot(hwm3,main="HPQ- Holt-Winters Damped ETS(A,A,M)",ylab="Adjusted Close",xlab="Month")
lines(spy,lty=3)
accuracy (hwm3, spy_test)

