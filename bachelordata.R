#install.packages("itsmr")
library(corrgram); library(astsa); library(TSA); library(tseries); library(xts)
library(forecast); library(fGarch); library(ggplot2); library(itsmr); library(zoo)


Bitcoin_df<- read.table("BitcoinDataCoinDesk.csv", header = TRUE, sep = ",", 
                           stringsAsFactors = FALSE)
Bitcoin_df$Date <- as.Date(Bitcoin_df$Date)

closing_price <- xts(Bitcoin_df$Closing.Price..USD., Bitcoin_df$Date)

plot(closing_price, ylab = "Closing Price (USD)") 
#Looks non-stationary
adf.test(closing_price, k = 0) #Definitely not stationary
adf.test(closing_price) #Augmented DF, NOT stationary
pp.test(closing_price) #Phillips & Perron test, NOT stationary

#Taking the log-prices, due to financial prospects. 
logprice <- log(closing_price)
adf.test(logprice, k = 0) #Dickey-Fuller test if stationary, p-val = 0.7886, hence NOT stationary
adf.test(logprice) #Augmented Dickey-Fuller, NOT stationary
pp.test(logprice) #Phillips & Perron test, NOT stationary

plot(logprice, ylab = "Log-Prices (USD)") #The time series is definitely not stationary
acf(logprice) #Slow decrease suggest good idea to difference the log-prices.

#Taking the first difference of the log-prices, to make the time series stationary 
logreturn <- diff(logprice)
logreturn <- logreturn[2:2743]
plot(logreturn, ylab = "log-return") 
#Looks more like a stationary time series. Using the three unit root tests to check

adf.test(logreturn, k = 0) #Dickey-Fuller test with p-val < 0.01, hence stationary. 
adf.test(logreturn) #Augmented DF, stationary
pp.test(logreturn) #Phillips & Perron test, stationary

acf2(logreturn) #The ACF and PACF for the first difference of the logged prices
acf(logreturn)
pacf(logreturn)

fit <- auto.arima(logreturn) #R's build-in function, to estimate the order of the ARMA model
#Hence, the model should be an ARMA(3,2).
summary(fit)
ARMA <- arima(logreturn, order = c(3,0,2))
summary(ARMA)

arma.model <- specify(ar = c(1.1751, -0.2757, 0.0335), ma = c(-1.2092, 0.2991))
check(arma.model) #The given model is both causal and invetible 

