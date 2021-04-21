library(corrgram)
library(astsa)
library(TSA)
library(tseries)
library(forecast)

Bitcoin_data <- read.table("BitcoinDataCoinDesk.csv", header = TRUE, sep = ",", 
                           stringsAsFactors = FALSE)

closing_price <- Bitcoin_data[,3]

plot.ts(closing_price, ylab = "Closing Price (USD)") #Looks non-stationary
adf.test(closing_price, k = 0) #Definitely not stationary

#Taking the log-prices, due to financial prospects. 
logprice <- log(closing_price)
adf.test(logprice, k = 0) #Dickey-Fuller test if stationary
#p-val = 0.7886, hence NOT stationary, plot to see 
plot.ts(logprice, ylab = "Log-Prices (USD)") #The time series is definitely not stationary
acf(logprice) #Slow decrease suggest good idea to difference the log-prices.

#Taking the first difference of the log-prices, to make the time series stationary 
logprice.df <- diff(logprice)
plot.ts(logprice.df, ylab = expression(paste(nabla, "log-Price"))) 
#Looks more like a stationary time series

adf.test(logprice.df, k = 0) #Dickey-Fuller test with p-val < 0.01, hence stationary. 
kpss.test(logprice.df) #Here, the null hypothesis is that the time series is stationary

acf2(logprice.df) #The ACF and PACF for the first difference of the logged prices

fit <- auto.arima(logprice.df) #R's build-in function, to estimate the order of the ARMA model
#Hence, the model should be an ARMA(3,2).






#plot(forecast, xlim = c(2000, 3500))
#fit1 <- auto.arima(logprice); fit1
#plot(fit1)
#forecast1 <- forecast(fit1, h = 100) 
#plot(forecast1, xlim = c(2000, 4500))
