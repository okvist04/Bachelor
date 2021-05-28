library(corrgram)
library(astsa); library(TSA)
library(tseries); library(xts)
library(forecast); library(fGarch)
library(ggplot2); library(timeSeries)
library(itsmr); library(rugarch)
library(zoo); library(MLmetrics)

Bitcoin_df<- read.table("BitcoinDataCoinDesk.csv", header = TRUE, sep = ",", 
                           stringsAsFactors = FALSE)

dates <- as.Date(Bitcoin_df$Date)

td <- seq(as.Date("2013-10-01"), as.Date("2021-04-19"), "days")

closing_price <- zoo(x = Bitcoin_df$Closing.Price..USD., order.by = td)

plot(closing_price, ylab = "Price (USD)", xlab = "Years", main = "Closing Prices",
     col = "red", lwd = 1) # Looks non-stationary

adf.test(closing_price, k = 0) # DF test, definitely not stationary
adf.test(closing_price) # Augmented DF, NOT stationary
pp.test(closing_price) # Phillips & Perron test, NOT stationary
acf(closing_price) # Auto correlation plot of closing prices 

# Finding the returns. 
return <- diff(closing_price) / closing_price[1:2757]
adf.test(return, k = 0) # Dickey-Fuller test if stationary, p-val < 0.01, hence stationary
adf.test(return) # Augmented Dickey-Fuller, stationary
pp.test(return) # Phillips & Perron test, stationary
plot(return, ylab = "Returns (USD)", xlab = "Years", main = "Simple Net Returns ") 
# The time series appear to be stationary
acf(return) 

arima.mod <- auto.arima(return, trace = TRUE, approximation = FALSE, stepwise = FALSE, allowmean = FALSE, 
           allowdrift = FALSE) #ARIMA(4,0,1)
# R's build-in function, to estimate the order of the ARMA model
# Hence, the model should be an ARMA(4,1). The model is picked based on AIC

coef(arima.mod) # Due to the coefficients being close to zero, the MSE is calculated

MSE(arima.mod$fitted, return) # MSE = 0.001874437 

MSE(0, return) # MSE = 0.001886479, almost the same as for the model. Hence, evidence in favor of EMH. 



