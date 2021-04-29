#install.packages("itsmr")
library(corrgram)
library(astsa)
library(TSA)
library(tseries)
library(xts)
library(forecast)
library(fGarch)
library(ggplot2)
library(itsmr)
library(zoo)


Bitcoin_df<- read.table("BitcoinDataCoinDesk.csv", header = TRUE, sep = ",", 
                           stringsAsFactors = FALSE)

Bitcoin_df$Date <- as.Date(Bitcoin_df$Date)

closing_price <- xts(Bitcoin_df$Closing.Price..USD., Bitcoin_df$Date)

#BTCData <- ts(closing_price, frequency = 12, start = c(2013,1))

plot(closing_price, ylab = "Closing Price (USD)") 

#Looks non-stationary
adf.test(closing_price, k = 0) #DF test, definitely not stationary
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
head(logreturn)
logreturn <- logreturn[2:2743]
plot(logreturn, ylab = "log-return") 
#Looks more like a stationary time series. Using the three unit root tests to check

adf.test(logreturn, k = 0) #Dickey-Fuller test with p-val < 0.01, hence stationary. 
adf.test(logreturn) #Augmented DF, stationary
pp.test(logreturn) #Phillips & Perron test, stationary

acf2(logreturn) #The ACF and PACF for the first difference of the logged prices
acf(logreturn)
pacf(logreturn)

fit <- auto.arima(logreturn); fit #R's build-in function, to estimate the order of the ARMA model
#Hence, the model should be an ARMA(3,2). The model is picked based on AIC
forecastauto <- forecast::forecast(fit, h = 10)
plot(forecastauto)
summary(fit)
asympcovar <- fit$var.coef #Asymptotic covariance matrix
res <- fit$residuals #Residuals from model
tsdisplay(res) #Diagnostic plots of residuals 

ts.plot(logreturn, main = "Daily BTC and Prediction")
ar <- logreturn - res
points(ar, type = "l", col = "red", lty = 2)

#arma.model <- Arima(logreturn, include.mean = FALSE, model = fit)
#summary(arma.model)
#residuals <- as.vector(arma.model$residuals)
#plot(residuals)
#qqnormPlot(residuals) ##QQ-plot of the residuals 
#acf(residuals)
#arma.model$var.coef


checkresiduals(fit)
autoplot(fit)
Box.test(resid(fit)^2, type = "Ljung")
acf(resid(fit)^2)

BTC_garch <-  ugarchspec(variance.model = list(model = "sGARCH",         #Other options are egarch, fgarch, etc.
                                                 garchOrder = c(1,1)), # You can modify the order GARCH(m,s) here
                           mean.model = list(armaOrder = c(3,2)), #Specify your ARMA model implying your model should be stationary.
                           distribution.model = "norm")         #Other distribution are "std" for t-distribution, and "ged" for General Error Distribution

BTC_Garch2 <- ugarchfit(spec = BTC_garch , 
                          data = logreturn, out.sample = 10)

modelfore = ugarchforecast(BTC_Garch2, data = NULL, n.ahead = 1, n.roll
                        = 6, out.sample = 10)

plot(modelfore)



plot(modelfore@forecast$seriesFor)
ARMA <- arima(logreturn, order = c(3,0,2))
summary(ARMA)
arma.model1 <- specify(ar = c(1.1891, -0.2973, 0.0320), ma = c(-1.2244, 0.3213))
check(arma.model1) #The given model is both causal and invetible 

