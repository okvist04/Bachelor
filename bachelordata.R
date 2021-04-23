library(corrgram)
library(astsa)
library(TSA)
library(tseries)
library(forecast)
library(fGarch)
library(ggplot2)

Bitcoin_data <- read.table("BitcoinDataCoinDesk.csv", header = TRUE, sep = ",", 
                           stringsAsFactors = FALSE)
head(Bitcoin_data$Date)
Bitcoin_data$Date <- as.Date(Bitcoin_data$Date, format = "%Y-%m-%d")
ggplot(data = Bitcoin_data, aes(x = Date, y = Closing.Price..USD.)) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(title = "Total daily precipitation in Boulder, Colorado",
       subtitle = "Fall 2013",
       x = "Date", y = "Daily Precipitation (Inches)")

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
