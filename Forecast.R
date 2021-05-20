library(corrgram); library(astsa); library(TSA); library(tseries); library(xts); library(rugarch)
library(forecast); library(fGarch); library(ggplot2); library(itsmr); library(zoo); library(quantmod)

Bitcoin_df<- read.table("BitcoinDataCoinDesk.csv", header = TRUE, sep = ",", 
                        stringsAsFactors = FALSE)

dates <- as.Date(Bitcoin_df$Date)

td <- seq(as.Date("2013-10-01"), as.Date("2021-04-19"), "days")

closing_price <- zoo(x = Bitcoin_df$Closing.Price..USD., order.by = td)

return <- diff(closing_price) / closing_price[1:2757]
adf.test(return, k = 0)
adf.test(return)
pp.test(return)
arma_fit <- auto.arima(return, stepwise = FALSE, approximation = FALSE, 
                       allowdrift = FALSE, allowmean = FALSE, trace = TRUE) #ARMA(4,1)

Train <- return[1:2747]
Test <- return[2748:2757]
forecast <- forecast::forecast(arma_fit, h = 1); forecast

T <- 2756

fitted <- return - residuals(arma_fit)
plot(return); points(fitted, type = "l", col = "red", lty = 2)

#Estimate model excluding the out of sample
out_of_sample <- round(T/100) 
dates_out_of_sample <- tail(index(return), out_of_sample)
arma_spec = arfimaspec(mean.model = list(armaOrder = c(4,1), include.mean = FALSE))
arma_fit <- arfimafit(spec = arma_spec, data = return, out.sample = out_of_sample)
coef(arma_fit)

#Forecast returns along the whole out-of-sample
arma_fore <- arfimaforecast(arma_fit, n.ahead = 1, n.roll = out_of_sample-1)
forecast_returns <- xts(arma_fore@forecast$seriesFor[1, ], dates_out_of_sample)

#Recover prices
prev_price <- head(tail(return, out_of_sample+1), out_of_sample)
forecast_prices <- xts(prev_price + arma_fore@forecast$seriesFor[1, ], 
                           dates_out_of_sample)
price <- xts(diffinv(as.vector(return))[-1], order.by = index(return))

# plot of returns
plot(cbind("fitted"   = fitted(arma_fit),
           "forecast" = forecast_returns,
           "original" = return), 
     col = c("blue", "red", "black"), lwd = c(0.5, 0.5, 2),
     main = "Forecast of Returns", legend.loc = "topleft")

# plot of prices
plot(cbind("forecast" = forecast_prices,
           "original" = price), 
     col = c("red", "black"), lwd = c(0.5, 0.5, 2),
     main = "Forecast of Prices", legend.loc = "topleft")

#Defining training and test data
T <- length(return)
T_trn <- round(0.75*T)
T_tst <- T - T_trn

out_of_sample <- round(T_tst)
dates_out_of_sample <- tail(index(return), out_of_sample)
garch_spec <- ugarchspec(mean.model = list(armaOrder = c(4,1), include.mean = TRUE),
                         variance.model = list(model = "sGARCH", garchOrder = c(1,1)))

garch_fit <- ugarchfit(spec = garch_spec, data = return, out.sample = out_of_sample)
coef(garch_fit)
garch_fore <- ugarchforecast(garch_fit, n.ahead = 1, n.roll = out_of_sample-1)

forecast_returns <- xts(garch_fore@forecast$seriesFor[1, ], dates_out_of_sample)
forecast_volatility <- xts(garch_fore@forecast$sigmaFor[1, ], dates_out_of_sample)

plot(cbind("fitted"   = fitted(garch_fit),
           "forecast" = forecast_returns,
           "original" = return), 
     col = c("blue", "red", "black"), lwd = c(0.5, 0.5, 2),
     main = "Forecast of returns", legend.loc = "topleft")
