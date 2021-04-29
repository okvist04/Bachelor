install.packages("rugarch")
library(corrgram); library(astsa); library(TSA); library(tseries); library(xts); library(rugarch)
library(forecast); library(fGarch); library(ggplot2); library(itsmr); library(zoo); library(quantmod)

Bitcoin_df<- read.table("BitcoinDataCoinDesk.csv", header = TRUE, sep = ",", 
                        stringsAsFactors = FALSE)

Bitcoin_df$Date <- as.Date(Bitcoin_df$Date)

closing_price <- xts(Bitcoin_df$Closing.Price..USD., Bitcoin_df$Date)

logreturn <- diff(log(closing_price))
logreturn <- logreturn[2:2743]

T <- 2743

# try different combinations
arma_fit <- autoarfima(data = logreturn, ar.max = 5, ma.max = 5, include.mean = TRUE, 
                       criterion = "BIC", method = "full")  # "AIC","BIC","SIC","HQIC"
# see the ranking of the combinations
arma_fit$rank.matrix

armaOrder <- arma_fit$rank.matrix[1, c("AR","MA")]
armaOrder

# estimate model excluding the out of sample
out_of_sample <- round(T/100) #Predicta little less observations 
dates_out_of_sample <- tail(index(logreturn), out_of_sample)
arma_spec = arfimaspec(mean.model = list(armaOrder = c(5,5), include.mean = TRUE))
arma_fit <- arfimafit(spec = arma_spec, data = logreturn, out.sample = out_of_sample)
coef(arma_fit)

# forecast log-returns along the whole out-of-sample
arma_fore <- arfimaforecast(arma_fit, n.ahead = 1, n.roll = out_of_sample-1)
forecast_log_returns <- xts(arma_fore@forecast$seriesFor[1, ], dates_out_of_sample)

# recover log-prices
prev_log_price <- head(tail(logreturn, out_of_sample+1), out_of_sample)
forecast_log_prices <- xts(prev_log_price + arma_fore@forecast$seriesFor[1, ], 
                           dates_out_of_sample)

logprice <- xts(diffinv(logreturn)[-1], order.by = index(logreturn))
# plot of log-returns
plot(cbind("fitted"   = fitted(arma_fit),
           "forecast" = forecast_log_returns,
           "original" = logreturn), 
     col = c("blue", "red", "black"), lwd = c(0.5, 0.5, 2),
     main = "Forecast of log-returns", legend.loc = "topleft")
# plot of log-prices
plot(cbind("forecast" = forecast_log_prices,
           "original" = logprice), 
     col = c("red", "black"), lwd = c(0.5, 0.5, 2),
     main = "Forecast of log-prices", legend.loc = "topleft")

