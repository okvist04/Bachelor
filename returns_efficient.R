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

dates <- as.Date(Bitcoin_df$Date, format = "%Y-%m-%d")

td <- seq(from = as.Date("2013-10-01"), to = as.Date("2021-04-19"), by = "days")
td.14 <- seq(from = as.Date("2014-01-01"), to = as.Date("2014-12-31"), by = "days")
td.15 <- seq(from = as.Date("2015-01-01"), to = as.Date("2015-12-31"), by = "days")
td.17 <- seq(from = as.Date("2017-01-01"), to = as.Date("2017-12-31"), by = "days")

closing_price <- zoo(x = Bitcoin_df$Closing.Price..USD., order.by = td)

return <- diff(closing_price)

#Forecasting the inefficient years, i.e., 2014, 2015, and 2017
t.14 <- window(return, start = as.Date("2014-01-01"), end = as.Date("2014-12-31"))
t.15 <- window(return, start = as.Date("2015-01-01"), end = as.Date("2015-12-31"))
t.17 <- window(return, start = as.Date("2017-01-01"), end = as.Date("2017-12-31"))

ar.14 <- ar.mle(t.14, aic = TRUE, order.max = NULL, intercept = TRUE) #AR(2)
ar.15 <- ar.mle(t.15, aic = TRUE, order.max = NULL, intercept = TRUE) #AR(7)
ar.17 <- ar.mle(t.17, aic = TRUE, order.max = NULL, intercept = TRUE) #AR(12)

plot(y = t.14, x = td.14, xaxt = "n", type = "l",
     ylab = "Returns", xlab = "Months", main = "Returns in 2014"); axis(side = 1, at = c(16101, 16129, 16160,
                                                                         16190, 16221, 16251, 16282,
                                                                         16313, 16343, 16374, 16404, 16435), 
                                            labels = c("Jan", "Feb", "Mar", "Apr", "May","Jun", 
                                                       "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), las = 2)
plot(y = t.15, x = td.15, xaxt = "n", type = "l",
     ylab = "Returns", xlab = "Months", main = "Returns in 2015")
axis(side = 1, at = c(16466, 16494, 16525, 16555, 16586, 16616, 16647, 16678, 16708, 16739, 16769, 16800),
     labels = c("Jan", "Feb", "Mar", "Apr", "May","Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), las = 2)

plot(y = t.17, x = td.17, xaxt = "n", type = "l",
     ylab = "Returns", xlab = "Months", main = "Returns in 2017")
axis(side = 1, at = c(17197, 17225, 17256, 17286, 17317, 17347, 17378, 17409, 17439, 17470, 17500, 17531),
     labels = c("Jan", "Feb", "Mar", "Apr", "May","Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), las = 2)

acf(t.14)
pacf(t.14)
acf(t.15)
pacf(t.15)
acf(t.17)
pacf(t.17)


