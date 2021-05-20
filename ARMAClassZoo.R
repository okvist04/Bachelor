library(timeSeries)
library(timeDate)
library(tseries)
Bitcoin_df<- read.table("BitcoinDataCoinDesk.csv", header = TRUE, sep = ",", 
                        stringsAsFactors = FALSE)

dates <- as.Date(Bitcoin_df$Date)

#hist(dates, breaks="months", freq=TRUE, main="Distribution of Dates by Month",
     #col="slateblue1", xlab="", format = "%b %Y", las=2)

td <- seq(as.Date("2013-10-01"), as.Date("2021-04-19"), "days")

BTC.z <- zoo(x = Bitcoin_df$Closing.Price..USD., order.by = td)
#window(BTC.z, start = as.Date("2013-10-01"), end = as.Date("2014-10-01"))
plot(BTC.z, col = "blue", lty = 1, lwd = 2)

adf.test(BTC.z, k = 0)
adf.test(BTC.z)
pp.test(BTC.z)

logprice <- log(BTC.z)
plot(logprice, lty = 1, lwd = 2)
adf.test(logprice, k = 0)
adf.test(logprice)
pp.test(logprice)

acf2(logprice) #Take first difference

logreturn <- diff(logprice)
plot(logreturn)
adf.test(logreturn, k = 0)
adf.test(logreturn)
pp.test(logreturn)

acf2(logreturn)

model <- auto.arima(logreturn, trace = TRUE, ic = "aic", approximation = FALSE,
                    stepwise = FALSE); model
stand.res <- rstandard(model)
qqnorm(stand.res); qqline(stand.res, col = "red")

#Box.test(stand.res, type = "Ljung-Box", lag = 10) #Chi-squared value = 39.645
#qchisq(p = 0.05, df = 10, lower.tail = FALSE) 
#Critical value = 31.41, hence the null hypothesis is rejected, 
#and the residuals are not white noise 

summary(model)
forecast::forecast(model, 10)
plot(forecast::forecast(model, 10))













