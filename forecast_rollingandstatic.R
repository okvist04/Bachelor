library(arfima); library(astsa); library(fGarch); library(FitAR); library(forecast)
library(fUnitRoots); library(ie2misc); library(itsmr); library(lmtest); library(MLmetrics)
library(rmgarch); library(timeDate); library(timeSeries); library(TSA); library(tseries)
library(zoo); library(xts); library(RColorBrewer)

Bitcoin_df<- read.table("BitcoinDataCoinDesk.csv", header = TRUE, sep = ",", 
                        stringsAsFactors = FALSE)

dates <- as.Date(Bitcoin_df$Date)
td <- seq(as.Date("2013-10-01"), as.Date("2021-04-19"), "days")

closing_price <- zoo(x = Bitcoin_df$Closing.Price..USD., order.by = td)

return <- diff(closing_price) / closing_price[1:2757]

td.years <- seq(as.Date("2013-10-01"), as.Date("2021-10-01"), "years")
plot(return, ylab = "", xlab = "", col = "#FFCC00", xaxt = "n",
     main = "Returns 02/10/2013 - 18/04/2021")
axis.Date(1, at = td.years, format = "%Y")

arma.mod <- auto.arima(return, approximation = FALSE, stepwise = FALSE, allowmean = FALSE, 
                       allowdrift = FALSE); coeftest(arma.mod) # ARMA(4,1)

# Checking it the found ARMA model is causal and invertible
autoplot(arma.mod) # Causal and invertible

ar.mod <- ar.mle(return, aic = TRUE, order.max = NULL, intercept = TRUE) # AR(10)

# Checking it the found AR model is causal and invertible
autoplot(ar.mod) # Causal

# Calculating the accuracy of the models, first for the auto.arima the for ar.mle
MSE(fitted(arma.mod), return)
accuracy(fitted(arma.mod), return)

# Calculating the fitted values, and finding the accuracy
fitted.ar.mod <- ts(return[11:2756] - ar.mod$resid[11:2756])

MSE(fitted.ar.mod, return[11:2756])
accuracy(fitted.ar.mod, return[11:2756])

# Checking if the residuals are indeed white noise
checkresiduals(arma.mod)
# They are not. 

# Forecasting using ARMA(4,1)
tendaysahead <- read.table("BTC_USD_2021-04-20_2021-04-29-CoinDesk.csv", header = TRUE, sep = ",", 
                        stringsAsFactors = FALSE)

tenaheaddates <- as.Date(tendaysahead$Date)

td.tenahead <- seq(as.Date("2021-04-20"), as.Date("2021-04-29"), "days")

closing_price.tenahead <- zoo(x = tendaysahead$Closing.Price..USD., order.by = td.tenahead)

tendaysahead.return <- diff(closing_price.tenahead) / closing_price.tenahead[1:9]

t.all <- seq(as.Date("2013-01-01"), as.Date("2021-04-29"), by = "month")

arma.forecast <- forecast::forecast(arma.mod, h = 10, level = c(80, 95))
par(mfrow = c(1,1))
plot(arma.forecast, xaxt = "n", main = "10 Days ahead Forecast, ARMA(4,1)", xlim = c(18300, 18733))
lines(tendaysahead.return, type = "l", col = "green", 
      lty = 2); axis.Date(1, at = t.all, format= "%m/%Y", las = 2)
legend("topleft", legend = c("Actual Returns 20/02 - 18/04/21", 
                                "Forecast", "Actual Returns 20/04/21 - 28/04/21"),
       col = c("black", "steelblue", "green"), lty = c(1,1,2), cex = 0.55, box.lty = 0)

par(mfrow = c(1,1))
McLeod.Li.test(arma.mod, main = "McLeod-Li test statistics for Returns") 
# Shows signs of conditional heteroskedasticity

abs.return <- abs(return)
acf(abs.return, main = "Absolute Value of Returns") # Shows signs of ARCH effect
sqr.return <- return^2
acf(sqr.return) # Shows signs of ARCH effect

# ARMA(3,2)-GARCH(4,2) lowest BIC = -3.77051
arma32.garch42 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,2)),
                             mean.model = list(armaOrder = c(3,2), include.mean = TRUE),
                             distribution.model = "norm")
arma32.garch42.fit <- ugarchfit(spec = arma32.garch42, data = return, 
                                out.sample = 100, solver = "hybrid")

par(mfrow = c(4,3))
plot(arma32.garch42.fit, which = "all")
arma32.garch42.fit 
# Ljung-Box Test shows that the null hypothesis is accepted for GARCH part, 
# but not for no serial correlation

par(mfrow = c(1,1))
# Observing if the above conclusion is correct 
stand.res <- residuals(arma32.garch42.fit, standardize = TRUE)
acf(stand.res, main = "Standardised Residuals")

sqr.stand.res <- stand.res^2
acf(sqr.stand.res, main = "Squared Standardised Residuals")

# Calculating the accuracy of the model 
MSE(arma32.garch42.fit@fit$fitted.values, return)
accuracy(arma32.garch42.fit@fit$fitted.values, return[2656:2757])

# Forecasting; static and rolling, conditional mean and sigma
forecast_arma32garch42 <- ugarchforecast(arma32.garch42.fit, 
                                         data = return, n.ahead = 10, n.roll = 100)
print(forecast_arma32garch42)
plot(forecast_arma32garch42, which = "all")

######################################################################################
# Based on AMIM value, the models are made based on the inefficient years, i.e., 2014 & 2015
######################################################################################
t2 <- window(return, start = as.Date("2014-01-01"), end = as.Date("2014-12-31")) #Inefficient
t3 <- window(return, start = as.Date("2015-01-01"), end = as.Date("2015-12-31")) #Inefficient

######################################################################################
# ARMA-GARCH model for 2014, static and rolling forecast
# Based on BIC, the best model is ARMA(2,4)-GARCH(1,1), BIC = -3.797563
######################################################################################
return2014 <- return[92:456]

arma.mod2014 <- auto.arima(return2014, approximation = FALSE, stepwise = FALSE, allowmean = FALSE, 
                       allowdrift = FALSE); coeftest(arma.mod2014) # AR(2)

# Checking it the found AR model is causal and invertible
autoplot(arma.mod2014) # Causal

ar.mod2014 <- ar.mle(return2014, aic = TRUE, order.max = NULL, intercept = TRUE) #AR(2)

# Checking it the found AR model is causal and invertible
autoplot(ar.mod2014) # Causal

# Calculating the accuracy of both models, first from auto.arima then from ar.mle
MSE(fitted(arma.mod2014), return2014)
accuracy(fitted(arma.mod2014), return2014)

MSE(fitted(ar.mod2014)[3:365], return2014[3:365])
accuracy(fitted(ar.mod2014)[3:365], return2014[3:365])

checkresiduals(arma.mod2014) 
checkresiduals(ar.mod2014); Box.test(residuals(ar.mod2014), type = "Ljung-Box")
# The residuals are indeed white noise.

t.2014 <- seq(as.Date("2014-01-01"), by = "month", 
              along.with = c(return2014, return[457:821]))
        
par(mfrow = c(1,1))

# Forecasting based on ARMA(2,0,0), auto.arima
arma.forecast2014 <- forecast::forecast(arma.mod2014, h = 10, level = c(80,95))
plot(arma.forecast2014, xaxt = "n", main = "10 Days ahead Forecast, AR(2)")
lines(return[456:465], type = "l", 
      col = "green", lty = 2); axis.Date(1, at = t.2014, format= "%m/%Y", las = 2)
legend("bottom", legend = c("Actual, 2014", "Forecast", "Actual, 2015"),
       col = c("black", "steelblue", "green"), lty = c(1,1,2), cex = 0.55, box.lty = 0)

# Forecasting based on AR(2), ar.mle will be equal to the above plot

# Checking for signs of conditional heteoryskedasticity, i.e., GARCH effects
par(mfrow = c(1,1))
McLeod.Li.test(arma.mod2014, main = "McLeod-Li test statistics for 2014 Returns") 
# Shows signs of conditional heteroskedasticity

abs.return2014 <- abs(return2014); acf2(abs.return2014) # Shows signs of ARCH effect
sqr.return2014 <- return2014^2; acf2(sqr.return2014) # Shows signs of ARCH effect

arma24.garch11 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                             mean.model = list(armaOrder = c(2,4), include.mean = TRUE),
                             distribution.model = "norm")
arma24.garch11.fit <- ugarchfit(spec = arma24.garch11, data = return2014, out.sample = 100,
                                solver = "hybrid")

plot(arma24.garch11.fit, which = "all")
arma24.garch11.fit 
# Ljung-Box Test shows that the null hypothesis is accepted for 
# both no serial correlation and ARCH

# Observing if the above conclusion is correct 
stand.res.14 <- residuals(arma24.garch11.fit, standardize = TRUE)
acf(stand.res.14, main = "Standardized Residuals")

sqr.stand.res.14 <- stand.res.14^2
acf(sqr.stand.res.14, main = "Squared Standardized Residuals")

# Calculating the accuracy of the model
MSE(arma24.garch11.fit@fit$fitted.values, return2014)
accuracy(arma24.garch11.fit@fit$fitted.values, return2014[355:365])

# Forecasting; static and rolling, conditional mean and sigma
forecast_arma24garch11 <- ugarchforecast(arma24.garch11.fit, data = return2014, 
                                         n.ahead = 10, n.roll = 100)
print(forecast_arma24garch11)
plot(forecast_arma24garch11, which = "all")

######################################################################################
# ARMA-GARCH model for 2015, static and rolling forecast
# Based on BIC, the best model is ARMA(3,3)-GARCH(1,1), BIC = -4.068104
######################################################################################
return2015 <- return[457:821]

arma.mod2015 <- auto.arima(return2015, approximation = FALSE, stepwise = FALSE, allowmean = FALSE, 
                           allowdrift = FALSE); coeftest(arma.mod2015) # AR(5)

# Checking it the found AR model is causal and invertible
autoplot(arma.mod2015) # Causal

ar.mod2015 <- ar.mle(return2015, aic = TRUE, order.max = NULL, intercept = TRUE) # AR(8)

# Checking it the found AR model is causal and invertible
autoplot(ar.mod2015) # Causal

# Calculating the accuracy of both models, first from auto.arima then from ar.mle
MSE(fitted(arma.mod2015), return2015)
accuracy(fitted(arma.mod2015), return2015)

MSE(fitted(ar.mod2015)[9:365], return2015[9:365])
accuracy(fitted(ar.mod2015)[9:365], return2015[9:365])

checkresiduals(arma.mod2015) 
checkresiduals(ar.mod2015); Box.test(residuals(ar.mod2015), type = "Ljung-Box")
# The residuals are white noise

t.2015 <- seq(as.Date("2015-01-01"), by = "month", along.with = c(return2015, return[822:1187]))

par(mfrow = c(1,1))

# Forecasting based on ARMA(5,0,0), auto.arima
arma.forecast2015 <- forecast::forecast(arma.mod2015, h = 10, level = c(80,95))
plot(arma.forecast2015, xaxt = "n", main = "10 Days ahead Forecast, AR(5)")
lines(return[822:831], type = "l", 
      col = "green", lty = 2); axis.Date(1, at = t.2015, format= "%m/%Y", las = 2)
legend("bottom", legend = c("Actual, 2015", "Forecast", "Actual, 2016"),
       col = c("black", "steelblue", "green"), lty = c(1,1,2), cex = 0.55, box.lty = 0)

# Forecasting based on AR(8), ar.mle
ar.forecast2015 <- forecast::forecast(ar.mod2015, h = 10, level = c(80,95))
plot(ar.forecast2015, xaxt = "n", main = "10 Days ahead Forecast, AR(8)")
lines(return[822:831], type = "l", 
      col = "green", lty = 2); axis.Date(1, at = t.2015, format= "%m/%Y", las = 2)
legend("bottomleft", legend = c("Actual, 2015", "Forecast", "Actual, 2016"),
       col = c("black", "steelblue", "green"), lty = c(1,1,2), cex = 0.55, box.lty = 0)

# Checking for signs of conditional heteoryskedasticity, i.e., GARCH effects
par(mfrow = c(1,1))
McLeod.Li.test(arma.mod2015, main = "McLeod-Li test statistics for 2014 Returns") 
# Shows signs of conditional heteroskedasticity

arma33.garch11 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                             mean.model = list(armaOrder = c(3,3), include.mean = TRUE),
                             distribution.model = "norm")
arma33.garch11.fit <- ugarchfit(spec = arma33.garch11, data = return2015, out.sample = 100,
                                solver = "hybrid")

plot(arma33.garch11.fit, which = "all")
arma33.garch11.fit 
# Ljung-Box Test shows that the null hypothesis is 
# accepted for both no serial correlation and ARCH

# Observing if the above conclusion is correct 
stand.res.15 <- residuals(arma33.garch11.fit, standardize = TRUE)
acf(stand.res.15, main = "Standardized Residuals")

sqr.stand.res.15 <- stand.res.15^2
acf(sqr.stand.res.15, main = "Squared Standardized Residuals")

# Calculating the accuracy of the model
MSE(arma33.garch11.fit@fit$fitted.values, return2015)
accuracy(arma33.garch11.fit@fit$fitted.values, return2015[355:365])

# Forecasting; static and rolling, conditional mean and sigma
forecast_arma33garch11 <- ugarchforecast(arma33.garch11.fit, 
                                         data = return2015, n.ahead = 10, n.roll = 100)
print(forecast_arma33garch11)
plot(forecast_arma33garch11, which = "all")

######################################################################################
# Based on AMIM value, the models are made based on the inefficient months.
# Three months are chosen, highest: October 16, lowest: May 18, median: September 18
######################################################################################
#Monthly Windows: 
oct2016 <- window(return, start = as.Date("2016-10-01"), end = as.Date("2016-10-31")) # Highest
nov2016 <- window(return, start = as.Date("2016-11-01"), end = as.Date("2016-11-30"))
may2018 <- window(return, start = as.Date("2018-05-01"), end = as.Date("2018-05-31")) # Lowest
jun2018 <- window(return, start = as.Date("2018-06-01"), end = as.Date("2018-06-30"))
sep2018 <- window(return, start = as.Date("2018-09-01"), end = as.Date("2018-09-30")) # Median
oct2018 <- window(return, start = as.Date("2018-10-01"), end = as.Date("2018-10-31"))

######################################################################################
# Month with highest AMIM - Oct. 2016 = 0.5026904, modelling
######################################################################################
arma.oct2016 <- auto.arima(oct2016, trace = TRUE, stepwise = FALSE, approximation = FALSE,
                           allowmean = FALSE, allowdrift = FALSE) # ARMA(0,0,0)

# Checking it the found ARMA model is causal and invertible
autoplot(arma.oct2016) # No roots

ar.oct2016 <- ar.mle(oct2016, aic = TRUE, order.max = NULL, intercept = TRUE) # AR(11)

# Checking it the found AR model is causal and invertible
autoplot(ar.oct2016) # Causal

# Calculating the accuracy of both models, first from auto.arima then from ar.mle
MSE(fitted(arma.oct2016), oct2016)
accuracy(fitted(arma.oct2016), oct2016)

MSE(fitted(ar.oct2016)[12:31], oct2016[12:31])
accuracy(fitted(ar.oct2016)[12:31], oct2016[12:31])

# Checking if the residuals are white noise
checkresiduals(arma.oct2016) 
checkresiduals(ar.oct2016); Box.test(residuals(ar.oct2016), type = "Ljung-Box")
# White noise

par(mfrow = c(1,1))
McLeod.Li.test(arma.oct2016, main = "McLeod-Li test statistics for Returns Oct. 2016")
# No signs of volatility

# Static 10-days ahead forecast based on October 2016, AR(11)
octnov.16 <- as.Date(paste(2016, c(10,11), rep(c(02, 04, 06, 08, 10, 12, 
                                                14, 16, 18, 20, 22, 24, 
                                                26, 28, 30, 31), 2), sep = "-"))

oct16.arforecast <- forecast::forecast(ar.oct2016, h = 10)
plot(oct16.arforecast, xaxt = "n", xaxs = "i", main = "10 Days ahead Forecast, AR(11)")
lines(nov2016, type = "o", col = "green", lty = 2); axis.Date(1, at = octnov.16, format= "%d-%m", las = 1)
legend("topleft", legend = c("Actual, Oct 16", "Forecast", "Actual, Nov 16"),
       col = c("black", "steelblue", "green"), lty = c(1,1,2), cex = 0.6, box.lty = 0)

# Static 10-days ahead forecast based on October 2016, ARMA(0,0)
oct16.maforecast <- forecast::forecast(arma.oct2016, h = 10)
plot(oct16.maforecast, xaxt = "n", xaxs = "i", main = "10 Days ahead Forecast, ARMA(0,0)")
lines(nov2016, type = "o", 
      col = "green", lty = 2); axis.Date(1, at = octnov.16, format= "%d-%m", las = 1)
legend("topleft", legend = c("Actual, Oct 16", "Forecast", "Actual, Nov 16"),
       col = c("black", "steelblue", "green"), lty = c(1,1,2), cex = 0.55, box.lty = 0)

######################################################################################
# Month with smallest AMIM, May 2018, AMIM = 0.0019, modelling
######################################################################################
arma.may2018 <- auto.arima(may2018, trace = TRUE, stepwise = FALSE, approximation = FALSE,
                              allowmean = FALSE, allowdrift = FALSE, ic = "aic"); coeftest(arma.may2018)
#MA(3), ma1 = 0.0375, ma2 = 0.321, ma3 = -0.4121

# Checking it the found AR model is causal and invertible
autoplot(arma.may2018) # Invertible

ar.may2018 <- ar.mle(may2018, aic = TRUE, order.max = NULL, intercept = TRUE) 
#AR(3), ar1 = -0.1247, ar2 = 0.1467, ar3 = -0.3265

# Checking it the found AR model is causal and invertible
autoplot(ar.may2018) # Causal

# Calculating the accuracy of both models, first from auto.arima then from ar.mle
MSE(fitted(arma.may2018), may2018)
accuracy(fitted(arma.may2018), may2018)

MSE(fitted(ar.may2018)[4:31], may2018[4:31])
accuracy(fitted(ar.may2018)[4:31], may2018[4:31])

# Checking if residuals are white noise.
checkresiduals(arma.may2018) 
checkresiduals(ar.may2018); Box.test(residuals(ar.may2018), type = "Ljung-Box")
# White noise 

par(mfrow = c(1,1))
# Testing for GARCH effect
McLeod.Li.test(arma.may2018, main = "McLeod-Li test statistics for Returns May 2018")
# No sign of GARCH effect

# Static 10-days ahead forecast based on May 2018, AR(3)
mayjun.18 <- as.Date(paste(2018, c(5,6), rep(c(02, 04, 06, 08, 10, 12, 
                                                14, 16, 18, 20, 22, 24, 
                                                26, 28, 30, 31), 2), sep = "-"))

may18.arforecast <- forecast::forecast(ar.may2018, h = 10)
plot(may18.arforecast, xaxt = "n", xaxs = "i", main = "10 Days ahead Forecast, AR(3)")
lines(jun2018, type = "o", 
      col = "green", lty = 2); axis.Date(1, at = mayjun.18, format= "%d-%m", las = 1)
legend("topleft", legend = c("Actual, May 18", "Forecast", "Actual, Jun 18"),
       col = c("black", "steelblue", "green"), lty = c(1,1,2), cex = 0.55, box.lty = 0)

# Static 10-days ahead forecast based on May 2018, MA(3)
may18.maforecast <- forecast::forecast(arma.may2018, h = 10)
plot(may18.maforecast, xaxt = "n", xaxs = "i", main = "10 Days ahead Forecast, MA(3)")
lines(jun2018, type = "o", 
      col = "green", lty = 2); axis.Date(1, at = mayjun.18, format= "%d-%m", las = 1)
legend("topleft", legend = c("Actual, May 18", "Forecast", "Actual, Jun 18"),
       col = c("black", "steelblue", "green"), lty = c(1,1,2), cex = 0.7, box.lty = 0)

######################################################################################
# Month with AMIM in the middle of Sep 2018 AMIM = 0.266, modelling
######################################################################################
arma.sep2018 <- auto.arima(sep2018, trace = TRUE, stepwise = FALSE, approximation = FALSE,
                           allowmean = FALSE, allowdrift = FALSE, ic = "aic"); coeftest(arma.sep2018)
#MA(3), ma1 = 0.5281156, ma2 = 0.3158695, ma3 = 0.6554238

# Checking it the found AR model is causal and invertible
autoplot(arma.sep2018) # Invertible

ar.sep2018 <- ar.mle(sep2018, aic = TRUE, order.max = NULL, intercept = TRUE) 
#AR(4), ar1 = 0.2689, ar2 = 0.0880, ar3 = 0.2644, ar4 = -0.5190

# Checking it the found AR model is causal and invertible
autoplot(ar.sep2018) # Causal 

# Calculating the accuracy of both models, first from auto.arima then from ar.mle
MSE(fitted(arma.sep2018), sep2018)
accuracy(fitted(arma.sep2018), sep2018)

MSE(fitted(ar.sep2018)[5:30], sep2018[5:30])
accuracy(fitted(ar.sep2018)[5:30], sep2018[5:30])

# Checking the residuals
checkresiduals(arma.sep2018) 
checkresiduals(ar.sep2018); Box.test(residuals(ar.sep2018), type = "Ljung-Box")
# White noise

par(mfrow = c(1,1))
# Testing for GARCH effecit
McLeod.Li.test(arma.sep2018, main = "McLeod-Li test statistics for Returns Sep 2018")
# No sign of GARCH effect

sepoct.18 <- as.Date(paste(2018, c(9,10), rep(c(02, 04, 06, 08, 10, 12, 
                                                14, 16, 18, 20, 22, 24, 
                                                26, 28, 30, 31), 2), sep = "-"))
# Static 10-days ahead forecast based on September 2018, AR(4)
sep18.arforecast <- forecast::forecast(ar.sep2018, h = 10)
plot(sep18.arforecast, xaxt = "n", xaxs = "i", main = "10 Days ahead Forecast, AR(4)")
lines(oct2018, type = "o", 
      col = "green", lty = 2); axis.Date(1, at = sepoct.18, format= "%d-%m", las = 1)
legend("topleft", legend = c("Actual, Sep 18", "Forecast", "Actual, Oct 18"),
       col = c("black", "steelblue", "green"), lty = c(1,1,2), cex = 0.55, box.lty = 0)

# Static 10-days ahead forecast based on September 2018, MA(3)
sep18.maforecast <- forecast::forecast(arma.sep2018, h = 10)
plot(sep18.maforecast, xaxt = "n", xaxs = "i", main = "10 Days ahead Forecast, MA(3)")
lines(oct2018, type = "o", 
      col = "green", lty = 2); axis.Date(1, at = sepoct.18, format= "%d-%m", las = 1)
legend("topleft", legend = c("Actual, Sep 18", "Forecast", "Actual, Oct 18"),
       col = c("black", "steelblue", "green"), lty = c(1,1,2), cex = 0.55, box.lty = 0)


#############################################################
# Constant models accuracy
#############################################################

constant.all <- auto.arima(y = return, d = NA, D = NA, max.p = 0, max.q = 0, max.order = 0)
MSE(constant.all$fitted, return) #0.00188
accuracy(constant.all$fitted, return) #RMSE = 0.0434, MAE = 0.0274

constant.2014 <- auto.arima(y = return2014, d = NA, D = NA, max.p = 0, max.q = 0, max.order = 0)
MSE(constant.2014$fitted, return2014)
accuracy(constant.2014$fitted, return2014)

constant.2015 <- auto.arima(y = return2015, d = NA, D = NA, max.p = 0, max.q = 0, max.order = 0)
MSE(constant.2015$fitted, return2015)
accuracy(constant.2015$fitted, return2015)

constant.oct16 <- auto.arima(y = oct2016, d = NA, D = NA, max.p = 0, max.q = 0, max.order = 0)
MSE(constant.oct16$fitted, oct2016)
accuracy(constant.oct16$fitted, oct2016)

constant.may18 <- auto.arima(y = may2018, d = NA, D = NA, max.p = 0, max.q = 0, max.order = 0)
MSE(constant.may18$fitted, may2018)
accuracy(constant.may18$fitted, may2018)

constant.sep18 <- auto.arima(y = sep2018, d = NA, D = NA, max.p = 0, max.q = 0, max.order = 0)
MSE(constant.sep18$fitted, sep2018)
accuracy(constant.sep18$fitted, sep2018)




