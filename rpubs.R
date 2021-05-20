Bitcoin_df<- read.table("BitcoinDataCoinDesk.csv", header = TRUE, sep = ",", 
                        stringsAsFactors = FALSE)

Bitcoin_df$Closing.Price..USD. <- as.numeric(as.character(gsub(",","",Bitcoin_df$Closing.Price..USD.)))

#Converting the data set into time series object
ts_closingprice <- ts(as.vector(Bitcoin_df$Closing.Price..USD.), start = 2013, frequency = 365)
plot.ts(ts_closingprice)
title("Time Series plot of Bitcoin Prices ", sub = "(2013-2021)",
      cex.main = 1.5,   font.main= 1, col.main= "black",
      cex.sub = 0.75, font.sub = 3, col.sub = "red")

par(mfrow=c(1,2))
acf(ts_closingprice, main = "ACF")
pacf(ts_closingprice, main = "PACF")

ar(diff(ts_closingprice))
adf.test(ts_closingprice)

return <- diff(ts_closingprice) / ts_closingprice[1:2757]

ar(return)
adf.test(return); adf.test(return, k = 0); pp.test(return)
par(mfrow=c(1,2))
acf(return, ci.type = "ma", main = "ACF of Return")
pacf(return, main = "PACF of Return")

arma.mod <- auto.arima(return, approximation = FALSE, stepwise = FALSE, allowmean = FALSE, 
                       allowdrift = FALSE); coeftest(arma.mod) #ARMA(1,3)

residual.analysis <- function(model, std = TRUE, start = 2, class = c("ARIMA","GARCH","ARMA-GARCH")[1]){
  # If you have an output from arima() function use class = "ARIMA"
  # If you have an output from garch() function use class = "GARCH"
  # If you have an output from ugarchfit() function use class = "ARMA-GARCH"
  library(TSA)
  library(FitAR)
  if (class == "ARIMA"){
    if (std == TRUE){
      res.model = rstandard(model)
    }else{
      res.model = residuals(model)
    }
  }else if (class == "GARCH"){
    res.model = model$residuals[start:model$n.used]
  }else if (class == "ARMA-GARCH"){
    res.model = model@fit$residuals
  }else {
    stop("The argument 'class' must be either 'ARIMA' or 'GARCH' ")
  }
  par(mfrow=c(3,2))
  plot(res.model, type = 'o', ylab = 'Standardised residuals', main = "Time series plot of standardised residuals")
  abline(h = 0)
  hist(res.model, main = "Histogram of standardised residuals")
  acf(res.model, main = "ACF of standardised residuals")
  pacf(res.model, main = "PACF of standardised residuals")
  qqnorm(res.model, main = "QQ plot of standardised residuals")
  qqline(res.model, col = 2)
  print(shapiro.test(res.model))
  k = 0
  LBQPlot(res.model, lag.max = 30, StartLag = k + 1, k = 0, SquaredQ = FALSE)
}
residual.analysis(arma.mod, std = TRUE, start = 1) #Shapiro-Wilk normality test rejects the normality assumption

par(mfrow = c(1,1))
r.bitcoin <- return*100
plot(r.bitcoin)
title("Plot of returns of Bitcoin", sub = "(2013-2021)",
      cex.main = 1.5,   font.main = 1, col.main = "black",
      cex.sub = 0.75, font.sub = 3, col.sub = "red")

McLeod.Li.test(y = r.bitcoin, main = "McLeod-Li test statistics for Daily return series")

abs <- abs(r.bitcoin)
sqr <- r.bitcoin^2

par(mfrow=c(1,2))
acf(abs, ci.type = "ma", main = "ACF for abs. returns")
pacf(abs, main = "PACF plot for abs.returns")
eacf(abs) #GARCH(1,1), (2,1), (2,2), (3,1), (3,2), (3,3), (4,1), (4,2), (4,3), (4,4), (5,1), (5,2),
#(5,3), (5,4), (5,5)

par(mfrow=c(1,2))
acf(sqr, ci.type = "ma", main = "ACF  for sqr. return")
pacf(sqr, main = "PACF for sqr. return")
eacf(sqr) #GARCH(3,2), (3,3), (3,6), (4,1), (4,2), (4,3), (4,4), (4,5), (5,1), (5,2), (5,7)

#Hence, (1,1), (2,1), (2,2), (3,1), (3,2), (3,3), (3,6), (4,1), (4,2), (4,3), (4,4), (4,5), (5,1), (5,3),
#(5,4), (5,5), (5,7)

m.11 <- garch(r.bitcoin, order = c(1,1), trace = FALSE); summary(m.11)

m.21 <- garch(r.bitcoin, order = c(2,1), trace = FALSE); summary(m.21)

m.22 <- garch(r.bitcoin, order = c(2,2), trace = FALSE); summary(m.22)

m.31 <- garch(r.bitcoin, order = c(3,1), trace = FALSE); summary(m.31)

m.32 <- garch(r.bitcoin, order = c(3,2), trace = FALSE); summary(m.32)

m.33 <- garch(r.bitcoin, order = c(3,3), trace = FALSE); summary(m.33)

m.36 <- garch(r.bitcoin, order = c(3,6), trace = FALSE); summary(m.33)

m.41 <- garch(r.bitcoin, order = c(4,1), trace = FALSE); summary(m.41)

m.42 <- garch(r.bitcoin, order = c(4,2), trace = FALSE); summary(m.42)

m.43 <- garch(r.bitcoin, order = c(4,3), trace = FALSE); summary(m.43)

m.44 <- garch(r.bitcoin, order = c(4,4), trace = FALSE); summary(m.44)

m.45 <- garch(r.bitcoin, order = c(4,5), trace = FALSE); summary(m.45)

m.51 <- garch(r.bitcoin, order = c(5,1), trace = FALSE); summary(m.51)

m.53 <- garch(r.bitcoin, order = c(5,3), trace = FALSE); summary(m.53)

m.54 <- garch(r.bitcoin, order = c(5,4), trace = FALSE); summary(m.54)

m.55 <- garch(r.bitcoin, order = c(5,5), trace = FALSE); summary(m.55)

m.57 <- garch(r.bitcoin, order = c(5,7), trace = FALSE); summary(m.57)

sc.AIC <- AIC(m.11, m.21, m.22, m.31, m.32, m.33, m.36, m.41, m.42, 
              m.43, m.44, m.45, m.51, m.53, m.54, m.55, m.57)

sort.score <- function(x, score = c("bic", "aic")){
  if (score == "aic"){
    x[with(x, order(AIC)),]
  } else if (score == "bic") {
    x[with(x, order(BIC)),]
  } else {
    warning('score = "x" only accepts valid arguments ("aic","bic")')
  }
}
sort.score(sc.AIC, score = "aic") #GARCH(1,1) best 

arma13.garch11 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)), 
                    mean.model = list(armaOrder = c(1, 3), include.mean = TRUE), 
                    distribution.model = "norm")

m.13.11 <- ugarchfit(spec = arma13.garch11, data = r.bitcoin, out.sample = 2600)
plot(m.13.11, which = "all")
m.13.11

m13.11_t_dis <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                          mean.model = list(armaOrder = c(1, 3), include.mean = TRUE), 
                          distribution.model = "std")

m.13.11_t_dis <- ugarchfit(spec = m13.11_t_dis, data = r.bitcoin, out.sample = 100)
plot(m.13.11_t_dis, which = "all")
m.13.11_t_dis

m13.11_t_skw <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                          mean.model = list(armaOrder = c(1, 3), include.mean = TRUE), 
                          distribution.model = "sstd")

m.13.11_t_skw <- ugarchfit(spec = m13.11_t_skw, data = r.bitcoin, out.sample = 100)
plot(m.13.11_t_skw, which = "all")
m.13.11_t_skw

forc <- ugarchforecast(m.13.11, data = r.bitcoin, n.ahead = 10, n.roll = 2500)
print(forc)
plot(forc, which = "all")

forc_t_dis <- ugarchforecast(m.13.11_t_dis, data = r.bitcoin, n.ahead = 10, n.roll = 10)
print(forc_t_dis)
plot(forc_t_dis, which = "all")

######################################################

return_train <- return[1:2400]
return_test <- return[2401:2756]
train_ts <- zoo(return_train)
test_ts <- zoo(return_test)

arma41.garch43 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                    mean.model = list(armaOrder = c(4,1), include.mean = TRUE),
                    distribution.model = "norm")

arma41.garch11.fit <- ugarchfit(spec = first, data = return, out.sample = 2600); infocriteria(arma41.garch11.fit)

ma2.garch42 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,2)),
                          mean.model = list(armaOrder = c(0,2), include.mean = TRUE), 
                          distribution.model = "norm")

ma2garch42 <- ugarchfit(spec = ma2.garch42, data = return, out.sample = 2600); infocriteria(ma2garch42)


