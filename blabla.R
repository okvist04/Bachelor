library(magrittr)
dat = read.csv('BitcoinDataCoinDesk.csv') 

data_with_date = dat %>%
  dplyr::mutate(Date = as.Date(Date))

plot(x = data_with_date$Date, y = data_with_date$Closing.Price..USD., type = 'l')

#R's build-in function, to estimate the order of the ARMA model
#Hence, the model should be an ARMA(4,1).
#AR <- c(model$coef[1:4]) #AR coefficients from the ARMA(4,1) model.
#MA <- c(model$coef[5]) #MA coefficients from the ARMA(4,1) model.
#ARMAtoAR(ar = AR, ma = MA) #AR(20)
#arima1 <- arima(log.return, order = c(20,0,0), include.mean = FALSE); arima1 #Simulating the AR(20) model. 

#arima1$coef
#comat <- arima1$var.coef
#U <- chol(comat)
#L <- t(U) 
#L.inv <- solve(L)
#beta.stand <- L.inv %*% arima1$coef
#mean(beta.stand)
#MIM <- (sum(abs(beta.stand)))/(1+sum(abs(beta.stand))); MIM #MIM value is 0.966, indicating a very 
#inefficient market. 

#ar1 <- auto.arima(t1, trace = TRUE, ic = "aic", approximation = FALSE, stepwise = FALSE,
#                  allowmean = FALSE); ar1 #ARIMA(0,0,0) zero mean
#ar2 <- auto.arima(t2, trace = TRUE, ic = "aic", approximation = FALSE, stepwise = FALSE,
#                  allowmean = FALSE); ar2 #AR(2) zero mean
#ar3 <- auto.arima(t3, trace = TRUE, ic = "aic", approximation = FALSE, stepwise = FALSE,
#                  allowmean = FALSE); ar3 #AR(5) zero mean
#ar4 <- auto.arima(t4, trace = TRUE, ic = "aic", approximation = FALSE, stepwise = FALSE,
#                  allowmean = FALSE); ar4 #ARIMA(0,0,0) zero mean
#ar5 <- auto.arima(t5, trace = TRUE, ic = "aic", approximation = FALSE, stepwise = FALSE,
#                  allowmean = FALSE); ar5 #ARIMA(0,0,0) zero mean
#ar6 <- auto.arima(t6, trace = TRUE, ic = "aic", approximation = FALSE, stepwise = FALSE,
#                  allowmean = FALSE); ar6 #ARMA(2,2) zero mean
#ar6 <- ARMAtoAR(ar = ar6$coef[1:2], ma = ar6$coef[3:4]) #AR(20)
#ar6.1 <- arima(t6, order = c(20,0,0), include.mean = FALSE); ar6.1
#ar7 <- auto.arima(t7, trace = TRUE, ic = "aic", approximation = FALSE, stepwise = FALSE,
#                  allowmean = FALSE); ar7 #ARIMA(0,0,0) zero mean
#ar8 <- auto.arima(t8, trace = TRUE, ic = "aic", approximation = FALSE, stepwise = FALSE,
#                  allowmean = FALSE); ar8 #ARMA(1,2) zero mean
#ar8 <- ARMAtoAR(ar = ar8$coef[1], ma = ar8$coef[2:3]) #AR(20)
#ar9 <- auto.arima(t9, trace = TRUE, ic = "aic", approximation = FALSE, stepwise = FALSE,
#                  allowmean = FALSE); ar9 #ARIMA(0,0,0) zero mean

#mim.t1 <- 0

#covar.t2 <- ar2$var.coef
#U.t2 <- chol(covar.t2)
#L.t2 <- t(U.t2)
#invL.t2 <- solve(L.t2)
#betastand.t2 <- invL.t2 %*% ar2$coef
#mim.t2 <- (sum(abs(betastand.t2)))/(1+sum(abs(betastand.t2)))

#covar.t3 <- ar3$var.coef
#U.t3 <- chol(covar.t3)
#L.t3 <- t(U.t3)
#invL.t3 <- solve(L.t3)
#betastand.t3 <- invL.t3 %*% ar3$coef
#mim.t3 <- (sum(abs(betastand.t3)))/(1+sum(abs(betastand.t3)))

#mim.t4 <- 0
#mim.t5 <- 0

#covar.t6 <- ar6$var.coef
#U.t6 <- chol(covar.t6)
#L.t6 <- t(U.t6)
#invL.t6 <- solve(L.t6)
#betastand.t6 <- invL.t6 %*% ar6$coef
#mim.t6 <- (sum(abs(betastand.t6)))/(1+sum(abs(betastand.t6)))

#covar.t6.1 <- ar6.1$var.coef
#U.t6.1 <- chol(covar.t6.1)
#L.t6.1 <- t(U.t6.1)
#invL.t6.1 <- solve(L.t6.1)
#betastand.t6.1 <- invL.t6.1 %*% ar6.1$coef
#mim.t6.1 <- (sum(abs(betastand.t6.1)))/(1+sum(abs(betastand.t6.1)))

#mim.t7 <- 0

#covar.t8 <- ar8$var.coef
#U.t8 <- chol(covar.t8)
#L.t8 <- t(U.t8)
#invL.t8 <- solve(L.t8)
#betastand.t8 <- invL.t8 %*% ar8$coef
#mim.t8 <- (sum(abs(betastand.t8)))/(1+sum(abs(betastand.t8)))

#mim.t9 <- 0 

#par(mfrow = c(1,1))
#MIM.tAll <- rbind(mim.t1, mim.t2, mim.t3, mim.t4, mim.t5, mim.t6, mim.t7, mim.t8, mim.t9) 
#plot(MIM.tAll, col = "red")
#qqplot(MIM.tAll)
#simbeta.t2.1 <- rnorm(2700, mean = 0, sd = 1)
#simbeta.t2.2 <- rnorm(2700, mean = 0, sd = 1)
#simbeta.t3.1 <- rnorm(2700, mean = 0, sd = 1)
#simbeta.t3.2 <- rnorm(2700, mean = 0, sd = 1)
#simbeta.t3.3 <- rnorm(2700, mean = 0, sd = 1)
#simbeta.t3.4 <- rnorm(2700, mean = 0, sd = 1)
#simbeta.t3.5 <- rnorm(2700, mean = 0, sd = 1)
#simbeta.t6.1 <- rnorm(2700, mean = 0, sd = 1)
#simbeta.t6.2 <- rnorm(2700, mean = 0, sd = 1)
#simbeta.t6.3 <- rnorm(2700, mean = 0, sd = 1)
#simbeta.t6.4 <- rnorm(2700, mean = 0, sd = 1)
#simbeta.t8.1 <- rnorm(2700, mean = 0, sd = 1)
#simbeta.t8.2 <- rnorm(2700, mean = 0, sd = 1)
#simbeta.t8.3 <- rnorm(2700, mean = 0, sd = 1)

#mim.sim.t2.1 <- (sum(abs(simbeta.t2.1)))/(1+sum(abs(simbeta.t2.1))); mim.sim.t2.2 <- (sum(abs(simbeta.t2.2)))/(1+sum(abs(simbeta.t2.2)))
#mim.sim.t3.1 <- (sum(abs(simbeta.t3.1)))/(1+sum(abs(simbeta.t3.1))); mim.sim.t3.2 <- (sum(abs(simbeta.t3.2)))/(1+sum(abs(simbeta.t3.2)))
#mim.sim.t3.3 <- (sum(abs(simbeta.t3.3)))/(1+sum(abs(simbeta.t3.3))); mim.sim.t3.4 <- (sum(abs(simbeta.t3.4)))/(1+sum(abs(simbeta.t3.4)))
#mim.sim.t3.5 <- (sum(abs(simbeta.t3.5)))/(1+sum(abs(simbeta.t3.5))); mim.sim.t6.1 <- (sum(abs(simbeta.t6.1)))/(1+sum(abs(simbeta.t6.1)))
#mim.sim.t6.2 <- (sum(abs(simbeta.t6.2)))/(1+sum(abs(simbeta.t6.2))); mim.sim.t6.3 <- (sum(abs(simbeta.t6.3)))/(1+sum(abs(simbeta.t6.3)))
#mim.sim.t6.4 <- (sum(abs(simbeta.t6.4)))/(1+sum(abs(simbeta.t6.4))); mim.sim.t8.1 <- (sum(abs(simbeta.t8.1)))/(1+sum(abs(simbeta.t8.1)))
#mim.sim.t8.2 <- (sum(abs(simbeta.t8.2)))/(1+sum(abs(simbeta.t8.2))); mim.sim.t8.3 <- (sum(abs(simbeta.t8.3)))/(1+sum(abs(simbeta.t8.3)))

#low <- sqrt((2700-1)*sd(simbeta.t2.1)^2/qchisq(.975, df = 2700 - 1))
#high <- sqrt((2700-1)*sd(simbeta.t2.1)^2/qchisq(.025, df = 2700 - 1))
#(conf <- c(low, sd(simbeta.t2.1), high))
#conf.mimsim.t2 <- CI(c(mim.sim.t2.1, mim.sim.t2.2), ci = 0.975); conf.mimsim.t2
#conf.mimsim.t3 <- CI(c(mim.sim.t3.1, mim.sim.t3.2, mim.sim.t3.3, mim.sim.t3.4, mim.sim.t3.5), 
#                     ci = 0.95)
#conf.mimsim.t6 <- CI(c(mim.sim.t6.1, mim.sim.t6.2, mim.sim.t6.3, mim.sim.t6.4), ci = 0.95)
#conf.mimsim.t8 <- CI(c(mim.sim.t8.1, mim.sim.t8.2, mim.sim.t8.3), ci = 0.95)

#amim <- (mim.t2 - 0.999642)/(1-0.999642)
#amim3 <- (mim.t3 - 0.9995463)/(1-0.9995463)
#amim6 <- (mim.t6 - 0.9995379)/(1-0.9995379)
#amim8 <- (mim.t8 - 0.9995521)/(1-0.9995521)
#IS THIS EVEN RIGHT?????????

#plot(sum(abs(beta.stand)), MIM, type = "l")
#mim_t <- c(rep(0, 20))
#t <- 1:20
#mim_t <- numeric(length = length(mim_t))
#for (i in seq_along(t)) {
#  mim_t[i] <- abs(beta.stand[i])/(1+abs(beta.stand[i]))
#}
#mim_t
#plot(x = abs(beta.stand), y = mim_t, type = "l")
#this indicates an inefficient market. 


#AMIM.all.lag1 



#plot(modelfore@forecast$seriesFor)
#ARMA <- arima(logreturn, order = c(3,0,2))
#summary(ARMA)
#arma.model1 <- specify(ar = c(1.1891, -0.2973, 0.0320), ma = c(-1.2244, 0.3213))
#check(arma.model1) #The given model is both causal and invetible 

T <- 2600
arma.list <- numeric()
ar1.list <- numeric()
ma.list <- numeric()
for (i in T:(length(logreturn)-1)) {
  arma.list <- list(auto.arima(logreturn[(i-T+1):i]))
  ar1.list <- list(arima(logreturn[(i-T+1):i], c(1,0,0)))
  ma.list <- list(mean(logreturn[(i-T+1):i]))
}
y.hat <- sapply(arma.list[T:length(arma.list)], 
                function(x) forecast::forecast(x,1)[[4]])
y.hat2 <- sapply(ar1.list[T:length(ar1.list)],
                 function(x) forecast::forecast(x,1)[[4]])
y.hat3 <- sign(unlist(ma.list))
forecast_accuracy <- cbind(mean(sign(y.hat) == sign(y)),
                           mean(sign(y.hat2) == sign(y)),
                           mean(sign(y.hat3) == sign(y)))


bitcoin = read.csv('BitcoinDataCoinDesk.csv') %>%
  tibble::as_tibble() %>%
  janitor::clean_names() %>%
  dplyr::mutate(
    date = as.Date(date),
    log_return = c(NA, diff(log(closing_price_usd)))
  ) %>%
  tidyr::drop_na(log_return)

2013:2021 %>%
  purrr::map_dfr(
    .f = function(current_year) {
      # browser()
      filtered_bitcoin = bitcoin %>%
        dplyr::filter(lubridate::year(date) == current_year)
      
      suppressWarnings({
        adf_test = tseries::adf.test(filtered_bitcoin$log_return)
        adf_test_k0 = tseries::adf.test(filtered_bitcoin$log_return, k = 0)
        pp_test = tseries::pp.test(filtered_bitcoin$log_return)
      })
      
      ar_object = ar.mle(filtered_bitcoin$log_return, aic = TRUE, order.max = NULL, intercept = TRUE)
      
      covar = ar_object$asy.var.coef
      mimt = 0
      beta_stand = 0
      
      if (!is.null(covar)) {
        U = chol(covar)
        TT = t(U)
        InvT = solve(TT)
        beta = ar_object$ar
        beta_stand = InvT %*% beta
        mimt = (sum(abs(beta_stand)))/(1+sum(abs(beta_stand))) 
      }
      
      
      return(
        tibble::tibble(
          year = current_year,
          first_day = filtered_bitcoin$date %>% dplyr::first(),
          adf_test_p = adf_test$p.value,
          adf_test_k0_p = adf_test_k0$p.value,
          pp_test_p = pp_test$p.value,
          ar_order = ar_object$order,
          beta_stand = sum(abs(beta_stand)),
          mimt = mimt,
          ar_cov = list(covar)
        )
      )
    }
  )

#forecastauto <- forecast::forecast(fit, h = 10)
#plot(forecastauto)
Box.test(residuals(fit), type = "Ljung-Box")
Acf(residuals(fit))
summary(fit)
asympcovar <- fit$var.coef #Asymptotic covariance matrix
res <- fit$residuals #Residuals from model
tsdisplay(res) #Diagnostic plots of residuals 

checkresiduals(fit)
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

autoplot(fit)
Box.test(resid(fit)^2, type = "Ljung")
acf(fit$residuals)

BTC_garch <-  ugarchspec(variance.model = list(model = "sGARCH",         
                                               #Other options are egarch, fgarch, etc.
                                               garchOrder = c(1,1)), 
                         # You can modify the order GARCH(m,s) here
                         mean.model = list(armaOrder = c(4,1)), 
                         #Specify your ARMA model implying your model should be stationary.
                         distribution.model = "norm")         
#Other distribution are "std" for t-distribution, and "ged" for General Error Distribution

BTC_Garch2 <- ugarchfit(spec = BTC_garch , 
                        data = logreturn, out.sample = 10)

modelfore = ugarchforecast(BTC_Garch2, data = NULL, n.ahead = 1, n.roll
                           = 5, out.sample = 10)

plot(modelfore)


#Step 3: Simulating to obtain confidence intervals. 
sim1.1 <- rnorm(100000, mean = 0, sd = 1); sim1.2 <- rnorm(100000, mean = 0, sd = 1)
sim1.3 <- rnorm(100000, mean = 0, sd = 1); sim1.4 <- rnorm(100000, mean = 0, sd = 1)
sim1.5 <- rnorm(100000, mean = 0, sd = 1)
sim2.1 <- rnorm(100000, mean = 0, sd = 1); sim2.2 <- rnorm(100000, mean = 0, sd = 1) 
#Simulating beta_{1,2} and beta_{2,2} 10,000 times
sim3.1 <- rnorm(100000, mean = 0, sd = 1); sim3.2 <- rnorm(100000, mean = 0, sd = 1) 
#Simulating beta_{1,3} and beta_{2,3} 10,000 times
sim3.3 <- rnorm(100000, mean = 0, sd = 1); sim3.4 <- rnorm(100000, mean = 0, sd = 1) 
#Simulating beta_{3,3} and beta_{4,3}
sim3.5 <- rnorm(100000, mean = 0, sd = 1); sim3.6 <- rnorm(100000, mean = 0, sd = 1) 
#Simulating beta_{5,3} and beta_{6,3}
sim3.7 <- rnorm(100000, mean = 0, sd = 1) #Simulating beta_{7,3} and beta_{8,3}
sim5.1 <- rnorm(100000, mean = 0, sd = 1); sim5.2 <- rnorm(100000, mean = 0, sd = 1)
sim5.3 <- rnorm(100000, mean = 0, sd = 1); sim5.4 <- rnorm(100000, mean = 0, sd = 1) 
sim5.5 <- rnorm(100000, mean = 0, sd = 1); sim5.6 <- rnorm(100000, mean = 0, sd = 1)
sim5.7 <- rnorm(100000, mean = 0, sd = 1); sim5.8 <- rnorm(100000, mean = 0, sd = 1) 
sim5.9 <- rnorm(100000, mean = 0, sd = 1); sim5.10 <- rnorm(100000, mean = 0, sd = 1)
sim5.11 <- rnorm(100000, mean = 0, sd = 1); sim5.12 <- rnorm(100000, mean = 0, sd = 1)
sim8.1 <- rnorm(100000, mean = 0, sd = 1) #Simulating beta_{1,8} and beta_{2,8} 10,000 times

mim.sim1 <- 0
for (i in 1:100000) {
  mim.sim1[i] <- sum(abs(sim1.1[i]), abs(sim1.2[i]), abs(sim1.3[i]), abs(sim1.4[i]),
                     abs(sim1.5[i]))/(1+sum(abs(sim1.1[i]), abs(sim1.2[i]), abs(sim1.3[i]), abs(sim1.4[i]),
                                            abs(sim1.5[i])))
  print(mim.sim1[i])
} #Calculating 100,000 mims for beta_{t=1}

mim.sim2 <- 0
for (i in 1:100000) {
  mim.sim2[i] <- sum(abs(sim2.1[i]), abs(sim2.2[i]))/(1+sum(abs(sim2.1[i]), abs(sim2.2[i])))
  print(mim.sim2[i])
} #Calculating 100,000 mims for beta_{t=2}

mim.sim3 <- 0
for (i in 1:100000) {
  mim.sim3[i] <- sum(abs(sim3.1[i]), abs(sim3.2[i]), abs(sim3.3[i]),
                     abs(sim3.4[i]), abs(sim3.5[i]), abs(sim3.6[i]),
                     abs(sim3.7[i]))/(1+sum(abs(sim3.1[i]), abs(sim3.2[i]), 
                                            abs(sim3.3[i]), abs(sim3.4[i]), 
                                            abs(sim3.5[i]), abs(sim3.6[i]),
                                            abs(sim3.7[i])))
  print(mim.sim3[i])
} #Calculating 100,000 mims for beta_{t=3}

mim.sim5 <- 0 
for (i in 1:100000) {
  mim.sim5[i] <- sum(abs(sim5.1[i]), abs(sim5.2[i]), abs(sim5.3[i]),
                     abs(sim5.4[i]), abs(sim5.5[i]), abs(sim5.6[i]),
                     abs(sim5.6[i]), abs(sim5.7[i]), abs(sim5.8[i]),
                     abs(sim5.9[i]), abs(sim5.10[i]), abs(sim5.11[i]),
                     abs(sim5.12[i]))/(1+sum(abs(sim5.1[i]), abs(sim5.2[i]), abs(sim5.3[i]),
                                             abs(sim5.4[i]), abs(sim5.5[i]), abs(sim5.6[i]),
                                             abs(sim5.6[i]), abs(sim5.7[i]), abs(sim5.8[i]),
                                             abs(sim5.9[i]), abs(sim5.10[i]), abs(sim5.11[i]),
                                             abs(sim5.12[i])))
  print(mim.sim5[i])
} #Calculating 100,000 mims for beta_{t=5}

mim.sim8 <- 0 
for (i in 1:100000) {
  mim.sim8[i] <- sum(abs(sim8.1[i]))/(1+sum(abs(sim8.1[i])))
  print(mim.sim8[i])
} #Calculating 100,000 mims for beta_{t=8}
