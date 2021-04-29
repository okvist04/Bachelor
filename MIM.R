Bitcoin_df <- read.table("BitcoinDataCoinDesk.csv", header = TRUE, sep = ",", 
                           stringsAsFactors = FALSE)

dates <- as.Date(Bitcoin_df$Date)

td <- seq(as.Date("2013-10-01"), as.Date("2021-04-19"), "days")

closing_price <- zoo(x = Bitcoin_df$Closing.Price..USD., order.by = td)

#closing_price <- as.ts(Bitcoin_data[,3])
log.return <- diff(log(closing_price))

model <- auto.arima(log.return, trace = TRUE, approximation = FALSE, stepwise = FALSE,
                    allowmean = FALSE)
#R's build-in function, to estimate the order of the ARMA model
#Hence, the model should be an ARMA(4,1).
AR <- c(model$coef[1:4]) #AR coefficients from the ARMA(4,1) model.
MA <- c(model$coef[5]) #MA coefficients from the ARMA(4,1) model.
ARMAtoAR(ar = AR, ma = MA) #AR(20)
arima1 <- arima(log.return, order = c(20,0,0), include.mean = FALSE); arima1 #Simulating the AR(20) model. 

arima1$coef
comat <- arima1$var.coef
U <- chol(comat)
L <- t(U) 
L.inv <- solve(L)
beta.stand <- L.inv %*% arima1$coef
mean(beta.stand)
MIM <- (sum(abs(beta.stand)))/(1+sum(abs(beta.stand))); MIM #MIM value is 0.966, indicating a very 
#inefficient market. 

t1 <- window(log.return, start = as.Date("2013-10-01"), end = as.Date("2013-12-31"))
t2 <- window(log.return, start = as.Date("2014-01-01"), end = as.Date("2014-12-31"))
t3 <- window(log.return, start = as.Date("2015-01-01"), end = as.Date("2015-12-31"))
t4 <- window(log.return, start = as.Date("2016-01-01"), end = as.Date("2016-12-31"))
t5 <- window(log.return, start = as.Date("2017-01-01"), end = as.Date("2017-12-31"))
t6 <- window(log.return, start = as.Date("2018-01-01"), end = as.Date("2018-12-31"))
t7 <- window(log.return, start = as.Date("2019-01-01"), end = as.Date("2019-12-31"))
t8 <- window(log.return, start = as.Date("2020-01-01"), end = as.Date("2020-12-31"))
t9 <- window(log.return, start = as.Date("2021-01-01"), end = as.Date("2021-04-19"))

par(mfrow = c(3,3))

plot(t1, ylab = "Oct 2013 - Dec 2013")
adf.test(t1, k = 0)
adf.test(t1)
pp.test(t1)

plot(t2, ylab = "Jan 2014 - Dec 2014")
adf.test(t2, k = 0)
adf.test(t2)
pp.test(t2)

plot(t3, ylab = "Jan 2015 - Dec 2015")
adf.test(t3, k = 0)
adf.test(t3)
pp.test(t3)

plot(t4, ylab = "Jan 2016 - Dec 2016")
adf.test(t4, k = 0)
adf.test(t4)
pp.test(t4)

plot(t5, ylab = "Jan 2017 - Dec 2017")
adf.test(t5, k = 0)
adf.test(t5)
pp.test(t5)

plot(t6, ylab = "Jan 2018 - Dec 2018")
adf.test(t6, k = 0)
adf.test(t6)
pp.test(t6)

plot(t7, ylab = "Jan 2019 - Dec 2019")
adf.test(t7, k = 0)
adf.test(t7)
pp.test(t7)

plot(t8, ylab = "Jan 2020 - Dec 2020")
adf.test(t8, k = 0)
adf.test(t8)
pp.test(t8)

plot(t9, ylab = "Jan 2021 - Apr 2021")
adf.test(t9, k = 0)
adf.test(t9)
pp.test(t9)

ar1 <- auto.arima(t1, trace = TRUE, ic = "aic", approximation = FALSE, stepwise = FALSE,
                  allowmean = FALSE); ar1 #ARIMA(0,0,0) zero mean
ar2 <- auto.arima(t2, trace = TRUE, ic = "aic", approximation = FALSE, stepwise = FALSE,
                  allowmean = FALSE); ar2 #AR(2) zero mean
ar3 <- auto.arima(t3, trace = TRUE, ic = "aic", approximation = FALSE, stepwise = FALSE,
                  allowmean = FALSE); ar3 #AR(5) zero mean
ar4 <- auto.arima(t4, trace = TRUE, ic = "aic", approximation = FALSE, stepwise = FALSE,
                  allowmean = FALSE); ar4 #ARIMA(0,0,0) zero mean
ar5 <- auto.arima(t5, trace = TRUE, ic = "aic", approximation = FALSE, stepwise = FALSE,
                  allowmean = FALSE); ar5 #ARIMA(0,0,0) zero mean
ar6 <- auto.arima(t6, trace = TRUE, ic = "aic", approximation = FALSE, stepwise = FALSE,
                  allowmean = FALSE); ar6 #ARMA(2,2) zero mean
ar6 <- ARMAtoAR(ar = ar6$coef[1:2], ma = ar6$coef[3:4]) #AR(20)
#ar6.1 <- arima(t6, order = c(20,0,0), include.mean = FALSE); ar6.1
ar7 <- auto.arima(t7, trace = TRUE, ic = "aic", approximation = FALSE, stepwise = FALSE,
                  allowmean = FALSE); ar7 #ARIMA(0,0,0) zero mean
ar8 <- auto.arima(t8, trace = TRUE, ic = "aic", approximation = FALSE, stepwise = FALSE,
                  allowmean = FALSE); ar8 #ARMA(1,2) zero mean
#ar8 <- ARMAtoAR(ar = ar8$coef[1], ma = ar8$coef[2:3]) #AR(20)
ar9 <- auto.arima(t9, trace = TRUE, ic = "aic", approximation = FALSE, stepwise = FALSE,
                  allowmean = FALSE); ar9 #ARIMA(0,0,0) zero mean

mim.t1 <- 0

covar.t2 <- ar2$var.coef
U.t2 <- chol(covar.t2)
L.t2 <- t(U.t2)
invL.t2 <- solve(L.t2)
betastand.t2 <- invL.t2 %*% ar2$coef
mim.t2 <- (sum(abs(betastand.t2)))/(1+sum(abs(betastand.t2)))

covar.t3 <- ar3$var.coef
U.t3 <- chol(covar.t3)
L.t3 <- t(U.t3)
invL.t3 <- solve(L.t3)
betastand.t3 <- invL.t3 %*% ar3$coef
mim.t3 <- (sum(abs(betastand.t3)))/(1+sum(abs(betastand.t3)))

mim.t4 <- 0
mim.t5 <- 0

covar.t6 <- ar6$var.coef
U.t6 <- chol(covar.t6)
L.t6 <- t(U.t6)
invL.t6 <- solve(L.t6)
betastand.t6 <- invL.t6 %*% ar6$coef
mim.t6 <- (sum(abs(betastand.t6)))/(1+sum(abs(betastand.t6)))

covar.t6.1 <- ar6.1$var.coef
U.t6.1 <- chol(covar.t6.1)
L.t6.1 <- t(U.t6.1)
invL.t6.1 <- solve(L.t6.1)
betastand.t6.1 <- invL.t6.1 %*% ar6.1$coef
mim.t6.1 <- (sum(abs(betastand.t6.1)))/(1+sum(abs(betastand.t6.1)))

mim.t7 <- 0

covar.t8 <- ar8$var.coef
U.t8 <- chol(covar.t8)
L.t8 <- t(U.t8)
invL.t8 <- solve(L.t8)
betastand.t8 <- invL.t8 %*% ar8$coef
mim.t8 <- (sum(abs(betastand.t8)))/(1+sum(abs(betastand.t8)))

mim.t9 <- 0 

par(mfrow = c(1,1))
MIM.tAll <- rbind(mim.t1, mim.t2, mim.t3, mim.t4, mim.t5, mim.t6, mim.t7, mim.t8, mim.t9) 
plot(MIM.tAll, col = "red")
qqplot(MIM.tAll)
simbeta.t2.1 <- rnorm(2700, mean = 0, sd = 1)
simbeta.t2.2 <- rnorm(2700, mean = 0, sd = 1)
simbeta.t3.1 <- rnorm(2700, mean = 0, sd = 1)
simbeta.t3.2 <- rnorm(2700, mean = 0, sd = 1)
simbeta.t3.3 <- rnorm(2700, mean = 0, sd = 1)
simbeta.t3.4 <- rnorm(2700, mean = 0, sd = 1)
simbeta.t3.5 <- rnorm(2700, mean = 0, sd = 1)
simbeta.t6.1 <- rnorm(2700, mean = 0, sd = 1)
simbeta.t6.2 <- rnorm(2700, mean = 0, sd = 1)
simbeta.t6.3 <- rnorm(2700, mean = 0, sd = 1)
simbeta.t6.4 <- rnorm(2700, mean = 0, sd = 1)
simbeta.t8.1 <- rnorm(2700, mean = 0, sd = 1)
simbeta.t8.2 <- rnorm(2700, mean = 0, sd = 1)
simbeta.t8.3 <- rnorm(2700, mean = 0, sd = 1)

mim.sim.t2.1 <- (sum(abs(simbeta.t2.1)))/(1+sum(abs(simbeta.t2.1)))
mim.sim.t2.2 <- (sum(abs(simbeta.t2.2)))/(1+sum(abs(simbeta.t2.2)))
mim.sim.t3.1 <- (sum(abs(simbeta.t3.1)))/(1+sum(abs(simbeta.t3.1)))
mim.sim.t3.2 <- (sum(abs(simbeta.t3.2)))/(1+sum(abs(simbeta.t3.2)))
mim.sim.t3.3 <- (sum(abs(simbeta.t3.3)))/(1+sum(abs(simbeta.t3.3)))
mim.sim.t3.4 <- (sum(abs(simbeta.t3.4)))/(1+sum(abs(simbeta.t3.4)))
mim.sim.t3.5 <- (sum(abs(simbeta.t3.5)))/(1+sum(abs(simbeta.t3.5)))
mim.sim.t6.1 <- (sum(abs(simbeta.t6.1)))/(1+sum(abs(simbeta.t6.1)))
mim.sim.t6.2 <- (sum(abs(simbeta.t6.2)))/(1+sum(abs(simbeta.t6.2)))
mim.sim.t6.3 <- (sum(abs(simbeta.t6.3)))/(1+sum(abs(simbeta.t6.3)))
mim.sim.t6.4 <- (sum(abs(simbeta.t6.4)))/(1+sum(abs(simbeta.t6.4)))
mim.sim.t8.1 <- (sum(abs(simbeta.t8.1)))/(1+sum(abs(simbeta.t8.1)))
mim.sim.t8.2 <- (sum(abs(simbeta.t8.2)))/(1+sum(abs(simbeta.t8.2)))
mim.sim.t8.3 <- (sum(abs(simbeta.t8.3)))/(1+sum(abs(simbeta.t8.3)))

conf.mimsim.t2 <- CI(c(mim.sim.t2.1, mim.sim.t2.2), ci = 0.975); conf.mimsim.t2
conf.mimsim.t3 <- CI(c(mim.sim.t3.1, mim.sim.t3.2, mim.sim.t3.3, mim.sim.t3.4, mim.sim.t3.5), 
                     ci = 0.95)
conf.mimsim.t6 <- CI(c(mim.sim.t6.1, mim.sim.t6.2, mim.sim.t6.3, mim.sim.t6.4), ci = 0.95)
conf.mimsim.t8 <- CI(c(mim.sim.t8.1, mim.sim.t8.2, mim.sim.t8.3), ci = 0.95)

amim <- (mim.t2 - 0.999642)/(1-0.999642)
amim3 <- (mim.t3 - 0.9995463)/(1-0.9995463)
amim6 <- (mim.t6 - 0.9995379)/(1-0.9995379)
amim8 <- (mim.t8 - 0.9995521)/(1-0.9995521)
#IS THIS EVEN RIGHT?????????

plot(sum(abs(beta.stand)), MIM, type = "l")
mim_t <- c(rep(0, 20))
t <- 1:20
mim_t <- numeric(length = length(mim_t))
for (i in seq_along(t)) {
  mim_t[i] <- abs(beta.stand[i])/(1+abs(beta.stand[i]))
}
mim_t
plot(x = abs(beta.stand), y = mim_t, type = "l")
#this indicates an inefficient market. 

