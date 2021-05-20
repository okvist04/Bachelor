Bitcoin_df <- read.table("BitcoinDataCoinDesk.csv", header = TRUE, sep = ",", 
                           stringsAsFactors = FALSE)

dates <- as.Date(Bitcoin_df$Date)

td <- seq(as.Date("2013-10-01"), as.Date("2021-04-19"), "days")

closing_price <- zoo(x = Bitcoin_df$Closing.Price..USD., order.by = td)

log.return <- diff(log(closing_price))

#model <- auto.arima(log.return, trace = TRUE, approximation = FALSE, stepwise = FALSE,
#                    allowmean = FALSE)

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


par(mfrow = c(1,1))
ar1.1 <- ar.mle(t1, aic = TRUE, order.max = NULL, intercept = TRUE); ar1.1 #AR(0)
ar2.2 <- ar.mle(t2, aic = TRUE, order.max = NULL, intercept = TRUE); ar2.2 #AR(2)
ar3.3 <- ar.mle(t3, aic = TRUE, order.max = NULL, intercept = TRUE); ar3.3 #AR(8)
ar4.4 <- ar.mle(t4, aic = TRUE, order.max = NULL, intercept = TRUE); ar4.4 #AR(0)
ar5.5 <- ar.mle(t5, aic = TRUE, order.max = NULL, intercept = TRUE); ar5.5 #AR(0)
ar6.6 <- ar.mle(t6, aic = TRUE, order.max = NULL, intercept = TRUE); ar6.6 #AR(1)
ar7.7 <- ar.mle(t7, aic = TRUE, order.max = NULL, intercept = TRUE); ar7.7 #AR(0)
ar8.8 <- ar.mle(t8, aic = TRUE, order.max = NULL, intercept = TRUE); ar8.8 #AR(4)
ar9.9 <- ar.mle(t9, aic = TRUE, order.max = NULL, intercept = TRUE); ar9.9 #AR(0)

#Check how well these AR(p) models fit
fitted1 <- t1 - residuals(ar1.1)
ts.plot(t1); points(fitted1, type = "l", col = "red")

fitted2 <- t2 - residuals(ar2.2)
ts.plot(t2); points(fitted2, type = "l", col = "red")

fitted3 <- t3 - residuals(ar3.3)
ts.plot(t3); points(fitted3, type = "l", col = "red")

fitted4 <- t4 - residuals(ar4.4)
ts.plot(t4); points(fitted4, type = "l", col = "red")

fitted5 <- t5 - residuals(ar5.5)
ts.plot(t5); points(fitted5, type = "l", col = "red")

fitted6 <- t6 - residuals(ar6.6)
ts.plot(t6); points(fitted6, type = "l", col = "red")
arima6 <- auto.arima(t6, trace = TRUE, approximation = FALSE, ic = "aic", 
                     stepwise = FALSE, allowmean = FALSE)
fitted6.1 <- t6 - residuals(arima6)
ts.plot(t6); points(fitted6.1, type = "l", col = "red"); points(fitted6, type = "l", col = "blue")

fitted7 <- t7 - residuals(ar7.7)
ts.plot(t7); points(fitted7, type = "l", col = "red")

fitted8 <- t8 - residuals(ar8.8)
ts.plot(t8); points(fitted8, type = "l", col = "red")
arima8 <- auto.arima(t8, trace = TRUE, ic = "aic", approximation = FALSE, 
                     stepwise = FALSE, allowmean = FALSE)
fitted8.1 <- t8 - residuals(arima8)
ts.plot(t8); points(fitted8.1, type = "l", col = "red"); points(fitted8, type = "l", col = "blue")

fitted9 <- t9 - residuals(ar9.9)
ts.plot(t9); points(fitted9, type = "l", col = "red")

covar1 <- ar1.1$asy.var.coef; covar2 <- ar2.2$asy.var.coef; covar3 <- ar3.3$asy.var.coef
covar4 <- ar4.4$asy.var.coef; covar5 <- ar5.5$asy.var.coef; covar6 <- ar6.6$asy.var.coef
covar7 <- ar7.7$asy.var.coef; covar8 <- ar8.8$asy.var.coef; covar9 <- ar9.9$asy.var.coef

mimt1 <- 0 #Efficient market

U2 <- chol(covar2)
T2 <- t(U2)
InvT2 <- solve(T2)
beta2 <- ar2.2$ar
beta.stand2 <- InvT2 %*% beta2
mimt2 <- (sum(abs(beta.stand2)))/(1+sum(abs(beta.stand2))) #Inefficient market

U3 <- chol(covar3)
T3 <- t(U3)
InvT3 <- solve(T3)
beta3 <- ar3.3$ar
beta.stand3 <- InvT3 %*% beta3
mimt3 <- (sum(abs(beta.stand3)))/(1+sum(abs(beta.stand3))) #Inefficient market

mimt4 <- 0 #Efficient market
mimt5 <- 0 #Efficient market

U6 <- chol(covar6)
T6 <- t(U6)
InvT6 <- solve(T6)
beta6 <- ar6.6$ar
beta.stand6 <- InvT6 %*% beta6
mimt6 <- (sum(abs(beta.stand6)))/(1+sum(abs(beta.stand6))) #Inefficient market

mimt7 <- 0 #Efficient market

U8 <- chol(covar8)
T8 <- t(U8)
InvT8 <- solve(T8)
beta8 <- ar8.8$ar
beta.stand8 <- InvT8 %*% beta8
mimt8 <- (sum(abs(beta.stand8)))/(1+sum(abs(beta.stand8))) #Inefficient market

mimt9 <- 0 #Efficient market 

#Combining all mim values, and the sum of the absolute values of the standardized
#level of auto-correlation. Then making a plot for visualisation.  
stand.beta <- rbind(0, sum(abs(beta.stand2)), sum(abs(beta.stand3)), 0, 0, 
                    sum(abs(beta.stand6)), 0, sum(abs(beta.stand8)), 0)
mim <- rbind(mimt1, mimt2, mimt3, mimt4, mimt5, mimt6, mimt7, mimt8, mimt9)
data <- data.frame(stand.beta, mim)
ggplot(data = data, aes(x = stand.beta, y = mim)) + geom_line()

#Step 3: Simulating to obtain confidence intervals. 
sim2.1 <- rnorm(100000, mean = 0, sd = 1); sim2.2 <- rnorm(100000, mean = 0, sd = 1) #Simulating beta_{1,2} and beta_{2,2} 10,000 times
sim3.1 <- rnorm(100000, mean = 0, sd = 1); sim3.2 <- rnorm(100000, mean = 0, sd = 1) #Simulating beta_{1,3} and beta_{2,3} 10,000 times
sim3.3 <- rnorm(100000, mean = 0, sd = 1); sim3.4 <- rnorm(100000, mean = 0, sd = 1) #Simulating beta_{3,3} and beta_{4,3}
sim3.5 <- rnorm(100000, mean = 0, sd = 1); sim3.6 <- rnorm(100000, mean = 0, sd = 1) #Simulating beta_{5,3} and beta_{6,3}
sim3.7 <- rnorm(100000, mean = 0, sd = 1); sim3.8 <- rnorm(100000, mean = 0, sd = 1) #Simulating beta_{7,3} and beta_{8,3}
sim6.1 <- rnorm(100000, mean = 0, sd = 1) #Simulating beta_{1,6} 10,000 times
sim8.1 <- rnorm(100000, mean = 0, sd = 1); sim8.2 <- rnorm(100000, mean = 0, sd = 1) #Simulating beta_{1,8} and beta_{2,8} 10,000 times
sim8.3 <- rnorm(100000, mean = 0, sd = 1); sim8.4 <- rnorm(100000, mean = 0, sd = 1) #Simulating beta_{3,8} and beta_{4,8}

mim.sim2 <- 0
for (i in 1:100000) {
  mim.sim2[i] <- sum(abs(sim2.1[i]), abs(sim2.2[i]))/(1+sum(abs(sim2.1[i]), abs(sim2.2[i])))
  print(mim.sim2[i])
} #Calculating 10,000 mims for beta_{t=2}

mim.sim3 <- 0
for (i in 1:100000) {
  mim.sim3[i] <- sum(abs(sim3.1[i]), abs(sim3.2[i]), abs(sim3.3[i]),
                     abs(sim3.4[i]), abs(sim3.5[i]), abs(sim3.6[i]),
                     abs(sim3.7[i]), abs(sim3.8[i]))/(1+sum(abs(sim3.1[i]), abs(sim3.2[i]), 
                                                            abs(sim3.3[i]), abs(sim3.4[i]), 
                                                            abs(sim3.5[i]), abs(sim3.6[i]),
                                                            abs(sim3.7[i]), abs(sim3.8[i])))
  print(mim.sim3[i])
} #Calculating 10,000 mims for beta_{t=3}

mim.sim6 <- 0 
for (i in 1:100000) {
  mim.sim6[i] <- sum(abs(sim6.1[i]))/(1+sum(abs(sim6.1[i])))
  print(mim.sim6[i])
} #Calculating 10,000 mims for beta_{t=6}

mim.sim8 <- 0 
for (i in 1:100000) {
  mim.sim8[i] <- sum(abs(sim8.1[i]), abs(sim8.2[i]), abs(sim8.3[i]), 
                     abs(sim8.4[i]))/(1+sum(abs(sim8.1[i]), abs(sim8.2[i]), abs(sim8.3[i]),
                                            abs(sim8.4[i])))
  print(mim.sim8[i])
} #Calculating 10,000 mims for beta_{t=8}

CI2 <- quantile(mim.sim2, probs = .95) #Computing the 95% quantile, i.e., 95% confidence interval for MIM_{t=2}
CI3 <- quantile(mim.sim3, probs = .95) #Computing the 95% quantile, i.e., 95% confidence interval for MIM_{t=3}
CI6 <- quantile(mim.sim6, probs = .95) #Computing the 95% quantile, i.e., 95% confidence interval for MIM_{t=6}
CI8 <- quantile(mim.sim8, probs = .95) #Computing the 95% quantile, i.e., 95% confidence interval for MIM_{t=8}

AMIM2 <- (mimt2 - CI2)/(1-CI2)
AMIM3 <- (mimt3 - CI3)/(1-CI3)
AMIM6 <- (mimt6 - CI6)/(1-CI6)
AMIM8 <- (mimt8 - CI8)/(1-CI8)

CI <- rbind(0, CI2, CI3, 0, 0, CI6, 0, CI8, 0)

mim.sim <- cbind(mim.sim2, mim.sim3, mim.sim6, mim.sim8)

plot.ts(mim); lines(CI, col = "red", lty = 2)

AMIM <- rbind(0, AMIM2, AMIM3, 0, 0, AMIM6, 0, AMIM8, 0)
plot(AMIM, type = "l"); abline(h = 0, col = "blue", lty = 3)




#Making an AR(p) model based on all the data points. 
ar <- ar.mle(log.return, aic = TRUE, intercept = TRUE) #AR(12) model
covar <- ar$asy.var.coef
U <- chol(covar)
T.all <- t(U)
InvT.all <- solve(T.all)
beta <- ar$ar
beta.standard <- InvT.all %*% beta
mim.ALL <- (sum(abs(beta.standard)))/(1+sum(abs(beta.standard))) #Overall inefficient market

sim.all.lag1 <- rnorm(100000, mean = 0, sd = 1)
sim.all.lag2 <- rnorm(100000, mean = 0, sd = 1)
sim.all.lag3 <- rnorm(100000, mean = 0, sd = 1)
sim.all.lag4 <- rnorm(100000, mean = 0, sd = 1)
sim.all.lag5 <- rnorm(100000, mean = 0, sd = 1)
sim.all.lag6 <- rnorm(100000, mean = 0, sd = 1)
sim.all.lag7 <- rnorm(100000, mean = 0, sd = 1)
sim.all.lag8 <- rnorm(100000, mean = 0, sd = 1)
sim.all.lag9 <- rnorm(100000, mean = 0, sd = 1)
sim.all.lag10 <- rnorm(100000, mean = 0, sd = 1)
sim.all.lag11 <- rnorm(100000, mean = 0, sd = 1)
sim.all.lag12 <- rnorm(100000, mean = 0, sd = 1)

mim.sim.all.lag1 <- 0
for (i in 1:100000) {
  mim.sim.all.lag1[i] <- sum(abs(sim.all.lag1[i]))/(1+sum(abs(sim.all.lag1[i])))
  print(mim.sim.all.lag1[i])
}

mim.sim.all.lag2 <- 0
for (i in 1:100000) {
  mim.sim.all.lag2[i] <- sum(abs(sim.all.lag2[i]))/(1+sum(abs(sim.all.lag2[i])))
  print(mim.sim.all.lag2[i])
}

mim.sim.all.lag3 <- 0
for (i in 1:100000) {
  mim.sim.all.lag3[i] <- sum(abs(sim.all.lag3[i]))/(1+sum(abs(sim.all.lag3[i])))
  print(mim.sim.all.lag3[i])
}

mim.sim.all.lag4 <- 0
for (i in 1:100000) {
  mim.sim.all.lag4[i] <- sum(abs(sim.all.lag4[i]))/(1+sum(abs(sim.all.lag4[i])))
  print(mim.sim.all.lag4[i])
}

mim.sim.all.lag5 <- 0
for (i in 1:100000) {
  mim.sim.all.lag5[i] <- sum(abs(sim.all.lag5[i]))/(1+sum(abs(sim.all.lag5[i])))
  print(mim.sim.all.lag5[i])
}

mim.sim.all.lag6 <- 0
for (i in 1:100000) {
  mim.sim.all.lag6[i] <- sum(abs(sim.all.lag6[i]))/(1+sum(abs(sim.all.lag6[i])))
  print(mim.sim.all.lag6[i])
}

mim.sim.all.lag7 <- 0
for (i in 1:100000) {
  mim.sim.all.lag7[i] <- sum(abs(sim.all.lag7[i]))/(1+sum(abs(sim.all.lag7[i])))
  print(mim.sim.all.lag7[i])
}

mim.sim.all.lag8 <- 0
for (i in 1:100000) {
  mim.sim.all.lag8[i] <- sum(abs(sim.all.lag8[i]))/(1+sum(abs(sim.all.lag8[i])))
  print(mim.sim.all.lag8[i])
}

mim.sim.all.lag9 <- 0
for (i in 1:100000) {
  mim.sim.all.lag9[i] <- sum(abs(sim.all.lag9[i]))/(1+sum(abs(sim.all.lag9[i])))
  print(mim.sim.all.lag9[i])
}

mim.sim.all.lag10 <- 0
for (i in 1:100000) {
  mim.sim.all.lag10[i] <- sum(abs(sim.all.lag10[i]))/(1+sum(abs(sim.all.lag10[i])))
  print(mim.sim.all.lag10[i])
}

mim.sim.all.lag11 <- 0
for (i in 1:100000) {
  mim.sim.all.lag11[i] <- sum(abs(sim.all.lag11[i]))/(1+sum(abs(sim.all.lag11[i])))
  print(mim.sim.all.lag11[i])
}

mim.sim.all.lag12 <- 0
for (i in 1:100000) {
  mim.sim.all.lag12[i] <- sum(abs(sim.all.lag12[i]))/(1+sum(abs(sim.all.lag12[i])))
  print(mim.sim.all.lag12[i])
}

CI.all.lag1 <- quantile(mim.sim.all.lag1, probs = .95) #0.662
CI.all.lag2 <- quantile(mim.sim.all.lag2, .95) #0.659
CI.all.lag3 <- quantile(mim.sim.all.lag3, .95) #0.662
CI.all.lag4 <- quantile(mim.sim.all.lag4, .95) #0.658
CI.all.lag5 <- quantile(mim.sim.all.lag5, .95) #0.66
CI.all.lag6 <- quantile(mim.sim.all.lag6, .95) #0.659
CI.all.lag7 <- quantile(mim.sim.all.lag7, .95) #0.658
CI.all.lag8 <- quantile(mim.sim.all.lag8, .95) #0.664
CI.all.lag9 <- quantile(mim.sim.all.lag9, .95) #0.662
CI.all.lag10 <- quantile(mim.sim.all.lag10, .95) #0.66
CI.all.lag11 <- quantile(mim.sim.all.lag11, .95) #0.662
CI.all.lag12 <- quantile(mim.sim.all.lag12, .95) #0.661

AMIM.all1 <- (mim.ALL - CI.all.lag1)/(1-CI.all.lag1)
AMIM.all2 <- (mim.ALL - CI.all.lag2)/(1-CI.all.lag2)
AMIM.all3 <- (mim.ALL - CI.all.lag3)/(1-CI.all.lag3)
AMIM.all4 <- (mim.ALL - CI.all.lag4)/(1-CI.all.lag4)
AMIM.all5 <- (mim.ALL - CI.all.lag5)/(1-CI.all.lag5)
AMIM.all6 <- (mim.ALL - CI.all.lag6)/(1-CI.all.lag6)
AMIM.all7 <- (mim.ALL - CI.all.lag7)/(1-CI.all.lag7)
AMIM.all8 <- (mim.ALL - CI.all.lag8)/(1-CI.all.lag8)
AMIM.all9 <- (mim.ALL - CI.all.lag9)/(1-CI.all.lag9)
AMIM.all10 <- (mim.ALL - CI.all.lag10)/(1-CI.all.lag10)
AMIM.all11 <- (mim.ALL - CI.all.lag11)/(1-CI.all.lag11)
AMIM.all12 <- (mim.ALL - CI.all.lag12)/(1-CI.all.lag12)

(mim.sim.all.lag1-0.6618)/(1-0.6618)


mim.simALL <- 0
for (i in 1:10000) {
  mim.simALL[i] <- (sum(abs(mim.sim.all.lag1[i]), abs(mim.sim.all.lag2[i]), abs(mim.sim.all.lag3[i]), 
                       abs(mim.sim.all.lag4[i]), abs(mim.sim.all.lag5[i]), abs(mim.sim.all.lag6[i]),
                       abs(mim.sim.all.lag7[i]), abs(mim.sim.all.lag8[i]), abs(mim.sim.all.lag9[i]),
                       abs(mim.sim.all.lag10[i]), abs(mim.sim.all.lag11[i]), 
                       abs(mim.sim.all.lag12[i])))/(1+sum(abs(mim.sim.all.lag1[i]), abs(mim.sim.all.lag2[i]), 
                                                                                      abs(mim.sim.all.lag3[i]), abs(mim.sim.all.lag4[i]), 
                                                                                      abs(mim.sim.all.lag5[i]), abs(mim.sim.all.lag6[i]),
                                                                                      abs(mim.sim.all.lag7[i]), abs(mim.sim.all.lag8[i]), 
                                                                                      abs(mim.sim.all.lag9[i]), abs(mim.sim.all.lag10[i]), 
                                                                                      abs(mim.sim.all.lag11[i]), abs(mim.sim.all.lag12[i])))
}
CI.ALL <- quantile(mim.simALL, .95)
AMIM.ALL <- (mim.ALL - CI.ALL)/(1-CI.ALL)
arima.ALL <- auto.arima(log.return, trace = TRUE, ic = "aic", approximation = FALSE, stepwise = FALSE,
                        allowmean = FALSE)
fitted.ALL <- log.return - residuals(arima.ALL)
fitted.ar.ALL <- log.return - residuals(ar)
ts.plot(log.return); points(fitted.ALL, type = "l", col = "red"); 
points(fitted.ar.ALL, type = "l", col = "blue", lty = 3)

mean(0, AMIM2, AMIM3, 0, 0, AMIM6, 0, AMIM8, 0)
