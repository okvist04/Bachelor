Bitcoin_data1 <- read.table("BitcoinDataCoinDesk.csv", header = TRUE, sep = ",", 
                           stringsAsFactors = FALSE)

closing_price <- as.ts(Bitcoin_data[,3])
log.return <- diff(log(closing_price))

model <- auto.arima(log.return) #R's build-in function, to estimate the order of the ARMA model
#Hence, the model should be an ARMA(3,2).
AR <- c(model$coef[1:3]) #AR coefficients from the ARMA(3,2) model.
MA <- c(model$coef[4:5]) #MA coefficients from the ARMA(3,2) model.
ARMAtoAR(ar = AR, ma = MA) #AR(20)
arima1 <- arima(log.return, order = c(20,0,0)) #Simulating the AR(20) model. 
arima1$coef
comat <- arima1$var.coef
U <- chol(comat)
L <- t(U) 
L.inv <- solve(L)
beta.stand <- L.inv %*% arima1$coef
mean(beta.stand)
MIM <- (sum(abs(beta.stand)))/(1+sum(abs(beta.stand))) #MIM value is 0.967, indicating a very 
#inefficient market. 



#IS THIS EVEN RIGHT?????????
summary(fit)
res <- resid(fit)
hist(res, breaks = 200, prob = TRUE); curve(dnorm(x, mean = mean(logprice.df),
                                                  sd = sd(logprice.df)), add = TRUE, col = "red", 
                                            lwd = 1.5)

covar.matrix <- fit$var.coef
beta.hat <- fit$coef
Upper <- chol(covar.matrix)
Lower <- t(Upper)
beta.stand <- solve(Lower)%*%beta.hat
mim <- (sum(abs(beta.stand)))/(1+sum(abs(beta.stand)))


ARMAtoAR(ar = 3, ma = 2, lag.max = 10)



model <- ar(logprice.df, aic = FALSE, demean = TRUE, method = "yw") #AR(32) model
model
model$aic
model$ar
covar.matrix <- model$asy.var.coef #Asymptotic covariance matrix 
beta.hat <- model$ar
eigen(covar.matrix) #The matrix only has positive eigenvalues => positive definite. 
Upper.mat <- chol(covar.matrix) #Gives upper triangular matrix
Lower.mat <- t(Upper.mat) #Makes the lower triangular matrix
View(tcrossprod(Lower.mat)) #Does not print true in all entries. 
beta.standard <- solve(Lower.mat)%*%beta.hat

mim <- (sum(abs(beta.standard)))/(1+sum(abs(beta.standard))) #MIM = 0.9778, which is almost 1, 
plot(sum(abs(beta.standard)), mim, type = "l")
mim_t <- c(rep(0, 32))
t <- 1:32
mim_t <- numeric(length = length(mim_t))
for (i in seq_along(t)) {
  mim_t[i] <- abs(beta.standard[i])/(1+abs(beta.standard[i]))
}
mim_t
plot(x = abs(beta.standard), y = mim_t, type = "l")
#this indicates an inefficient market. 
plot(x = log)
cor(logprice.df)

model <- lm(log.return[2743] ~ 0.034*log.return[2742] + 0.018*log.return[2741] - 0.022*log.return[2740]
            - 0.032*log.return[2739] - 0.032*log.return[2738] - 0.029*log.return[2737] 
            - 0.026*log.return[2736] - 0.022*log.return[2735] - 0.019*log.return[2734] 
            - 0.017*log.return[2733] - 0.014*log.return[2732] - 0.012*log.return[2731] 
            - 0.011*log.return[2730] - 0.009*log.return[2729] - 0.008*log.return[2728] 
            - 0.007*log.return[2727] - 0.006*log.return[2726] - 0.005*log.return[2725] 
            - 0.004*log.return[2724] - 0.004*log.return[2723], data = log.return)