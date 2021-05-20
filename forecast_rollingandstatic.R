Bitcoin_df<- read.table("BitcoinDataCoinDesk.csv", header = TRUE, sep = ",", 
                        stringsAsFactors = FALSE)

dates <- as.Date(Bitcoin_df$Date)

td <- seq(as.Date("2013-10-01"), as.Date("2021-04-19"), "days")

closing_price <- zoo(x = Bitcoin_df$Closing.Price..USD., order.by = td)

return <- diff(closing_price) / closing_price[1:2757]

arma.mod <- auto.arima(return, approximation = FALSE, stepwise = FALSE, allowmean = FALSE, 
                       allowdrift = FALSE); coeftest(arma.mod) #ARMA(4,1)

RMSE.arma <- RMSE(return, fitted(arma.mod)) #RMSE = 0.0433

arma.forecast <- forecast::forecast(arma.mod, h = 10, level = 95)
plot(arma.forecast, xlim = as.Date(c(18000, 18750)), xaxs = "i")

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
residual.analysis(arma.mod, std = TRUE, start = 1) 
#Shapiro-Wilk normality test rejects the normality assumption

par(mfrow = c(1,1))
McLeod.Li.test(y = return, main = "McLeod-Li test statistics") #Shows signs of conditional heteroskedasticity

abs <- abs(return)
sqr <- return^2

eacf(return) #ARMA(0,0), (0,1), (0,2), (0,3), (1,1), (1,2), (1,3), (1,4), (2,2), (2,3), (2,4), (3,3), (3,4)

par(mfrow=c(1,2))
acf(abs, ci.type = "ma", main = "ACF for abs. returns")
pacf(abs, main = "PACF plot for abs.returns")
eacf(abs) 
#GARCH(1,1), (2,1), (2,2), (3,1), (3,2), (3,3), (3,4), (4,1) --- (4,7), (5,1) --- (5,7)

par(mfrow=c(1,2))
acf(sqr, ci.type = "ma", main = "ACF  for sqr. return")
pacf(sqr, main = "PACF for sqr. return")
eacf(sqr) 
#GARCH(2,1), (2,2), (3,1) --- (3,3), (4,1), (4,2), (4,3), (4,4), (4,6), (4,7), (5,1) --- (5,5), (5,7)

#Hence, (1,1), (2,1), (2,2), (3,1), (3,2), (3,3), (3,4), (4,1) --- (4,7), (5,1) --- (5,7)
m.11 <- garch(return, order = c(1,1), trace = FALSE); summary(m.11) #All significant 

m.21 <- garch(return, order = c(2,1), trace = FALSE); summary(m.21) #All significant 

m.22 <- garch(return, order = c(2,2), trace = FALSE); summary(m.22) #a0, a1, a2, b2 significant

m.31 <- garch(return, order = c(3,1), trace = FALSE); summary(m.31) #All significant

m.32 <- garch(return, order = c(3,2), trace = FALSE); summary(m.32) #a0, a1, a2, b2, b3 significant

m.33 <- garch(return, order = c(3,3), trace = FALSE); summary(m.33) #a0, a1, a3, b3 significant

m.34 <- garch(return, order = c(3,4), trace = FALSE); summary(m.34) #a0, a1, a2, a4, b2, b3 significant

m.41 <- garch(return, order = c(4,1), trace = FALSE); summary(m.41) #a0, a1, b1, b2, b3 significant

m.42 <- garch(return, order = c(4,2), trace = FALSE); summary(m.42) #a1 significant

m.43 <- garch(return, order = c(4,3), trace = FALSE); summary(m.43) #Overfitter

m.44 <- garch(return, order = c(4,4), trace = FALSE); summary(m.44) #Overfitter

m.45 <- garch(return, order = c(4,5), trace = FALSE); summary(m.45) #Overfitter

m.46 <- garch(return, order = c(4,6), trace = FALSE); summary(m.46) #a0, a1, a2, a4, a6 significant

m.47 <- garch(return, order = c(4,7), trace = FALSE); summary(m.47) #Overfitter

m.51 <- garch(return, order = c(5,1), trace = FALSE); summary(m.51) #a0, a1, b1, b2, b3, b5 significant

m.52 <- garch(return, order = c(5,2), trace = FALSE); summary(m.52) #a0, a1, b2, b5 significant

m.53 <- garch(return, order = c(5,3), trace = FALSE); summary(m.53) #a0, a1 significant

m.54 <- garch(return, order = c(5,4), trace = FALSE); summary(m.54) #Overfitter

m.55 <- garch(return, order = c(5,5), trace = FALSE); summary(m.55) #Overfitter

m.56 <- garch(return, order = c(5,6), trace = FALSE); summary(m.56) #a0, a1, a2, a6 significant

m.57 <- garch(return, order = c(5,7), trace = FALSE); summary(m.57) #Overfitter

sc.AIC <- AIC(m.11, m.21, m.22, m.31, m.32, m.33, m.34, m.41, m.42, m.46, m.51, m.52, m.53, m.56)

sort.score <- function(x, score = c("bic", "aic")){
        if (score == "aic"){
                x[with(x, order(AIC)),]
        } else if (score == "bic") {
                x[with(x, order(BIC)),]
        } else {
                warning('score = "x" only accepts valid arguments ("aic","bic")')
        }
}
sort.score(sc.AIC, score = "aic") #GARCH(3,2) best AIC = -10387.17

arma41.garch11 <- garchFit(formula = ~arma(4,1) + garch(1,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma41.garch11)
arma41.garch21 <- garchFit(formula = ~arma(4,1) + garch(2,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma41.garch21)
arma41.garch22 <- garchFit(formula = ~arma(4,1) + garch(2,2), data = return, cond.dist = "norm", trace = FALSE); summary(arma41.garch22)
arma41.garch31 <- garchFit(formula = ~arma(4,1) + garch(3,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma41.garch31)
arma41.garch32 <- garchFit(formula = ~arma(4,1) + garch(3,2), data = return, cond.dist = "norm", trace = FALSE); summary(arma41.garch32)
arma41.garch33 <- garchFit(formula = ~arma(4,1) + garch(3,3), data = return, cond.dist = "norm", trace = FALSE); summary(arma41.garch33)
arma41.garch34 <- garchFit(formula = ~arma(4,1) + garch(3,4), data = return, cond.dist = "norm", trace = FALSE); summary(arma41.garch34)
arma41.garch41 <- garchFit(formula = ~arma(4,1) + garch(4,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma41.garch41)
arma41.garch42 <- garchFit(formula = ~arma(4,1) + garch(4,2), data = return, cond.dist = "norm", trace = FALSE); summary(arma41.garch42)
arma41.garch46 <- garchFit(formula = ~arma(4,1) + garch(4,6), data = return, cond.dist = "norm", trace = FALSE); summary(arma41.garch46)
arma41.garch51 <- garchFit(formula = ~arma(4,1) + garch(5,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma41.garch51)
arma41.garch52 <- garchFit(formula = ~arma(4,1) + garch(5,2), data = return, cond.dist = "norm", trace = FALSE); summary(arma41.garch52)
arma41.garch53 <- garchFit(formula = ~arma(4,1) + garch(5,3), data = return, cond.dist = "norm", trace = FALSE); summary(arma41.garch53)
arma41.garch56 <- garchFit(formula = ~arma(4,1) + garch(5,6), data = return, cond.dist = "norm", trace = FALSE); summary(arma41.garch56)

#Best model is ARMA(4,1)+GARCH(1,1) based on BIC = - 4.010682 (cond.dist = "std")
#Best model is ARMA(4,1)+GARCH(4,2) based on BIC = - 3.752123 (cond.dist = "norm")

#ARMA(0,0), (0,1), (0,2), (0,3), (1,1), (1,2), (1,3), (1,4), (2,2), (2,3), (2,4), (3,3), (3,4)
#GARCH(1,1), (2,1), (2,2), (3,1), (3,2), (3,3), (3,4), (4,1), (4,2), (4,6), (5,1), (5,2), (5,3), (5,6)

arma00.garch11 <- garchFit(formula = ~arma(0,0) + garch(1,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma00.garch11)
arma00.garch21 <- garchFit(formula = ~arma(0,0) + garch(2,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma00.garch21)
arma00.garch22 <- garchFit(formula = ~arma(0,0) + garch(2,2), data = return, cond.dist = "norm", trace = FALSE); summary(arma00.garch22)
arma00.garch31 <- garchFit(formula = ~arma(0,0) + garch(3,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma00.garch31)
arma00.garch32 <- garchFit(formula = ~arma(0,0) + garch(3,2), data = return, cond.dist = "norm", trace = FALSE); summary(arma00.garch32)
arma00.garch33 <- garchFit(formula = ~arma(0,0) + garch(3,3), data = return, cond.dist = "norm", trace = FALSE); summary(arma00.garch33)
arma00.garch34 <- garchFit(formula = ~arma(0,0) + garch(3,4), data = return, cond.dist = "norm", trace = FALSE); summary(arma00.garch34)
arma00.garch41 <- garchFit(formula = ~arma(0,0) + garch(4,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma00.garch41)
arma00.garch42 <- garchFit(formula = ~arma(0,0) + garch(4,2), data = return, cond.dist = "norm", trace = FALSE); summary(arma00.garch42)
arma00.garch46 <- garchFit(formula = ~arma(0,0) + garch(4,6), data = return, cond.dist = "norm", trace = FALSE); summary(arma00.garch46)
arma00.garch51 <- garchFit(formula = ~arma(0,0) + garch(5,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma00.garch51)
arma00.garch52 <- garchFit(formula = ~arma(0,0) + garch(5,2), data = return, cond.dist = "norm", trace = FALSE); summary(arma00.garch52)
arma00.garch53 <- garchFit(formula = ~arma(0,0) + garch(5,3), data = return, cond.dist = "norm", trace = FALSE); summary(arma00.garch53)
arma00.garch56 <- garchFit(formula = ~arma(0,0) + garch(5,6), data = return, cond.dist = "norm", trace = FALSE); summary(arma00.garch56)

arma01.garch11 <- garchFit(formula = ~arma(0,1) + garch(1,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma01.garch11)
arma01.garch21 <- garchFit(formula = ~arma(0,1) + garch(2,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma01.garch21)
arma01.garch22 <- garchFit(formula = ~arma(0,1) + garch(2,2), data = return, cond.dist = "norm", trace = FALSE); summary(arma01.garch22)
arma01.garch31 <- garchFit(formula = ~arma(0,1) + garch(3,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma01.garch31)
arma01.garch32 <- garchFit(formula = ~arma(0,1) + garch(3,2), data = return, cond.dist = "norm", trace = FALSE); summary(arma01.garch32)
arma01.garch33 <- garchFit(formula = ~arma(0,1) + garch(3,3), data = return, cond.dist = "norm", trace = FALSE); summary(arma01.garch33)
arma01.garch34 <- garchFit(formula = ~arma(0,1) + garch(3,4), data = return, cond.dist = "norm", trace = FALSE); summary(arma01.garch34)
arma01.garch41 <- garchFit(formula = ~arma(0,1) + garch(4,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma01.garch41)
arma01.garch42 <- garchFit(formula = ~arma(0,1) + garch(4,2), data = return, cond.dist = "norm", trace = FALSE); summary(arma01.garch42)
arma01.garch46 <- garchFit(formula = ~arma(0,1) + garch(4,6), data = return, cond.dist = "norm", trace = FALSE); summary(arma01.garch46)
arma01.garch51 <- garchFit(formula = ~arma(0,1) + garch(5,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma01.garch51)
arma01.garch52 <- garchFit(formula = ~arma(0,1) + garch(5,2), data = return, cond.dist = "norm", trace = FALSE); summary(arma01.garch52)
arma01.garch53 <- garchFit(formula = ~arma(0,1) + garch(5,3), data = return, cond.dist = "norm", trace = FALSE); summary(arma01.garch53)
arma01.garch56 <- garchFit(formula = ~arma(0,1) + garch(5,6), data = return, cond.dist = "norm", trace = FALSE); summary(arma01.garch56)

arma02.garch11 <- garchFit(formula = ~arma(0,2) + garch(1,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma02.garch11)
arma02.garch21 <- garchFit(formula = ~arma(0,2) + garch(2,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma02.garch21)
arma02.garch22 <- garchFit(formula = ~arma(0,2) + garch(2,2), data = return, cond.dist = "norm", trace = FALSE); summary(arma02.garch22)
arma02.garch31 <- garchFit(formula = ~arma(0,2) + garch(3,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma02.garch31)
arma02.garch32 <- garchFit(formula = ~arma(0,2) + garch(3,2), data = return, cond.dist = "norm", trace = FALSE); summary(arma02.garch32)
arma02.garch33 <- garchFit(formula = ~arma(0,2) + garch(3,3), data = return, cond.dist = "norm", trace = FALSE); summary(arma02.garch33)
arma02.garch34 <- garchFit(formula = ~arma(0,2) + garch(3,4), data = return, cond.dist = "norm", trace = FALSE); summary(arma02.garch34)
arma02.garch41 <- garchFit(formula = ~arma(0,2) + garch(4,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma02.garch41)
arma02.garch42 <- garchFit(formula = ~arma(0,2) + garch(4,2), data = return, cond.dist = "norm", trace = FALSE); summary(arma02.garch42)
arma02.garch46 <- garchFit(formula = ~arma(0,2) + garch(4,6), data = return, cond.dist = "norm", trace = FALSE); summary(arma02.garch46)
arma02.garch51 <- garchFit(formula = ~arma(0,2) + garch(5,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma02.garch51)
arma02.garch52 <- garchFit(formula = ~arma(0,2) + garch(5,2), data = return, cond.dist = "norm", trace = FALSE); summary(arma02.garch52)
arma02.garch53 <- garchFit(formula = ~arma(0,2) + garch(5,3), data = return, cond.dist = "norm", trace = FALSE); summary(arma02.garch53)
arma02.garch56 <- garchFit(formula = ~arma(0,2) + garch(5,6), data = return, cond.dist = "norm", trace = FALSE); summary(arma02.garch56)

arma03.garch11 <- garchFit(formula = ~arma(0,3) + garch(1,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma03.garch11)
arma03.garch21 <- garchFit(formula = ~arma(0,3) + garch(2,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma03.garch21)
arma03.garch22 <- garchFit(formula = ~arma(0,3) + garch(2,2), data = return, cond.dist = "norm", trace = FALSE); summary(arma03.garch22)
arma03.garch31 <- garchFit(formula = ~arma(0,3) + garch(3,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma03.garch31)
arma03.garch32 <- garchFit(formula = ~arma(0,3) + garch(3,2), data = return, cond.dist = "norm", trace = FALSE); summary(arma03.garch32)
arma03.garch33 <- garchFit(formula = ~arma(0,3) + garch(3,3), data = return, cond.dist = "norm", trace = FALSE); summary(arma03.garch33)
arma03.garch34 <- garchFit(formula = ~arma(0,3) + garch(3,4), data = return, cond.dist = "norm", trace = FALSE); summary(arma03.garch34)
arma03.garch41 <- garchFit(formula = ~arma(0,3) + garch(4,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma03.garch41)
arma03.garch42 <- garchFit(formula = ~arma(0,3) + garch(4,2), data = return, cond.dist = "norm", trace = FALSE); summary(arma03.garch42)
arma03.garch46 <- garchFit(formula = ~arma(0,3) + garch(4,6), data = return, cond.dist = "norm", trace = FALSE); summary(arma03.garch46)
arma03.garch51 <- garchFit(formula = ~arma(0,3) + garch(5,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma03.garch51)
arma03.garch52 <- garchFit(formula = ~arma(0,3) + garch(5,2), data = return, cond.dist = "norm", trace = FALSE); summary(arma03.garch52)
arma03.garch53 <- garchFit(formula = ~arma(0,3) + garch(5,3), data = return, cond.dist = "norm", trace = FALSE); summary(arma03.garch53)
arma03.garch56 <- garchFit(formula = ~arma(0,3) + garch(5,6), data = return, cond.dist = "norm", trace = FALSE); summary(arma03.garch56)

arma11.garch11 <- garchFit(formula = ~arma(1,1) + garch(1,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma11.garch11)
arma11.garch21 <- garchFit(formula = ~arma(1,1) + garch(2,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma11.garch21)
arma11.garch22 <- garchFit(formula = ~arma(1,1) + garch(2,2), data = return, cond.dist = "norm", trace = FALSE); summary(arma11.garch22)
arma11.garch31 <- garchFit(formula = ~arma(1,1) + garch(3,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma11.garch31)
arma11.garch32 <- garchFit(formula = ~arma(1,1) + garch(3,2), data = return, cond.dist = "norm", trace = FALSE); summary(arma11.garch32)
arma11.garch33 <- garchFit(formula = ~arma(1,1) + garch(3,3), data = return, cond.dist = "norm", trace = FALSE); summary(arma11.garch33)
arma11.garch34 <- garchFit(formula = ~arma(1,1) + garch(3,4), data = return, cond.dist = "norm", trace = FALSE); summary(arma11.garch34)
arma11.garch41 <- garchFit(formula = ~arma(1,1) + garch(4,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma11.garch41)
arma11.garch42 <- garchFit(formula = ~arma(1,1) + garch(4,2), data = return, cond.dist = "norm", trace = FALSE); summary(arma11.garch42)
arma11.garch46 <- garchFit(formula = ~arma(1,1) + garch(4,6), data = return, cond.dist = "norm", trace = FALSE); summary(arma11.garch46)
arma11.garch51 <- garchFit(formula = ~arma(1,1) + garch(5,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma11.garch51)
arma11.garch52 <- garchFit(formula = ~arma(1,1) + garch(5,2), data = return, cond.dist = "norm", trace = FALSE); summary(arma11.garch52)
arma11.garch53 <- garchFit(formula = ~arma(1,1) + garch(5,3), data = return, cond.dist = "norm", trace = FALSE); summary(arma11.garch53)
arma11.garch56 <- garchFit(formula = ~arma(1,1) + garch(5,6), data = return, cond.dist = "norm", trace = FALSE); summary(arma11.garch56)

arma12.garch11 <- garchFit(formula = ~arma(1,2) + garch(1,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma12.garch11)
arma12.garch21 <- garchFit(formula = ~arma(1,2) + garch(2,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma12.garch21)
arma12.garch22 <- garchFit(formula = ~arma(1,2) + garch(2,2), data = return, cond.dist = "norm", trace = FALSE); summary(arma12.garch22)
arma12.garch31 <- garchFit(formula = ~arma(1,2) + garch(3,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma12.garch31)
arma12.garch32 <- garchFit(formula = ~arma(1,2) + garch(3,2), data = return, cond.dist = "norm", trace = FALSE); summary(arma12.garch32)
arma12.garch33 <- garchFit(formula = ~arma(1,2) + garch(3,3), data = return, cond.dist = "norm", trace = FALSE); summary(arma12.garch33)
arma12.garch34 <- garchFit(formula = ~arma(1,2) + garch(3,4), data = return, cond.dist = "norm", trace = FALSE); summary(arma12.garch34)
arma12.garch41 <- garchFit(formula = ~arma(1,2) + garch(4,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma12.garch41)
arma12.garch42 <- garchFit(formula = ~arma(1,2) + garch(4,2), data = return, cond.dist = "norm", trace = FALSE); summary(arma12.garch42)
arma12.garch46 <- garchFit(formula = ~arma(1,2) + garch(4,6), data = return, cond.dist = "norm", trace = FALSE); summary(arma12.garch46)
arma12.garch51 <- garchFit(formula = ~arma(1,2) + garch(5,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma12.garch51)
arma12.garch52 <- garchFit(formula = ~arma(1,2) + garch(5,2), data = return, cond.dist = "norm", trace = FALSE); summary(arma12.garch52)
arma12.garch53 <- garchFit(formula = ~arma(1,2) + garch(5,3), data = return, cond.dist = "norm", trace = FALSE); summary(arma12.garch53)
arma12.garch56 <- garchFit(formula = ~arma(1,2) + garch(5,6), data = return, cond.dist = "norm", trace = FALSE); summary(arma12.garch56)

arma13.garch11 <- garchFit(formula = ~arma(1,3) + garch(1,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma13.garch11)
arma13.garch21 <- garchFit(formula = ~arma(1,3) + garch(2,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma13.garch21)
arma13.garch22 <- garchFit(formula = ~arma(1,3) + garch(2,2), data = return, cond.dist = "norm", trace = FALSE); summary(arma13.garch22)
arma13.garch31 <- garchFit(formula = ~arma(1,3) + garch(3,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma13.garch31)
arma13.garch32 <- garchFit(formula = ~arma(1,3) + garch(3,2), data = return, cond.dist = "norm", trace = FALSE); summary(arma13.garch32)
arma13.garch33 <- garchFit(formula = ~arma(1,3) + garch(3,3), data = return, cond.dist = "norm", trace = FALSE); summary(arma13.garch33)
arma13.garch34 <- garchFit(formula = ~arma(1,3) + garch(3,4), data = return, cond.dist = "norm", trace = FALSE); summary(arma13.garch34)
arma13.garch41 <- garchFit(formula = ~arma(1,3) + garch(4,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma13.garch41)
arma13.garch42 <- garchFit(formula = ~arma(1,3) + garch(4,2), data = return, cond.dist = "norm", trace = FALSE); summary(arma13.garch42)
arma13.garch46 <- garchFit(formula = ~arma(1,3) + garch(4,6), data = return, cond.dist = "norm", trace = FALSE); summary(arma13.garch46)
arma13.garch51 <- garchFit(formula = ~arma(1,3) + garch(5,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma13.garch51)
arma13.garch52 <- garchFit(formula = ~arma(1,3) + garch(5,2), data = return, cond.dist = "norm", trace = FALSE); summary(arma13.garch52)
arma13.garch53 <- garchFit(formula = ~arma(1,3) + garch(5,3), data = return, cond.dist = "norm", trace = FALSE); summary(arma13.garch53)
arma13.garch56 <- garchFit(formula = ~arma(1,3) + garch(5,6), data = return, cond.dist = "norm", trace = FALSE); summary(arma13.garch56)

arma14.garch11 <- garchFit(formula = ~arma(1,4) + garch(1,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma14.garch11)
arma14.garch21 <- garchFit(formula = ~arma(1,4) + garch(2,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma14.garch21)
arma14.garch22 <- garchFit(formula = ~arma(1,4) + garch(2,2), data = return, cond.dist = "norm", trace = FALSE); summary(arma14.garch22)
arma14.garch31 <- garchFit(formula = ~arma(1,4) + garch(3,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma14.garch31)
arma14.garch32 <- garchFit(formula = ~arma(1,4) + garch(3,2), data = return, cond.dist = "norm", trace = FALSE); summary(arma14.garch32)
arma14.garch33 <- garchFit(formula = ~arma(1,4) + garch(3,3), data = return, cond.dist = "norm", trace = FALSE); summary(arma14.garch33)
arma14.garch34 <- garchFit(formula = ~arma(1,4) + garch(3,4), data = return, cond.dist = "norm", trace = FALSE); summary(arma14.garch34)
arma14.garch41 <- garchFit(formula = ~arma(1,4) + garch(4,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma14.garch41)
arma14.garch42 <- garchFit(formula = ~arma(1,4) + garch(4,2), data = return, cond.dist = "norm", trace = FALSE); summary(arma14.garch42)
arma14.garch46 <- garchFit(formula = ~arma(1,4) + garch(4,6), data = return, cond.dist = "norm", trace = FALSE); summary(arma14.garch46)
arma14.garch51 <- garchFit(formula = ~arma(1,4) + garch(5,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma14.garch51)
arma14.garch52 <- garchFit(formula = ~arma(1,4) + garch(5,2), data = return, cond.dist = "norm", trace = FALSE); summary(arma14.garch52)
arma14.garch53 <- garchFit(formula = ~arma(1,4) + garch(5,3), data = return, cond.dist = "norm", trace = FALSE); summary(arma14.garch53)
arma14.garch56 <- garchFit(formula = ~arma(1,4) + garch(5,6), data = return, cond.dist = "norm", trace = FALSE); summary(arma14.garch56)

arma22.garch11 <- garchFit(formula = ~arma(2,2) + garch(1,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma22.garch11)
arma22.garch21 <- garchFit(formula = ~arma(2,2) + garch(2,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma22.garch21)
arma22.garch22 <- garchFit(formula = ~arma(2,2) + garch(2,2), data = return, cond.dist = "norm", trace = FALSE); summary(arma22.garch22)
arma22.garch31 <- garchFit(formula = ~arma(2,2) + garch(3,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma22.garch31)
arma22.garch32 <- garchFit(formula = ~arma(2,2) + garch(3,2), data = return, cond.dist = "norm", trace = FALSE); summary(arma22.garch32)
arma22.garch33 <- garchFit(formula = ~arma(2,2) + garch(3,3), data = return, cond.dist = "norm", trace = FALSE); summary(arma22.garch33)
arma22.garch34 <- garchFit(formula = ~arma(2,2) + garch(3,4), data = return, cond.dist = "norm", trace = FALSE); summary(arma22.garch34)
arma22.garch41 <- garchFit(formula = ~arma(2,2) + garch(4,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma22.garch41)
arma22.garch42 <- garchFit(formula = ~arma(2,2) + garch(4,2), data = return, cond.dist = "norm", trace = FALSE); summary(arma22.garch42)
arma22.garch46 <- garchFit(formula = ~arma(2,2) + garch(4,6), data = return, cond.dist = "norm", trace = FALSE); summary(arma22.garch46)
arma22.garch51 <- garchFit(formula = ~arma(2,2) + garch(5,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma22.garch51)
arma22.garch52 <- garchFit(formula = ~arma(2,2) + garch(5,2), data = return, cond.dist = "norm", trace = FALSE); summary(arma22.garch52)
arma22.garch53 <- garchFit(formula = ~arma(2,2) + garch(5,3), data = return, cond.dist = "norm", trace = FALSE); summary(arma22.garch53)
arma22.garch56 <- garchFit(formula = ~arma(2,2) + garch(5,6), data = return, cond.dist = "norm", trace = FALSE); summary(arma22.garch56)

arma23.garch11 <- garchFit(formula = ~arma(2,3) + garch(1,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma23.garch11)
arma23.garch21 <- garchFit(formula = ~arma(2,3) + garch(2,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma23.garch21)
arma23.garch22 <- garchFit(formula = ~arma(2,3) + garch(2,2), data = return, cond.dist = "norm", trace = FALSE); summary(arma23.garch22)
arma23.garch31 <- garchFit(formula = ~arma(2,3) + garch(3,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma23.garch31)
arma23.garch32 <- garchFit(formula = ~arma(2,3) + garch(3,2), data = return, cond.dist = "norm", trace = FALSE); summary(arma23.garch32)
arma23.garch33 <- garchFit(formula = ~arma(2,3) + garch(3,3), data = return, cond.dist = "norm", trace = FALSE); summary(arma23.garch33)
arma23.garch34 <- garchFit(formula = ~arma(2,3) + garch(3,4), data = return, cond.dist = "norm", trace = FALSE); summary(arma23.garch34)
arma23.garch41 <- garchFit(formula = ~arma(2,3) + garch(4,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma23.garch41)
arma23.garch42 <- garchFit(formula = ~arma(2,3) + garch(4,2), data = return, cond.dist = "norm", trace = FALSE); summary(arma23.garch42)
arma23.garch46 <- garchFit(formula = ~arma(2,3) + garch(4,6), data = return, cond.dist = "norm", trace = FALSE); summary(arma23.garch46)
arma23.garch51 <- garchFit(formula = ~arma(2,3) + garch(5,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma23.garch51)
arma23.garch52 <- garchFit(formula = ~arma(2,3) + garch(5,2), data = return, cond.dist = "norm", trace = FALSE); summary(arma23.garch52)
arma23.garch53 <- garchFit(formula = ~arma(2,3) + garch(5,3), data = return, cond.dist = "norm", trace = FALSE); summary(arma23.garch53)
arma23.garch56 <- garchFit(formula = ~arma(2,3) + garch(5,6), data = return, cond.dist = "norm", trace = FALSE); summary(arma23.garch56)

arma24.garch11 <- garchFit(formula = ~arma(2,4) + garch(1,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma24.garch11)
arma24.garch21 <- garchFit(formula = ~arma(2,4) + garch(2,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma24.garch21)
arma24.garch22 <- garchFit(formula = ~arma(2,4) + garch(2,2), data = return, cond.dist = "norm", trace = FALSE); summary(arma24.garch22)
arma24.garch31 <- garchFit(formula = ~arma(2,4) + garch(3,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma24.garch31)
arma24.garch32 <- garchFit(formula = ~arma(2,4) + garch(3,2), data = return, cond.dist = "norm", trace = FALSE); summary(arma24.garch32)
arma24.garch33 <- garchFit(formula = ~arma(2,4) + garch(3,3), data = return, cond.dist = "norm", trace = FALSE); summary(arma24.garch33)
arma24.garch34 <- garchFit(formula = ~arma(2,4) + garch(3,4), data = return, cond.dist = "norm", trace = FALSE); summary(arma24.garch34)
arma24.garch41 <- garchFit(formula = ~arma(2,4) + garch(4,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma24.garch41)
arma24.garch42 <- garchFit(formula = ~arma(2,4) + garch(4,2), data = return, cond.dist = "norm", trace = FALSE); summary(arma24.garch42)
arma24.garch46 <- garchFit(formula = ~arma(2,4) + garch(4,6), data = return, cond.dist = "norm", trace = FALSE); summary(arma24.garch46)
arma24.garch51 <- garchFit(formula = ~arma(2,4) + garch(5,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma24.garch51)
arma24.garch52 <- garchFit(formula = ~arma(2,4) + garch(5,2), data = return, cond.dist = "norm", trace = FALSE); summary(arma24.garch52)
arma24.garch53 <- garchFit(formula = ~arma(2,4) + garch(5,3), data = return, cond.dist = "norm", trace = FALSE); summary(arma24.garch53)
arma24.garch56 <- garchFit(formula = ~arma(2,4) + garch(5,6), data = return, cond.dist = "norm", trace = FALSE); summary(arma24.garch56)

arma33.garch11 <- garchFit(formula = ~arma(3,3) + garch(1,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma33.garch11)
arma33.garch21 <- garchFit(formula = ~arma(3,3) + garch(2,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma33.garch21)
arma33.garch22 <- garchFit(formula = ~arma(3,3) + garch(2,2), data = return, cond.dist = "norm", trace = FALSE); summary(arma33.garch22)
arma33.garch31 <- garchFit(formula = ~arma(3,3) + garch(3,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma33.garch31)
arma33.garch32 <- garchFit(formula = ~arma(3,3) + garch(3,2), data = return, cond.dist = "norm", trace = FALSE); summary(arma33.garch32)
arma33.garch33 <- garchFit(formula = ~arma(3,3) + garch(3,3), data = return, cond.dist = "norm", trace = FALSE); summary(arma33.garch33)
arma33.garch34 <- garchFit(formula = ~arma(3,3) + garch(3,4), data = return, cond.dist = "norm", trace = FALSE); summary(arma33.garch34)
arma33.garch41 <- garchFit(formula = ~arma(3,3) + garch(4,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma33.garch41)
arma33.garch42 <- garchFit(formula = ~arma(3,3) + garch(4,2), data = return, cond.dist = "norm", trace = FALSE); summary(arma33.garch42)
arma33.garch46 <- garchFit(formula = ~arma(3,3) + garch(4,6), data = return, cond.dist = "norm", trace = FALSE); summary(arma33.garch46)
arma33.garch51 <- garchFit(formula = ~arma(3,3) + garch(5,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma33.garch51)
arma33.garch52 <- garchFit(formula = ~arma(3,3) + garch(5,2), data = return, cond.dist = "norm", trace = FALSE); summary(arma33.garch52)
arma33.garch53 <- garchFit(formula = ~arma(3,3) + garch(5,3), data = return, cond.dist = "norm", trace = FALSE); summary(arma33.garch53)
arma33.garch56 <- garchFit(formula = ~arma(3,4) + garch(5,6), data = return, cond.dist = "norm", trace = FALSE); summary(arma33.garch56)

arma34.garch11 <- garchFit(formula = ~arma(3,4) + garch(1,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma34.garch11)
arma34.garch21 <- garchFit(formula = ~arma(3,4) + garch(2,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma34.garch21)
arma34.garch22 <- garchFit(formula = ~arma(3,4) + garch(2,2), data = return, cond.dist = "norm", trace = FALSE); summary(arma34.garch22)
arma34.garch31 <- garchFit(formula = ~arma(3,4) + garch(3,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma34.garch31)
arma34.garch32 <- garchFit(formula = ~arma(3,4) + garch(3,2), data = return, cond.dist = "norm", trace = FALSE); summary(arma34.garch32)
arma34.garch33 <- garchFit(formula = ~arma(3,4) + garch(3,3), data = return, cond.dist = "norm", trace = FALSE); summary(arma34.garch33)
arma34.garch34 <- garchFit(formula = ~arma(3,4) + garch(3,4), data = return, cond.dist = "norm", trace = FALSE); summary(arma34.garch34)
arma34.garch41 <- garchFit(formula = ~arma(3,4) + garch(4,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma34.garch41)
arma34.garch42 <- garchFit(formula = ~arma(3,4) + garch(4,2), data = return, cond.dist = "norm", trace = FALSE); summary(arma34.garch42)
arma34.garch46 <- garchFit(formula = ~arma(3,4) + garch(4,6), data = return, cond.dist = "norm", trace = FALSE); summary(arma34.garch46)
arma34.garch51 <- garchFit(formula = ~arma(3,4) + garch(5,1), data = return, cond.dist = "norm", trace = FALSE); summary(arma34.garch51)
arma34.garch52 <- garchFit(formula = ~arma(3,4) + garch(5,2), data = return, cond.dist = "norm", trace = FALSE); summary(arma34.garch52)
arma34.garch53 <- garchFit(formula = ~arma(3,4) + garch(5,3), data = return, cond.dist = "norm", trace = FALSE); summary(arma34.garch53)
arma34.garch56 <- garchFit(formula = ~arma(3,4) + garch(5,6), data = return, cond.dist = "norm", trace = FALSE); summary(arma34.garch56)

#MA(2)-GARCH(4,2) lowest BIC = -3.758062

ma2.garch42 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,2)),
                          mean.model = list(armaOrder = c(0,2), include.mean = TRUE), 
                          distribution.model = "norm")

ma2garch42 <- ugarchfit(spec = ma2.garch42, data = return, out.sample = 2600)
plot(ma2garch42, which = "all")
ma2garch42

forecast_ma2garch42 <- ugarchforecast(ma2garch42, data = return, n.ahead = 5, n.roll = 2600)
print(forecast_ma2garch42)
plot(forecast_ma2garch42, which = "all")

arma41.garch11 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)), 
                             mean.model = list(armaOrder = c(4, 1), include.mean = TRUE), 
                             distribution.model = "norm")

arma41.garch42 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,2)), 
                             mean.model = list(armaOrder = c(4, 1), include.mean = TRUE), 
                             distribution.model = "norm")

arma41garch11 <- ugarchfit(spec = arma41.garch11, data = return, out.sample = 2600)
plot(arma41garch11, which = "all")
arma41garch11

arma41garch42 <- ugarchfit(spec = arma41.garch42, data = return, out.sample = 2600)
plot(arma41garch42, which = "all")
arma41garch42

arma21.11 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                        mean.model = list(armaOrder = c(2,1), include.mean = TRUE),
                        distribution.model = "norm")

arma21garch11 <- ugarchfit(spec = arma21.11, data = return, out.sample = 2600)


RMSE.ma2garch42 <- RMSE(return, ma2garch42@fit$fitted.values) #0.04367
RMSE.arma41garch11 <- RMSE(return, arma41garch11@fit$fitted.values) #RMSE = 0.04636
RMSE.arma41garch42 <- RMSE(return, arma41garch42@fit$fitted.values) #RMSE = 0.04669
RMSE.arma21garch11 <- RMSE(return, arma21garch11@fit$fitted.values) #RMSE =0.04371

const.mod <- Arima(return, order = c(0,0,0))
fit.const <- fitted(const.mod)
RMSE.const <- RMSE(return, fit.const) #RMSE = 0.0434
summary(const.mod)
res <- const.mod$residuals
tsdisplay(res)
tsdisplay(res^2)
garch32 <- garchFit(formula = ~garch(3,2), data = return)
RMSE.garch <- RMSE(return, fitted(garch32)) #RMSE = 0
eps <- garch32@sigma.t
returnpluseps <- return + 1.96 * eps
returnminuseps <- return - 1.96 * eps
plot(return)
lines(returnpluseps, lty = 2, col = "blue")
lines(returnminuseps, lty = 2, col = "red")
pred <- predict(garch32, n.ahead = 10)

returnforecast <- pred$meanForecast
returnforecastpluseps <- returnforecast + 1.96*(pred$meanError)
returnforecastminuseps <- returnforecast - 1.96*(pred$meanError)

returnI <- return + const.mod$coef
returnplusuI <- returnpluseps + pred$coef
returnminusuI <- returnminuseps + pred$coef

returnforecastI <- returnforecast + const.mod$coef
returnforecastplusepsI<- returnforecastpluseps  + const.mod$coef
returnforecastminusepsI<- returnforecastminuseps  + const.mod$coef

close.ts <- head(closing_price, -6)

close.ts.1 <- head(close.ts, -1)  
close.ts.2 <- tail(close.ts, (length(closing_price)-2))
close.ts.3 <- tail(closing_price, 6)

gold.ts.arga <- (returnI)*close.ts.1
gold.ts.argaplusu <- (returnplusuI)*close.ts.1
gold.ts.argaminusu <- (returnminusuI)*close.ts.1

par(mfrow=c(1,1))
plot(close.ts.2, type="l", main="Series (Since Inception)")
lines(gold.ts.arga, lty= 2, col = "green")
lines(gold.ts.argaplusu, lty= 2, col = "blue")
lines(gold.ts.argaminusu, lty= 2, col = "blue")

plot(tail(close.ts.2,100), type="l", main="Series (Last 100 Obs)")
lines(tail(gold.ts.arga,100), lty= 2,col = "green")
lines(tail(gold.ts.argaplusu,100), lty= 2, col = "blue")
lines(tail(gold.ts.argaminusu,100), lty= 2, col = "blue")



forecast_arma41garch11 <- ugarchforecast(arma41garch11, data = return, n.ahead = 5, n.roll = 2600)
print(forecast_arma41garch11)
plot(forecast_arma41garch11, which = "all")

forecast_arma41garch42 <- ugarchforecast(arma41garch42, data = return, n.ahead = 5, n.roll = 2600)
print(forecast_arma41garch42)
plot(forecast_arma41garch42, which = "all")

#Forecast of ineffcicient years. 
t1 <- window(return, start = as.Date("2013-10-01"), end = as.Date("2013-12-31"))
t2 <- window(return, start = as.Date("2014-01-01"), end = as.Date("2014-12-31")) #Inefficient
t3 <- window(return, start = as.Date("2015-01-01"), end = as.Date("2015-12-31")) #Inefficient
t4 <- window(return, start = as.Date("2016-01-01"), end = as.Date("2016-12-31"))
t5 <- window(return, start = as.Date("2017-01-01"), end = as.Date("2017-12-31")) 
t6 <- window(return, start = as.Date("2018-01-01"), end = as.Date("2018-12-31"))
t7 <- window(return, start = as.Date("2019-01-01"), end = as.Date("2019-12-31"))
t8 <- window(return, start = as.Date("2020-01-01"), end = as.Date("2020-12-31"))
t9 <- window(return, start = as.Date("2021-01-01"), end = as.Date("2021-04-19"))

#2014
arima2014 <- auto.arima(t2, stepwise=FALSE, allowmean = FALSE, allowdrift = FALSE, trace=TRUE, 
                        approximation=FALSE) #AR(2) ar1 = -0.03357, ar2 = -0.12005

ar2014 <- ar.mle(t2, aic = TRUE, order.max = NULL, intercept = TRUE) #AR(2) ar1 = -0.0415, ar2 = -0.1280

RMSE.arima2014 <- RMSE(fitted(arima2014), t2) #RMSE = 0.0393
RMSE.ar2014 <- RMSE(fitted(ar2014)[3:365], t2[3:365]) #RMSE = 0.0392

return2014 <- return[92:456,] 

par(mfrow = c(1,1))
McLeod.Li.test(y = return2014, main = "McLeod-Li test statistics for Daily return series")
#Signs of volatility

abs.14 <- abs(return2014)
sqr.14 <- return2014^2

par(mfrow=c(1,2))
acf(abs.14, ci.type = "ma", main = "ACF for abs. returns")
pacf(abs.14, main = "PACF plot for abs.returns")
eacf(abs.14) 
#GARCH(0,4), (1,1), (1,5), (1,6), (2,1), (2,2), (2,7), (3,1), (3,2), (3,3), (3,4), (3,6), (3,7),
#(4,0) --- (4,4), (4,7), (5,0) --- (5,7)

par(mfrow=c(1,2))
acf(sqr.14, ci.type = "ma", main = "ACF  for sqr. return")
pacf(sqr.14, main = "PACF for sqr. return")
eacf(sqr.14) 
#GARCH(1,1), (1,2), (2,1), (2,2), (3,3), (3,6), (3,7), (4,0) --- (4,4), (5,0) --- (5,5)

#Hence, (0,4), (1,1), (1,2), (1,5), (1,6), (2,1), (2,2), (2,7), (3,1) --- (3,4), (3,6), (3,7), 
#(4,0) --- (4,4), (4,7), (5,0) --- (5,7)
m.04.14 <- garch(return2014, order = c(1,1), trace = FALSE); summary(m.04.14)

m.11.14 <- garch(return2014, order = c(1,1), trace = FALSE); summary(m.11.14)

m.12.14 <- garch(return2014, order = c(1,2), trace = FALSE); summary(m.12.14)

m.15.14 <- garch(return2014, order = c(1,5), trace = FALSE); summary(m.15.14)

m.16.14 <- garch(return2014, order = c(1,6), trace = FALSE); summary(m.16.14)

m.21.14 <- garch(return2014, order = c(2,1), trace = FALSE); summary(m.21.14)

m.22.14 <- garch(return2014, order = c(2,2), trace = FALSE); summary(m.22.14)

m.27.14 <- garch(return2014, order = c(2,7), trace = FALSE); summary(m.27.14)

m.31.14 <- garch(return2014, order = c(3,1), trace = FALSE); summary(m.31.14)

m.32.14 <- garch(return2014, order = c(3,2), trace = FALSE); summary(m.32.14)

m.33.14 <- garch(return2014, order = c(3,3), trace = FALSE); summary(m.33.14)

m.34.14 <- garch(return2014, order = c(3,4), trace = FALSE); summary(m.34.14)

m.36.14 <- garch(return2014, order = c(3,6), trace = FALSE); summary(m.36.14)

m.37.14 <- garch(return2014, order = c(3,7), trace = FALSE); summary(m.37.14)

m.40.14 <- garch(return2014, order = c(4,0), trace = FALSE); summary(m.40.14)

m.41.14 <- garch(return2014, order = c(4,1), trace = FALSE); summary(m.41.14)

m.42.14 <- garch(return2014, order = c(4,2), trace = FALSE); summary(m.42.14)

m.43.14 <- garch(return2014, order = c(4,3), trace = FALSE); summary(m.43.14)

m.44.14 <- garch(return2014, order = c(4,4), trace = FALSE); summary(m.44.14)

m.47.14 <- garch(return2014, order = c(4,7), trace = FALSE); summary(m.47.14)

m.50.14 <- garch(return2014, order = c(5,0), trace = FALSE); summary(m.50.14)

m.51.14 <- garch(return2014, order = c(5,1), trace = FALSE); summary(m.51.14)

m.52.14 <- garch(return2014, order = c(5,2), trace = FALSE); summary(m.52.14)

m.53.14 <- garch(return2014, order = c(5,3), trace = FALSE); summary(m.53.14)

m.54.14 <- garch(return2014, order = c(5,4), trace = FALSE); summary(m.54.14)

m.55.14 <- garch(return2014, order = c(5,5), trace = FALSE); summary(m.55.14)

m.56.14 <- garch(return2014, order = c(5,6), trace = FALSE); summary(m.56.14)

m.57.14 <- garch(return2014, order = c(5,7), trace = FALSE); summary(m.57.14)

sc.AIC.14 <- AIC(m.04.14, m.11.14, m.16.14, m.21.14, m.22.14, m.27.14, m.31.14, m.32.14, m.33.14, m.34.14, 
                 m.40.14, m.41.14, m.42.14, m.43.14, m.44.14, m.47.14, m.50.14, m.51.14, m.52.14, m.53.14, 
                 m.54.14, m.55.14, m.56.14, m.57.14)

sort.score(sc.AIC.14, score = "aic") #GARCH(0,4) best 

ar2.garch04 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(0,4)), 
                             mean.model = list(armaOrder = c(2, 0), include.mean = TRUE), 
                             distribution.model = "norm")

ar2garch04 <- ugarchfit(spec = ar2.garch04, data = return2014, out.sample = 265)
plot(ar2garch04, which = "all")
armagarch

forecast_ar2garch04 <- ugarchforecast(ar2garch04, data = return2014, n.ahead = 5, n.roll = 250)
print(forecast_ar2garch04)
plot(forecast_ar2garch04, which = "all")

RMSE.armagarch2014 <- RMSE(fitted(ar2garch04), return2014[1:100]) #RMSE = 0.0473

#2015
arima2015 <- auto.arima(t3, stepwise=FALSE, allowmean = FALSE, allowdrift = FALSE, trace=TRUE, 
                        approximation=FALSE) 
#AR(5), ar1 = -0.02244, ar2 = -0.1092, ar3 = 0.0343, ar4 = -0.0588, ar5 = 0.1303

ar2015 <- ar.mle(t3, aic = TRUE, order.max = NULL, intercept = TRUE) 
#AR(8), ar1 = -0.0334, ar2 = -0.0842, ar3 = 0.0360, ar4 = -0.0489, ar5 = 0.1240, ar6 = 0.0620, ar7 = -0.1158, ar8 = -0.0856

RMSE.arima2015 <- RMSE(t3, fitted(arima2015)) #RMSE = 0.0371
RMSE.ar2015 <- RMSE(t3[9:365], fitted(ar2015)[9:365]) #RMSE = 0.0363

return2015 <- return[457:821]

par(mfrow = c(1,1))
McLeod.Li.test(y = return2015, main = "McLeod-Li test statistics for Daily return series")
#Signs of volatility

abs.15 <- abs(return2015)
sqr.15 <- return2015^2

par(mfrow=c(1,2))
acf(abs.15, ci.type = "ma", main = "ACF for abs. returns")
pacf(abs.15, main = "PACF plot for abs.returns")
eacf(abs.15) 
#GARCH(1,1), (1,2), (2,2), (3,1), (3,2), (3,3), (4,1) --- (4,4), (5,1) --- (5,5)

par(mfrow=c(1,2))
acf(sqr.15, ci.type = "ma", main = "ACF  for sqr. return")
pacf(sqr.15, main = "PACF for sqr. return")
eacf(sqr.15) 
#GARCH(1,1), (2,1), (2,2), (2,5) --- (2,7), (3,0) --- (3,3), (3,5) --- (3,7), (4,6), (4,7), (5,0) --- (5,5), (5,7)

#Hence, {(1,1), (1,2), (2,1), (2,2), (2,5) --- (2,7), (3,0) --- (3,3), (3,5) --- (3,7), (4,1) --- (4,4), (4,6),
#(4,7), (5,0) --- (5,5), (5,7)
m.11.15 <- garch(return2015, order = c(1,1), trace = FALSE); summary(m.11.15)

m.12.15 <- garch(return2015, order = c(1,2), trace = FALSE); summary(m.12.15)

m.21.15 <- garch(return2015, order = c(2,1), trace = FALSE); summary(m.21.15)

m.22.15 <- garch(return2015, order = c(2,2), trace = FALSE); summary(m.22.15)

m.25.15 <- garch(return2015, order = c(2,5), trace = FALSE); summary(m.25.15)

m.26.15 <- garch(return2015, order = c(2,6), trace = FALSE); summary(m.26.15)

m.27.15 <- garch(return2015, order = c(2,7), trace = FALSE); summary(m.27.15)

m.30.15 <- garch(return2015, order = c(3,0), trace = FALSE); summary(m.30.15)

m.31.15 <- garch(return2015, order = c(3,1), trace = FALSE); summary(m.31.15)

m.32.15 <- garch(return2015, order = c(3,2), trace = FALSE); summary(m.32.15)

m.33.15 <- garch(return2015, order = c(3,3), trace = FALSE); summary(m.33.15)

m.35.15 <- garch(return2015, order = c(3,5), trace = FALSE); summary(m.35.15)

m.36.15 <- garch(return2015, order = c(3,6), trace = FALSE); summary(m.36.15)

m.37.15 <- garch(return2015, order = c(3,7), trace = FALSE); summary(m.37.15)

m.41.15 <- garch(return2015, order = c(4,1), trace = FALSE); summary(m.41.15)

m.42.15 <- garch(return2015, order = c(4,2), trace = FALSE); summary(m.42.15)

m.43.15 <- garch(return2015, order = c(4,3), trace = FALSE); summary(m.43.15)

m.44.15 <- garch(return2015, order = c(4,4), trace = FALSE); summary(m.44.15)

m.46.15 <- garch(return2015, order = c(4,6), trace = FALSE); summary(m.46.15)

m.47.15 <- garch(return2015, order = c(4,7), trace = FALSE); summary(m.47.15)

m.50.15 <- garch(return2015, order = c(5,0), trace = FALSE); summary(m.50.15)

m.51.15 <- garch(return2015, order = c(5,1), trace = FALSE); summary(m.51.15)

m.52.15 <- garch(return2015, order = c(5,2), trace = FALSE); summary(m.52.15)

m.53.15 <- garch(return2015, order = c(5,3), trace = FALSE); summary(m.53.15)

m.54.15 <- garch(return2015, order = c(5,4), trace = FALSE); summary(m.54.15)

m.55.15 <- garch(return2015, order = c(5,5), trace = FALSE); summary(m.55.15)

m.57.15 <- garch(return2015, order = c(5,7), trace = FALSE); summary(m.57.15)

sc.AIC.15 <- AIC(m.11.15, m.12.15, m.21.15, m.22.15, m.25.15, m.26.15, m.27.15, m.30.15, m.31.15, m.32.15, 
                 m.33.15, m.35.15, m.36.15, m.37.15, m.41.15, m.42.15, m.43.15, m.44.15, m.46.15, m.47.15, 
                 m.50.15, m.51.15, m.52.15, m.53.15, m.54.15, m.55.15, m.57.15)

sort.score(sc.AIC.15, score = "aic") #GARCH(1,1) best 

ar5.garch11 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)), 
                          mean.model = list(armaOrder = c(5, 0), include.mean = TRUE), 
                          distribution.model = "norm")

ar5garch11 <- ugarchfit(spec = ar5.garch11, data = return2015, out.sample = 265)
plot(ar5garch11, which = "all")
ar5garch11

RMSE.armagarch2015 <- RMSE(fitted(ar5garch11), return2015[1:100]) #RMSE = 0.0463

forecast_ar5garch11 <- ugarchforecast(ar5garch11, data = return2015, n.ahead = 5, n.roll = 250)
print(forecast_ar5garch11)
plot(forecast_ar5garch11)

ar8.garch11 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)), 
                          mean.model = list(armaOrder = c(8, 0), include.mean = TRUE), 
                          distribution.model = "norm")

ar8garch11 <- ugarchfit(spec = ar8.garch11, data = return2015, out.sample = 265)
plot(ar8garch11, which = "all")
ar8garch11

RMSE.armagarch2015.2 <- RMSE(fitted(ar8garch11), return2015[1:100]) #RMSE = 0.0462

forecast_ar8garch11 <- ugarchforecast(ar8garch11, data = return2015, n.ahead = 5, n.roll = 250)
print(forecast_ar8garch11)
plot(forecast_ar8garch11)

#Monthly Windows: 
nov2013 <- window(return, start = as.Date("2013-11-01"), end = as.Date("2013-11-30"))
feb2014 <- window(return, start = as.Date("2014-02-01"), end = as.Date("2014-02-28"))
jul2014 <- window(return, start = as.Date("2014-07-01"), end = as.Date("2014-07-31"))
feb2015 <- window(return, start = as.Date("2015-02-01"), end = as.Date("2015-02-28"))
nov2015 <- window(return, start = as.Date("2015-11-01"), end = as.Date("2015-11-30"))
may2016 <- window(return, start = as.Date("2016-05-01"), end = as.Date("2016-05-31"))
jul2016 <- window(return, start = as.Date("2016-07-01"), end = as.Date("2016-07-31"))
oct2016 <- window(return, start = as.Date("2016-10-01"), end = as.Date("2016-10-31"))
jan2017 <- window(return, start = as.Date("2017-01-01"), end = as.Date("2017-01-31"))
nov2017 <- window(return, start = as.Date("2017-11-01"), end = as.Date("2017-11-30"))
mar2018 <- window(return, start = as.Date("2018-03-01"), end = as.Date("2018-03-31"))
may2018 <- window(return, start = as.Date("2018-05-01"), end = as.Date("2018-05-31"))
jun2018 <- window(return, start = as.Date("2018-06-01"), end = as.Date("2018-06-30"))
sep2018 <- window(return, start = as.Date("2018-09-01"), end = as.Date("2018-09-30"))
oct2018 <- window(return, start = as.Date("2018-10-01"), end = as.Date("2018-10-31"))
mar2019 <- window(return, start = as.Date("2019-03-01"), end = as.Date("2019-03-31"))
jul2019 <- window(return, start = as.Date("2019-07-01"), end = as.Date("2019-07-31"))
aug2019 <- window(return, start = as.Date("2019-08-01"), end = as.Date("2019-08-31"))
sep2019 <- window(return, start = as.Date("2019-09-01"), end = as.Date("2019-09-30"))
dec2019 <- window(return, start = as.Date("2019-12-01"), end = as.Date("2019-12-31"))
may2020 <- window(return, start = as.Date("2020-05-01"), end = as.Date("2020-05-31"))
jun2020 <- window(return, start = as.Date("2020-06-01"), end = as.Date("2020-06-30"))
sep2020 <- window(return, start = as.Date("2020-09-01"), end = as.Date("2022-09-30"))
mar2021 <- window(return, start = as.Date("2021-03-01"), end = as.Date("2021-03-31"))

#Month with highest AMIM - Oct. 2016 = 0.5026904
arma.oct2016 <- auto.arima(oct2016, trace = TRUE, stepwise = FALSE, approximation = FALSE,
                           allowmean = FALSE, allowdrift = FALSE) #ARMA(0,0,0)

ar.oct2016 <- ar.mle(oct2016, aic = TRUE, order.max = NULL, intercept = TRUE) #AR(11)

RMSE.oct2016.1 <- RMSE(fitted(arma.oct2016), oct2016) #RMSE = 0.01267
RMSE.oct2016.2 <- RMSE(fitted(ar.oct2016)[12:31], oct2016[12:31]) #RMSE = 0.00814

return.oct2016 <- return[1096:1126,]

par(mfrow = c(1,1))
McLeod.Li.test(y = return.oct2016, main = "McLeod-Li test statistics for Daily return series")
#No signs of volatility

T_oct2016 <- length(return.oct2016)
T_trn.oct2016 <- round(0.70*T_oct2016)
T_tst.oct2016 <- T_oct2016 - T_trn.oct2016
returns_trn.oct2016 <- return.oct2016[1:T_trn.oct2016]
returns_tst.oct2016 <- return.oct2016[-c(1:T_trn.oct2016)]

out_of_sample <- round(T_oct2016 - T_trn.oct2016)
dates_out_of_sample <- tail(index(return.oct2016), out_of_sample)

#Specifying and AR(11) and fit it. 
ar_spec_ar11_oct2016 <- arfimaspec(mean.model = list(armaOrder = c(11,0), include.mean = TRUE))
ar11_fit_oct2016 <- arfimafit(spec = ar_spec_ar11_oct2016, data = return.oct2016, 
                              out.sample = T_tst.oct2016)

#Static forecast AR(11)
ar11_fore_returns_oct2016 <- xts(arfimaforecast(ar11_fit_oct2016, n.ahead = 1, 
                                                n.roll = T_tst.oct2016 - 1)@forecast$seriesFor[1, ],
                                 dates_out_of_sample)

#Rolling forecast AR(11)
modelroll_ar11_oct2016 <- arfimaroll(spec = ar_spec_ar11_oct2016, data = return.oct2016, n.ahead = 1, 
                                 forecast.length = T_tst.oct2016, refit.every = 1, refit.window = "moving")
ar11_rolling_fore_returns_oct2016 <- xts(modelroll_ar11_oct2016@forecast$density$Mu, dates_out_of_sample)

#plot 
plot(cbind("AR(11) Static forecast"  = ar11_fore_returns_oct2016,
           "AR(11) Rolling forecast" = ar11_rolling_fore_returns_oct2016,
           "Return Oct 2016"= return.oct2016),
     col = c("blue", "red", "black"), lwd = 2, ylim = c(-0.15, 0.10),
     main = "Rolling and Static Forecast with AR(11) model", legend.loc = "topleft")

#Specifying ARMA(0,0) and fitting it
arma.oct2016.spec <- arfimaspec(mean.model = list(armaOrder = c(0,0), include.mean = TRUE))
arma_fit_oct2016 <- arfimafit(spec = arma.oct2016.spec, data = return.oct2016, 
                              out.sample = T_tst.oct2016)

#Static forecast ARMA(0,0)
arma_fore_returns_oct2016 <- xts(arfimaforecast(arma_fit_oct2016, n.ahead = 1, 
                                                n.roll = T_tst.oct2016 - 1)@forecast$seriesFor[1, ],
                                 dates_out_of_sample)

#Rolling forecast ARMA(0,0)
modelroll_arma_oct2016 <- arfimaroll(spec = arma.oct2016.spec, data = return.oct2016, n.ahead = 1, 
                                     forecast.length = T_tst.oct2016, refit.every = 1, refit.window = "moving")
arma_rolling_fore_returns_oct2016 <- xts(modelroll_arma_oct2016@forecast$density$Mu, dates_out_of_sample)

#plot 
plot(cbind("ARMA(0,0) Static forecast"  = arma_fore_returns_oct2016,
           "ARMA(0,0) Rolling forecast" = arma_rolling_fore_returns_oct2016,
           "Return Oct 2016"= return.oct2016),
     col = c("blue", "red", "black"), lwd = 2, ylim = c(-0.15, 0.10),
     main = "Rolling and Static Forecast with AR(11) model", legend.loc = "topleft")

#Month with smallest AMIM, May 2018, AMIM = 0.0019
arma.may2018 <- auto.arima(may2018, trace = TRUE, stepwise = FALSE, approximation = FALSE,
                              allowmean = FALSE, allowdrift = FALSE, ic = "aic") 
#ARMA(0,0,3), ma1 = 0.0375, ma2 = 0.321, ma3 = -0.4121

ar.may2018 <- ar.mle(may2018, aic = TRUE, order.max = NULL, intercept = TRUE) 
#AR(3), ar1 = -0.1247, ar2 = 0.1467, ar3 = -0.3265

RMSE.may2018.1 <- RMSE(fitted(arma.may2018), may2018) #0.0296
RMSE.may2018.2 <- RMSE(fitted(ar.may2018)[4:31], may2018[4:31]) #0.0298

return.may2018 <- return[2161:2190,] 

par(mfrow = c(1,1))
McLeod.Li.test(y = return.may2018, main = "McLeod-Li test statistics for Daily return series")
#No signs of volatility

T_may2018 <- length(return.may2018)
T_trn.may2018 <- round(0.70*T_may2018)
T_tst.may2018 <- T_may2018 - T_trn.may2018
returns_trn.may2018 <- return.may2018[1:T_trn.may2018]
returns_tst.may2018 <- return.may2018[-c(1:T_trn.may2018)]

out_of_sample.may2018 <- round(T_may2018 - T_trn.may2018)
dates_out_of_sample.may2018 <- tail(index(return.may2018), out_of_sample.may2018)

#Specifying and AR(3) and fit it. 
ar_spec_ar3_may2018 <- arfimaspec(mean.model = list(armaOrder = c(3,0), include.mean = TRUE))
ar3_fit_may2018 <- arfimafit(spec = ar_spec_ar3_may2018, data = return.may2018, out.sample = T_tst.may2018)

#Static forecast AR(3)
ar3_fore_returns_may2018 <- xts(arfimaforecast(ar3_fit_may2018, n.ahead = 1, 
                                                n.roll = T_tst.may2018 - 1)@forecast$seriesFor[1, ], 
                                 dates_out_of_sample.may2018)

#Rolling forecast AR(3)
modelroll_ar3_may2018 <- arfimaroll(spec = ar_spec_ar3_may2018, data = return.may2018, n.ahead = 1, 
                                     forecast.length = T_tst.may2018, refit.every = 1, 
                                    refit.window = "moving")

ar3_rolling_fore_returns_may2018 <- xts(modelroll_ar3_may2018@forecast$density$Mu, dates_out_of_sample.may2018)

#plot 
plot(cbind("AR(3) Static forecast"  = ar3_fore_returns_may2018,
           "AR(3) Rolling forecast" = ar3_rolling_fore_returns_may2018,
           "Returns May 2018" = return.may2018),
     col = c("blue", "red", "black"), lwd = 2, ylim = c(-0.15, 0.10),
     main = "Rolling and Static Forecast with AR(3) model", legend.loc = "topleft")

#Forecast using MA(3)
#Specifying and MA(3) and fit it. 
ma_spec_ma3_may2018 <- arfimaspec(mean.model = list(armaOrder = c(0,3), include.mean = TRUE))
ma3_fit_may2018 <- arfimafit(spec = ma_spec_ma3_may2018, data = return.may2018, out.sample = T_tst.may2018)

#Static forecast MA(3)
ma3_fore_returns_may2018 <- xts(arfimaforecast(ma3_fit_may2018, n.ahead = 1, 
                                               n.roll = T_tst.may2018 - 1)@forecast$seriesFor[1, ], 
                                dates_out_of_sample.may2018)

#Rolling forecast MA(3)
modelroll_ma3_may2018 <- arfimaroll(spec = ma_spec_ma3_may2018, data = return.may2018, n.ahead = 1, 
                                    forecast.length = T_tst.may2018, refit.every = 1, refit.window = "moving")

ma3_rolling_fore_returns_may2018 <- xts(modelroll_ma3_may2018@forecast$density$Mu, dates_out_of_sample.may2018)

#plot 
plot(cbind("MA(3) Static forecast"  = ma3_fore_returns_may2018,
           "MA(3) Rolling forecast" = ma3_rolling_fore_returns_may2018,
           "Returns May 2018" = return.may2018),
     col = c("blue", "red", "black"), lwd = 2, ylim = c(-0.20, 0.20),
     main = "Rolling and Static Forecast with MA(3) model", legend.loc = "topleft")


#Month with AMIM in the middle of Sep 2018 AMIM = 0.266
arma.sep2018 <- auto.arima(sep2018, trace = TRUE, stepwise = FALSE, approximation = FALSE,
                           allowmean = FALSE, allowdrift = FALSE, ic = "aic") 
#ARMA(0,0,3), ma1 = 0.5281156, ma2 = 0.3158695, ma3 = 0.6554238

ar.sep2018 <- ar.mle(sep2018, aic = TRUE, order.max = NULL, intercept = TRUE) 
#AR(4), ar1 = 0.2689, ar2 = 0.0880, ar3 = 0.2644, ar4 = -0.5190

RMSE.sep2018.1 <- RMSE(fitted(arma.sep2018), sep2018) # RMSE = 0.0182
RMSE.sep2018.2 <- RMSE(fitted(ar.sep2018)[5:30], sep2018[5:30]) #RMSE = 0.0186

par(mfrow = c(1,1))
McLeod.Li.test(y = sep2018, main = "McLeod-Li test statistics for Daily return series")
#No signs of volatility

return.sep2018 <- return[1796:1825,] 

T_sep2018 <- length(return.sep2018)
T_trn.sep2018 <- round(0.70*T_sep2018)
T_tst.sep2018 <- T_sep2018 - T_trn.sep2018
returns_trn.sep2018 <- return.sep2018[1:T_trn.sep2018]
returns_tst.sep2018 <- return.sep2018[-c(1:T_trn.sep2018)]

out_of_sample.sep2018 <- round(T_sep2018 - T_trn.sep2018)
dates_out_of_sample.sep2018 <- tail(index(return.sep2018), out_of_sample.sep2018)

#Specifying and AR(4) and fit it. 
ar_spec_ar4_sep2018 <- arfimaspec(mean.model = list(armaOrder = c(4,0), include.mean = TRUE))
ar1_fit_sep2018 <- arfimafit(spec = ar_spec_ar4_sep2018, data = return.sep2018, 
                             out.sample = T_tst.sep2018)

#Static forecast AR(4)
ar4_fore_returns_sep2018 <- xts(arfimaforecast(ar1_fit_sep2018, 
                                               n.ahead = 1, 
                                               n.roll = T_tst.sep2018 - 1)@forecast$seriesFor[1, ], 
                                dates_out_of_sample.sep2018)

#Rolling forecast AR(4)
modelroll_ar4_sep2018 <- arfimaroll(spec = ar_spec_ar4_sep2018, data = return.sep2018, n.ahead = 1, 
                                    forecast.length = T_tst.sep2018, refit.every = 1, refit.window = "moving")

ar4_rolling_fore_returns_sep2018 <- xts(modelroll_ar4_sep2018@forecast$density$Mu, 
                                        dates_out_of_sample.sep2018)

#plot 
plot(cbind("AR(4) Static forecast"  = ar4_fore_returns_sep2018,
           "AR(4) Rolling forecast" = ar4_rolling_fore_returns_sep2018,
           "Logreturn Sep 2018"= return.sep2018),
     col = c("blue", "red", "black"), lwd = 2, ylim = c(-0.15, 0.10),
     main = "Rolling and Static Forecast with AR(4) model", legend.loc = "topleft")

#Forecast using MA(3)
#Specifying and MA(3) and fit it. 
ma_spec_ma3_sep2018 <- arfimaspec(mean.model = list(armaOrder = c(0,3), include.mean = TRUE))
ma3_fit_sep2018 <- arfimafit(spec = ma_spec_ma3_sep2018, data = return.sep2018, 
                             out.sample = T_tst.sep2018)

#Static forecast MA(3)
ma3_fore_returns_sep2018 <- xts(arfimaforecast(ma3_fit_sep2018, n.ahead = 1, 
                                               n.roll = T_tst.sep2018 - 1)@forecast$seriesFor[1, ], 
                                dates_out_of_sample.sep2018)

#Rolling forecast MA(3)
modelroll_ma3_sep2018 <- arfimaroll(spec = ma_spec_ma3_sep2018, data = return.sep2018, n.ahead = 1, 
                                    forecast.length = T_tst.sep2018, refit.every = 1, refit.window = "moving")

ma3_rolling_fore_returns_sep2018 <- xts(modelroll_ma3_sep2018@forecast$density$Mu, 
                                        dates_out_of_sample.sep2018)

#plot 
plot(cbind("MA(3) Static forecast"  = ma3_fore_returns_sep2018,
           "MA(3) Rolling forecast" = ma3_rolling_fore_returns_sep2018,
           "Logreturn Sep 2018"= return.sep2018),
     col = c("blue", "red", "black"), lwd = 2, ylim = c(-0.45, 0.75),
     main = "Rolling and Static Forecast with MA(3) model", legend.loc = "topleft")

