install.packages("janitor")
library(janitor)
library(magrittr)

Bitcoin_df<- read.table("BitcoinDataCoinDesk.csv", header = TRUE, sep = ",", 
                        stringsAsFactors = FALSE)

dates <- as.Date(Bitcoin_df$Date)

td <- seq(as.Date("2013-10-01"), as.Date("2021-04-19"), "days")

closing_price <- zoo(x = Bitcoin_df$Closing.Price..USD., order.by = td)

return <- diff(closing_price)

t1 <- window(return, start = as.Date("2013-10-01"), end = as.Date("2013-12-31"))
t2 <- window(return, start = as.Date("2014-01-01"), end = as.Date("2014-12-31"))
t3 <- window(return, start = as.Date("2015-01-01"), end = as.Date("2015-12-31"))
t4 <- window(return, start = as.Date("2016-01-01"), end = as.Date("2016-12-31"))
t5 <- window(return, start = as.Date("2017-01-01"), end = as.Date("2017-12-31"))
t6 <- window(return, start = as.Date("2018-01-01"), end = as.Date("2018-12-31"))
t7 <- window(return, start = as.Date("2019-01-01"), end = as.Date("2019-12-31"))
t8 <- window(return, start = as.Date("2020-01-01"), end = as.Date("2020-12-31"))
t9 <- window(return, start = as.Date("2021-01-01"), end = as.Date("2021-04-19"))

ar1.1 <- ar.mle(t1, aic = TRUE, order.max = NULL, intercept = TRUE); ar1.1 #AR(5)
ar2.2 <- ar.mle(t2, aic = TRUE, order.max = NULL, intercept = TRUE); ar2.2 #AR(2)
ar3.3 <- ar.mle(t3, aic = TRUE, order.max = NULL, intercept = TRUE); ar3.3 #AR(7)
ar4.4 <- ar.mle(t4, aic = TRUE, order.max = NULL, intercept = TRUE); ar4.4 #AR(0)
ar5.5 <- ar.mle(t5, aic = TRUE, order.max = NULL, intercept = TRUE); ar5.5 #AR(12)
ar6.6 <- ar.mle(t6, aic = TRUE, order.max = NULL, intercept = TRUE); ar6.6 #AR(0)
ar7.7 <- ar.mle(t7, aic = TRUE, order.max = NULL, intercept = TRUE); ar7.7 #AR(0)
ar8.8 <- ar.mle(t8, aic = TRUE, order.max = NULL, intercept = TRUE); ar8.8 #AR(1)
ar9.9 <- ar.mle(t9, aic = TRUE, order.max = NULL, intercept = TRUE); ar9.9 #AR(0)

covar1 <- ar1.1$asy.var.coef; covar2 <- ar2.2$asy.var.coef; covar3 <- ar3.3$asy.var.coef; covar4 <- 0
covar5 <- ar5.5$asy.var.coef; covar6 <- 0; covar7 <- 0; covar8 <- ar8.8$asy.var.coef; covar9 <- 0

U1 <- chol(covar1)
T1 <- t(U1)
InvT1 <- solve(T1)
beta1 <- ar1.1$ar
beta.stand1 <- InvT1 %*% beta1
mimt1 <- (sum(abs(beta.stand1)))/(1+sum(abs(beta.stand1)))

U2 <- chol(covar2)
T2 <- t(U2)
InvT2 <- solve(T2)
beta2 <- ar2.2$ar
beta.stand2 <- InvT2 %*% beta2
mimt2 <- (sum(abs(beta.stand2)))/(1+sum(abs(beta.stand2)))

U3 <- chol(covar3)
T3 <- t(U3)
InvT3 <- solve(T3)
beta3 <- ar3.3$ar
beta.stand3 <- InvT3 %*% beta3
mimt3 <- (sum(abs(beta.stand3)))/(1+sum(abs(beta.stand3)))

mimt4 <- 0

U5 <- chol(covar5)
T5 <- t(U5)
InvT5 <- solve(T5)
beta5 <- ar5.5$ar
beta.stand5 <- InvT5 %*% beta5
mimt5 <- (sum(abs(beta.stand5)))/(1+sum(abs(beta.stand5)))

mimt6 <- 0; mimt7 <- 0

U8 <- chol(covar8)
T8 <- t(U8)
InvT8 <- solve(T8)
beta8 <- ar8.8$ar
beta.stand8 <- InvT8 %*% beta8
mimt8 <- (sum(abs(beta.stand8)))/(1+sum(abs(beta.stand8)))

mimt9 <- 0

stand.beta <- rbind(sum(abs(beta.stand1)), sum(abs(beta.stand2)), sum(abs(beta.stand3)), 0, 
                    sum(abs(beta.stand5)), 0, 0, sum(abs(beta.stand8)), 0)
mim <- rbind(mimt1, mimt2, mimt3, mimt4, mimt5, mimt6, mimt7, mimt8, mimt9)
data <- data.frame(stand.beta, mim)
ggplot(data = data, aes(x = stand.beta, y = mim)) + geom_line()

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

AMIM1 <- (mimt1 - 0.8643)/(1 - 0.8643); AMIM1
AMIM2 <- (mimt2 - 0.7605)/(1 - 0.7605); AMIM2
AMIM3 <- (mimt3 - 0.8932)/(1 - 0.8932); AMIM3
AMIM4 <- 0
AMIM5 <- (mimt5 - 0.9294)/(1 - 0.9294); AMIM5
AMIM6 <- 0
AMIM7 <- 0
AMIM8 <- (mimt8 - 0.6619)/(1 - 0.6619); AMIM8
AMIM9 <- 0

AMIM <- rbind(AMIM1, AMIM2, AMIM3, AMIM4, AMIM5, AMIM6, AMIM7, AMIM8, AMIM9)
plot(AMIM, type = "l"); abline(h = 0, col = "blue", lty = 3)

ar.all <- ar.mle(return, aic = TRUE, order.max = NULL, intercept = TRUE); ar.all #AR(11)

covar.all <- ar.all$asy.var.coef
U.all <- chol(covar.all)
T.all <- t(U.all)
InvT.all <- solve(T.all)
beta.all <- ar.all$ar
beta.stand.all <- InvT.all %*% beta.all
mim.all <- (sum(abs(beta.stand.all)))/(1+sum(abs(beta.stand.all)))

AMIM.all <- (mim.all - 0.9244)/(1 - 0.9244) #AMIM = 0.54833, hence inefficient market overall

bitcoin = read.csv('BitcoinDataCoinDesk.csv') %>%
  tibble::as_tibble() %>%
  janitor::clean_names() %>%
  dplyr::mutate(
    date = as.Date(date),
    log_return = c(NA, diff(log(closing_price_usd)))
  ) %>%
  tidyr::drop_na(log_return)

mim <- bitcoin %>%
  dplyr::mutate(
    year = lubridate::year(date),
    ym = format(date, '%Y-%m')
  ) %>%
  #dplyr::group_by(identifier = year) %>% # år
  dplyr::group_by(identifier = ym) %>% # måned
  # dplyr::group_by(identifier = date) %>% # dag
  dplyr::group_split() %>%
  purrr::map_dfr(
    .f = function(dat) {
      # browser()
      
      suppressWarnings({
        adf_test = tseries::adf.test(dat$log_return)
        adf_test_k0 = tseries::adf.test(dat$log_return, k = 0)
        pp_test = tseries::pp.test(dat$log_return)
      })
      
      ar_object = ar.mle(dat$log_return, aic = TRUE, order.max = NULL, intercept = TRUE)
      
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
          identifier = dat$identifier %>% unique(),
          first_day = dat$date %>% dplyr::first(),
          last_day = dat$date %>% dplyr::last(),
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
plot.ts(mim[,9])

#Around 29 months out of the 91 months of data, are showing signs of inefficiency due to high MIM-value.
#Calculating the AMIM value for the months showing signs of inefficiency;
AMIM.nov2013 <- (mim[2,9] - 0.81105)/(1 - 0.81105); AMIM.nov2013 #AR(3)
AMIM.feb2014 <- (mim[5,9] - 0.7605)/(1 - 0.7605); AMIM.feb2014 #AR(2)
AMIM.jul2014 <- (mim[10,9] - 0.6619)/(1 - 0.6619); AMIM.jul2014 #AR(1)
AMIM.nov2014 <- (mim[14,9] - 0.6619)/(1 - 0.6619); AMIM.nov2014 #AR(1), efficient
AMIM.feb2015 <- (mim[17,9] - 0.7605)/(1 - 0.7605); AMIM.feb2015 #AR(2)
AMIM.nov2015 <- (mim[26,9] - 0.9033)/(1 - 0.9033); AMIM.nov2015 #AR(8)
AMIM.dec2015 <- (mim[27,9] - 0.6619)/(1 - 0.6619); AMIM.dec2015 #AR(1), efficient
AMIM.feb2016 <- (mim[29,9] - 0.6619)/(1 - 0.6619); AMIM.feb2016 #AR(1), efficient
AMIM.may2016 <- (mim[32,9] - 0.8424)/(1 - 0.8424); AMIM.may2016 #AR(4)
AMIM.jul2016 <- (mim[34,9] - 0.6619)/(1 - 0.6619); AMIM.jul2016 #AR(1)
AMIM.oct2016 <- (mim[37,9] - 0.9244)/(1 - 0.9244); AMIM.oct2016 #AR(11)
AMIM.jan2017 <- (mim[40,9] - 0.8643)/(1 - 0.8643); AMIM.jan2017 #AR(5)
AMIM.sep2017 <- (mim[48,9] - 0.6619)/(1 - 0.6619); AMIM.sep2017 #AR(1), efficient
AMIM.nov2017 <- (mim[50,9] - 0.8424)/(1 - 0.8424); AMIM.nov2017 #AR(4)
AMIM.may2018 <- (mim[56,9] - 0.81105)/(1 - 0.81105); AMIM.may2018 #AR(3), efficient
AMIM.jun2018 <- (mim[57,9] - 0.9033)/(1 - 0.9033); AMIM.jun2018 #AR(8), efficient
AMIM.sep2018 <- (mim[60,9] - 0.8424)/(1 - 0.8424); AMIM.sep2018 #AR(4)
AMIM.oct2018 <- (mim[61,9] - 0.8424)/(1 - 0.8424); AMIM.oct2018 #AR(4)
AMIM.mar2019 <- (mim[66,9] - 0.9033)/(1 - 0.9033); AMIM.mar2019 #AR(8)
AMIM.jun2019 <- (mim[69,9] - 0.6619)/(1 - 0.6619); AMIM.jun2019 #AR(1), efficient
AMIM.jul2019 <- (mim[70,9] - 0.9033)/(1 - 0.9033); AMIM.jul2019 #AR(8)
AMIM.aug2019 <- (mim[71,9] - 0.9116)/(1 - 0.9116); AMIM.aug2019 #AR(9)
AMIM.sep2019 <- (mim[72,9] - 0.6619)/(1 - 0.6619); AMIM.sep2019 #AR(1)
AMIM.oct2019 <- (mim[73,9] - 0.6619)/(1 - 0.6619); AMIM.oct2019 #AR(1), efficient
AMIM.dec2019 <- (mim[75,9] - 0.7605)/(1 - 0.7605); AMIM.dec2019 #AR(2)
AMIM.may2020 <- (mim[80,9] - 0.8932)/(1 - 0.8932); AMIM.may2020 #AR(7)
AMIM.jun2020 <- (mim[81,9] - 0.7605)/(1 - 0.7605); AMIM.jun2020 #AR(2)
AMIM.aug2020 <- (mim[83,9] - 0.6619)/(1 - 0.6619); AMIM.aug2020 #AR(1), efficient
AMIM.nov2020 <- (mim[86,9] - 0.7605)/(1 - 0.7605); AMIM.nov2020 #AR(2), efficient 

AMIM.monthly <- rbind(0, AMIM.nov2013, 0, 0, AMIM.feb2014, 0, 0, 0, 0, AMIM.jul2014, 0, 0, 0, AMIM.nov2014, 
                     0, 0, AMIM.feb2015, 0, 0, 0, 0, 0, 0, 0, 0, AMIM.nov2015, AMIM.dec2015, 0, 
                     AMIM.feb2016, 0, 0, AMIM.may2016, 0, AMIM.jul2016, 0, 0, AMIM.oct2016, 0, 0,
                     AMIM.jan2017, 0, 0, 0, 0, 0, 0, 0, AMIM.sep2017, 0, AMIM.nov2017, 0, 0, 0, 0, 0, 
                     AMIM.may2018, AMIM.jun2018, 0, 0, AMIM.sep2018, AMIM.oct2018, 0, 0, 0, 0, 0,
                     AMIM.mar2019, 0, 0, AMIM.jun2019, AMIM.jul2019, AMIM.aug2019, AMIM.sep2019, AMIM.oct2019, 
                     0, AMIM.dec2019, 0, 0, 0, 0, AMIM.may2020, AMIM.jun2020, 0, AMIM.aug2020, 0, 0, 
                     AMIM.nov2020, 0, 0, 0, 0, 0)
plot.ts(AMIM.monthly); abline(h = 0, lty = 2, col = "blue")
