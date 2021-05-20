library(janitor); library(magrittr); library(arfima); library(astsa); library(forecast); library(fGarch)
library(rugarch); library(tseries); library(timeSeries); library(ltsa); library(timeDate); library(zoo)

Bitcoin_df<- read.table("BitcoinDataCoinDesk.csv", header = TRUE, sep = ",", 
                        stringsAsFactors = FALSE)

dates <- as.Date(Bitcoin_df$Date)

td <- seq(from = as.Date("2013-10-01"), to = as.Date("2021-04-19"), by = "days")

closing_price <- zoo(x = Bitcoin_df$Closing.Price..USD., order.by = td)

return <- diff(closing_price) /closing_price[1:2757]

#All MIM and AMIM
ar.all <- ar.mle(return, aic = TRUE, order.max = NULL, intercept = TRUE); ar.all #AR(10)

covar.all <- ar.all$asy.var.coef
U.all <- chol(covar.all)
T.all <- t(U.all)
InvT.all <- solve(T.all)
beta.all <- ar.all$ar
beta.stand.all <- InvT.all %*% beta.all
mim.all <- (sum(abs(beta.stand.all)))/(1+sum(abs(beta.stand.all))); mim.all

AMIM.all <- (mim.all - 0.9184596)/(1 - 0.9184596); AMIM.all #AMIM = 0.3481811, hence inefficient market overall

#Code for yearly and monthly MIM and AMIM values
bitcoin = read.csv('BitcoinDataCoinDesk.csv') %>%
  tibble::as_tibble() %>%
  janitor::clean_names() %>%
  dplyr::mutate(
    date = as.Date(date),
    return = c(NA, (diff(closing_price_usd) / closing_price_usd[1:2757]))
  ) %>%
  #tidyr::drop_na(log_return)
  tidyr::drop_na(return)
  
mim <- bitcoin %>%
  dplyr::mutate(
    year = lubridate::year(date),
    ym = format(date, '%Y-%m')
  ) %>%
  #dplyr::group_by(identifier = year) %>% # år
  dplyr::group_by(identifier = ym) %>% # måned
  # dplyr::group_by(identifier = date) %>% # dag #Disse tre kan ind-/udkommenteres efter behov
  dplyr::group_split() %>%
  purrr::map_dfr(
    .f = function(dat) {
      # browser()
      
      suppressWarnings({
        #adf_test = tseries::adf.test(dat$log_return)
        #adf_test_k0 = tseries::adf.test(dat$log_return, k = 0)
        #pp_test = tseries::pp.test(dat$log_return)
        adf_test = tseries::adf.test(dat$return)
        adf_test_k0 = tseries::adf.test(dat$return, k = 0)
        pp_test = tseries::pp.test(dat$return)
      })
      
      #ar_object = ar.mle(dat$log_return, aic = TRUE, order.max = NULL, intercept = TRUE)
      ar_object = ar.mle(dat$return, aic = TRUE, order.max = NULL, intercept = TRUE)
      
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

#2014 AR(2), 2015 AR(8), 2018 AR(1), and 2020 AR(4) MIM bigger than 0. 
mim.return.yearly <- cbind(mim[1,9], mim[2,9], mim[3,9], mim[4,9], mim[5,9], mim[6,9], mim[7,9],
                           mim[8,9], mim[9,9])

t.yearly <- lubridate::year(dates)
t.yearly <- cbind(t.yearly[1], t.yearly[93], t.yearly[458], t.yearly[823], t.yearly[1189], 
                  t.yearly[1554], t.yearly[1920], t.yearly[2286], t.yearly[2652])

plot(y = mim.return.yearly, x = t.yearly, type = "l", ylab = "MIM_t", xlab = "Years", main = "Yearly MIM")

amim.2013 <- 0; amim.2014 <- (mim[2,9] - 0.7605)/(1 - 0.7605)
amim.2015 <- (mim[3,9] - 0.903343)/(1 - 0.903343); amim.2016 <- 0; amim.2017 <- 0
amim.2018 <- (mim[6,9] - 0.6618747)/(1 - 0.6618747); amim.2019 <- 0
amim.2020 <- (mim[8,9] - 0.8423915)/(1 - 0.8423915); amim.2021 <- 0

amim.yearly <- cbind(amim.2013, amim.2014, amim.2015, amim.2016, amim.2017, amim.2018,
                     amim.2019, amim.2020, amim.2021)

plot(y = amim.yearly, x = t.yearly, type = "l", 
     ylab = "AMIM_t", xlab = "Years", main = "Yearly AMIM"); abline(h = 0, lty = 2, col = "blue")
#The years, which are inefficient, are 2014 and 2015


#Monthly MIM and AMIM values; 32 months out of 91 show signs of inefficiency. 
mim.monthly <- cbind(mim[1,9], mim[2,9], mim[3,9], mim[4,9], mim[5,9], mim[6,9], mim[7,9], mim[8,9], 
                       mim[9,9], mim[10,9], mim[11,9], mim[12,9], mim[13,9], mim[14,9], mim[15,9], mim[16,9], 
                       mim[17,9], mim[18,9], mim[19,9], mim[20,9], mim[21,9], mim[22,9], mim[23,9], mim[24,9], 
                       mim[25,9], mim[26,9], mim[27,9], mim[28,9], mim[29,9], mim[30,9], mim[31,9], mim[32,9], 
                       mim[33,9], mim[34,9], mim[35,9], mim[36,9], mim[37,9], mim[38,9], mim[39,9], mim[40,9], 
                       mim[41,9], mim[42,9], mim[43,9], mim[44,9], mim[45,9], mim[46,9], mim[47,9], mim[48,9], 
                       mim[49,9], mim[50,9], mim[51,9], mim[52,9], mim[53,9], mim[54,9], mim[55,9], mim[56,9], 
                       mim[57,9], mim[58,9] , mim[59,9], mim[60,9], mim[61,9], mim[62,9], mim[63,9], mim[64,9], 
                       mim[65,9], mim[66,9], mim[67,9], mim[68,9], mim[69,9], mim[70,9], mim[71,9], mim[72,9], 
                       mim[73,9], mim[74,9], mim[75,9], mim[76,9], mim[77,9], mim[78,9], mim[79,9], mim[80,9], 
                       mim[81,9], mim[82,9], mim[83,9], mim[84,9], mim[85,9], mim[86,9], mim[87,9], mim[88,9], 
                       mim[89,9], mim[90,9], mim[91,9])

td.month <- seq(from = as.Date("2013-10-01"), to = as.Date("2021-04-19"), by = "months")
td.month <- as.Date(td.month)
plot(y = mim.monthly, x = td.month, ylab = "MIM_t", xlab = "Years", main = "Monthly MIM", type = "l")

#Around 32 months out of the 91 months of data, are showing signs of inefficiency due to high MIM-value.
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
AMIM.mar2018 <- (mim[54,9] - 0.9244)/(1 - 0.9244); AMIM.mar2018 #AR(11)
AMIM.may2018 <- (mim[56,9] - 0.81105)/(1 - 0.81105); AMIM.may2018 #AR(3)
AMIM.jun2018 <- (mim[57,9] - 0.9033)/(1 - 0.9033); AMIM.jun2018 #AR(8)
AMIM.sep2018 <- (mim[60,9] - 0.8424)/(1 - 0.8424); AMIM.sep2018 #AR(4)
AMIM.oct2018 <- (mim[61,9] - 0.8424)/(1 - 0.8424); AMIM.oct2018 #AR(4)
AMIM.mar2019 <- (mim[66,9] - 0.9116)/(1 - 0.9116); AMIM.mar2019 #AR(9)
AMIM.jun2019 <- (mim[69,9] - 0.6619)/(1 - 0.6619); AMIM.jun2019 #AR(1), efficient
AMIM.jul2019 <- (mim[70,9] - 0.9033)/(1 - 0.9033); AMIM.jul2019 #AR(8)
AMIM.aug2019 <- (mim[71,9] - 0.9116)/(1 - 0.9116); AMIM.aug2019 #AR(9)
AMIM.sep2019 <- (mim[72,9] - 0.6619)/(1 - 0.6619); AMIM.sep2019 #AR(1)
AMIM.oct2019 <- (mim[73,9] - 0.6619)/(1 - 0.6619); AMIM.oct2019 #AR(1), efficient
AMIM.dec2019 <- (mim[75,9] - 0.7605)/(1 - 0.7605); AMIM.dec2019 #AR(2)
AMIM.may2020 <- (mim[80,9] - 0.7605)/(1 - 0.7605); AMIM.may2020 #AR(2)
AMIM.jun2020 <- (mim[81,9] - 0.7605)/(1 - 0.7605); AMIM.jun2020 #AR(2)
AMIM.aug2020 <- (mim[83,9] - 0.6619)/(1 - 0.6619); AMIM.aug2020 #AR(1), efficient
AMIM.sep2020 <- (mim[84,9] - 0.9244)/(1 - 0.9244); AMIM.sep2020 #AR(11)
AMIM.nov2020 <- (mim[86,9] - 0.7605)/(1 - 0.7605); AMIM.nov2020 #AR(2), efficient 
AMIM.mar2021 <- (mim[90,9] - 0.8643)/(1 - 0.8643); AMIM.mar2021 #AR(5)

AMIM.monthly <- cbind(0, AMIM.nov2013, 0, 0, AMIM.feb2014, 0, 0, 0, 0, AMIM.jul2014, 0, 0, 0, AMIM.nov2014, 
                     0, 0, AMIM.feb2015, 0, 0, 0, 0, 0, 0, 0, 0, AMIM.nov2015, AMIM.dec2015, 0, 
                     AMIM.feb2016, 0, 0, AMIM.may2016, 0, AMIM.jul2016, 0, 0, AMIM.oct2016, 0, 0,
                     AMIM.jan2017, 0, 0, 0, 0, 0, 0, 0, AMIM.sep2017, 0, AMIM.nov2017, 0, 0, 0, AMIM.mar2018, 0, 
                     AMIM.may2018, AMIM.jun2018, 0, 0, AMIM.sep2018, AMIM.oct2018, 0, 0, 0, 0,
                     AMIM.mar2019, 0, 0, AMIM.jun2019, AMIM.jul2019, AMIM.aug2019, AMIM.sep2019, AMIM.oct2019, 
                     0, AMIM.dec2019, 0, 0, 0, 0, AMIM.may2020, AMIM.jun2020, 0, AMIM.aug2020, AMIM.sep2020, 0, 
                     AMIM.nov2020, 0, 0, 0, AMIM.mar2021, 0) #In total 24 months out of 91 are inefficient. 
plot(y = AMIM.monthly, x = td.month, ylab = "AMIM_t", xlab = "Years", main = "Monthly AMIM",
     type = "l"); abline(h = 0, lty = 2, col = "blue")
