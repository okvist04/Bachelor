Bitcoin_df<- read.table("BitcoinDataCoinDesk.csv", header = TRUE, sep = ",", 
                        stringsAsFactors = FALSE)

dates <- as.Date(Bitcoin_df$Date)

td <- seq(as.Date("2013-10-01"), as.Date("2021-04-19"), "days")

closing_price <- zoo(x = Bitcoin_df$Closing.Price..USD., order.by = td)

return <- diff(closing_price) / closing_price[1:2757]

# 256 different ARMA-GARCH models defined, to find the one with smallest BIC 

#ARMA(1-4), GARCH(1-3)
arma11.garch11 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                             mean.model = list(armaOrder = c(1,1), include.mean = TRUE),
                             distribution.model = "norm")
arma11.garch11.fit <- ugarchfit(spec = arma11.garch11, data = return, out.sample = 100,
                                solver = "hybrid")
b.11.11 <- infocriteria(arma11.garch11.fit) #BIC = -2.2232

arma11.garch12 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,2)),
                             mean.model = list(armaOrder = c(1,1), include.mean = TRUE),
                             distribution.model = "norm")
arma11.garch12.fit <- ugarchfit(spec = arma11.garch12, data = return, out.sample = 100,
                                solver = "hybrid")
b.11.12 <- infocriteria(arma11.garch12.fit) #BIC = -2.2068

arma11.garch13 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,3)),
                             mean.model = list(armaOrder = c(1,1), include.mean = TRUE),
                             distribution.model = "norm")
arma11.garch13.fit <- ugarchfit(spec = arma11.garch13, data = return, out.sample = 100,
                                solver = "hybrid")
b.11.13 <- infocriteria(arma11.garch13.fit) #BIC = -2.2169

arma11.garch14 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,4)),
                             mean.model = list(armaOrder = c(1,1), include.mean = TRUE),
                             distribution.model = "norm")
arma11.garch14.fit <- ugarchfit(spec = arma11.garch14, data = return, out.sample = 100,
                                solver = "hybrid")
b.11.14 <- infocriteria(arma11.garch14.fit) #BIC = -2.1956

arma11.garch21 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,1)),
                             mean.model = list(armaOrder = c(1,1), include.mean = TRUE),
                             distribution.model = "norm")
arma11.garch21.fit <- ugarchfit(spec = arma11.garch21, data = return, out.sample = 100,
                                solver = "hybrid")
b.11.21 <- infocriteria(arma11.garch21.fit) #BIC = -2.1922

arma11.garch22 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,2)),
                             mean.model = list(armaOrder = c(1,1), include.mean = TRUE),
                             distribution.model = "norm")
arma11.garch22.fit <- ugarchfit(spec = arma11.garch22, data = return, out.sample = 100,
                                solver = "hybrid")
b.11.22 <-infocriteria(arma11.garch22.fit) #BIC = -2.1745

arma11.garch23 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,3)),
                             mean.model = list(armaOrder = c(1,1), include.mean = TRUE),
                             distribution.model = "norm")
arma11.garch23.fit <- ugarchfit(spec = arma11.garch23, data = return, out.sample = 100,
                                solver = "hybrid")
b.11.23 <- infocriteria(arma11.garch23.fit) #BIC = -2.2021

arma11.garch24 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,4)),
                             mean.model = list(armaOrder = c(1,1), include.mean = TRUE),
                             distribution.model = "norm")
arma11.garch24.fit <- ugarchfit(spec = arma11.garch24, data = return, out.sample = 100,
                                solver = "hybrid")
b.11.24 <- infocriteria(arma11.garch24.fit) #BIC = -2.1634

arma11.garch31 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,1)),
                             mean.model = list(armaOrder = c(1,1), include.mean = TRUE),
                             distribution.model = "norm")
arma11.garch31.fit <- ugarchfit(spec = arma11.garch31, data = return, out.sample = 100,
                                solver = "hybrid")
b.11.31 <- infocriteria(arma11.garch31.fit) #BIC = -2.1783

arma11.garch32 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,2)),
                             mean.model = list(armaOrder = c(1,1), include.mean = TRUE),
                             distribution.model = "norm")
arma11.garch32.fit <- ugarchfit(spec = arma11.garch32, data = return, out.sample = 100,
                                solver = "hybrid")
b.11.32 <- infocriteria(arma11.garch32.fit) #BIC = -2.1593

arma11.garch33 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,3)),
                             mean.model = list(armaOrder = c(1,1), include.mean = TRUE),
                             distribution.model = "norm")
arma11.garch33.fit <- ugarchfit(spec = arma11.garch33, data = return, out.sample = 100,
                                solver = "hybrid")
b.11.33 <- infocriteria(arma11.garch33.fit) #BIC = -2.1697

arma11.garch34 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,4)),
                             mean.model = list(armaOrder = c(1,1), include.mean = TRUE),
                             distribution.model = "norm")
arma11.garch34.fit <- ugarchfit(spec = arma11.garch34, data = return, out.sample = 100,
                                solver = "hybrid")
b.11.34 <-infocriteria(arma11.garch34.fit) #BIC = -2.131

arma11.garch41 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,1)),
                             mean.model = list(armaOrder = c(1,1), include.mean = TRUE),
                             distribution.model = "norm")
arma11.garch41.fit <- ugarchfit(spec = arma11.garch41, data = return, out.sample = 100,
                                solver = "hybrid")
b.11.41 <-infocriteria(arma11.garch41.fit) #BIC = -2.1444

arma11.garch42 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,2)),
                             mean.model = list(armaOrder = c(1,1), include.mean = TRUE),
                             distribution.model = "norm")
arma11.garch42.fit <- ugarchfit(spec = arma11.garch42, data = return, out.sample = 100,
                                solver = "hybrid")
b.11.42 <-infocriteria(arma11.garch42.fit) #BIC = -2.1287

arma11.garch43 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,3)),
                             mean.model = list(armaOrder = c(1,1), include.mean = TRUE),
                             distribution.model = "norm")
arma11.garch43.fit <- ugarchfit(spec = arma11.garch43, data = return, out.sample = 100,
                                solver = "hybrid")
b.11.43 <-infocriteria(arma11.garch43.fit) #BIC = -2.1416

arma11.garch44 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,4)),
                             mean.model = list(armaOrder = c(1,1), include.mean = TRUE),
                             distribution.model = "norm")
arma11.garch44.fit <- ugarchfit(spec = arma11.garch44, data = return, out.sample = 100,
                                solver = "hybrid")
b.11.44 <-infocriteria(arma11.garch44.fit) #BIC = -2.0986
####################################
arma12.garch11 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                             mean.model = list(armaOrder = c(1,2), include.mean = TRUE),
                             distribution.model = "norm")
arma12.garch11.fit <- ugarchfit(spec = arma12.garch11, data = return, out.sample = 100,
                                solver = "hybrid")
b.12.11 <-infocriteria(arma12.garch11.fit) #BIC = -2.1870

arma12.garch12 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,2)),
                             mean.model = list(armaOrder = c(1,2), include.mean = TRUE),
                             distribution.model = "norm")
arma12.garch12.fit <- ugarchfit(spec = arma12.garch12, data = return, out.sample = 100,
                                solver = "hybrid")
b.12.12 <-infocriteria(arma12.garch12.fit) #BIC = -2.1692

arma12.garch13 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,3)),
                             mean.model = list(armaOrder = c(1,2), include.mean = TRUE),
                             distribution.model = "norm")
arma12.garch13.fit <- ugarchfit(spec = arma12.garch13, data = return, out.sample = 100,
                                solver = "hybrid")
b.12.13 <-infocriteria(arma12.garch13.fit) #BIC = -2.1935

arma12.garch14 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,4)),
                             mean.model = list(armaOrder = c(1,2), include.mean = TRUE),
                             distribution.model = "norm")
arma12.garch14.fit <- ugarchfit(spec = arma12.garch14, data = return, out.sample = 100,
                                solver = "hybrid")
b.12.14 <-infocriteria(arma12.garch14.fit) #BIC = -2.1576

arma12.garch21 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,1)),
                             mean.model = list(armaOrder = c(1,2), include.mean = TRUE),
                             distribution.model = "norm")
arma12.garch21.fit <- ugarchfit(spec = arma12.garch21, data = return, out.sample = 100,
                                solver = "hybrid")
b.12.21 <-infocriteria(arma12.garch21.fit) #BIC = -2.155

arma12.garch22 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,2)),
                             mean.model = list(armaOrder = c(1,2), include.mean = TRUE),
                             distribution.model = "norm")
arma12.garch22.fit <- ugarchfit(spec = arma12.garch22, data = return, out.sample = 100,
                                solver = "hybrid")
b.12.22 <-infocriteria(arma12.garch22.fit) #BIC = -2.1368

arma12.garch23 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,3)),
                             mean.model = list(armaOrder = c(1,2), include.mean = TRUE),
                             distribution.model = "norm")
arma12.garch23.fit <- ugarchfit(spec = arma12.garch23, data = return, out.sample = 100,
                                solver = "hybrid")
b.12.23 <-infocriteria(arma12.garch23.fit) #BIC = -2.1769

arma12.garch24 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,4)),
                             mean.model = list(armaOrder = c(1,2), include.mean = TRUE),
                             distribution.model = "norm")
arma12.garch24.fit <- ugarchfit(spec = arma12.garch24, data = return, out.sample = 100,
                                solver = "hybrid")
b.12.24 <-infocriteria(arma12.garch24.fit) #BIC = -2.1258

arma12.garch31 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,1)),
                             mean.model = list(armaOrder = c(1,2), include.mean = TRUE),
                             distribution.model = "norm")
arma12.garch31.fit <- ugarchfit(spec = arma12.garch31, data = return, out.sample = 100,
                                solver = "hybrid")
b.12.31 <-infocriteria(arma12.garch31.fit) #BIC = -2.1390

arma12.garch32 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,2)),
                             mean.model = list(armaOrder = c(1,2), include.mean = TRUE),
                             distribution.model = "norm")
arma12.garch32.fit <- ugarchfit(spec = arma12.garch32, data = return, out.sample = 100,
                                solver = "hybrid")
b.12.32 <-infocriteria(arma12.garch32.fit) #BIC = -2.1204

arma12.garch33 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,3)),
                             mean.model = list(armaOrder = c(1,2), include.mean = TRUE),
                             distribution.model = "norm")
arma12.garch33.fit <- ugarchfit(spec = arma12.garch33, data = return, out.sample = 100,
                                solver = "hybrid")
b.12.33 <-infocriteria(arma12.garch33.fit) #BIC = -2.1446

arma12.garch34 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,4)),
                             mean.model = list(armaOrder = c(1,2), include.mean = TRUE),
                             distribution.model = "norm")
arma12.garch34.fit <- ugarchfit(spec = arma12.garch34, data = return, out.sample = 100,
                                solver = "hybrid")
b.12.34 <-infocriteria(arma12.garch34.fit) #BIC = -2.0934

arma12.garch41 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,1)),
                             mean.model = list(armaOrder = c(1,2), include.mean = TRUE),
                             distribution.model = "norm")
arma12.garch41.fit <- ugarchfit(spec = arma12.garch41, data = return, out.sample = 100,
                                solver = "hybrid")
b.12.41 <-infocriteria(arma12.garch41.fit) #BIC = -2.1093

arma12.garch42 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,2)),
                             mean.model = list(armaOrder = c(1,2), include.mean = TRUE),
                             distribution.model = "norm")
arma12.garch42.fit <- ugarchfit(spec = arma12.garch42, data = return, out.sample = 100,
                                solver = "hybrid")
b.12.42 <-infocriteria(arma12.garch42.fit) #BIC = -2.0902

arma12.garch43 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,3)),
                             mean.model = list(armaOrder = c(1,2), include.mean = TRUE),
                             distribution.model = "norm")
arma12.garch43.fit <- ugarchfit(spec = arma12.garch43, data = return, out.sample = 100,
                                solver = "hybrid")
b.12.43 <-infocriteria(arma12.garch43.fit) #BIC = -2.1150

arma12.garch44 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,4)),
                             mean.model = list(armaOrder = c(1,2), include.mean = TRUE),
                             distribution.model = "norm")
arma12.garch44.fit <- ugarchfit(spec = arma12.garch44, data = return, out.sample = 100,
                                solver = "hybrid")
b.12.44 <-infocriteria(arma12.garch44.fit) #BIC = -2.0610
############################
arma13.garch11 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                             mean.model = list(armaOrder = c(1,3), include.mean = TRUE),
                             distribution.model = "norm")
arma13.garch11.fit <- ugarchfit(spec = arma13.garch11, data = return, out.sample = 100,
                                solver = "hybrid")
b.13.11 <- infocriteria(arma13.garch11.fit) #BIC = -2.1751

arma13.garch12 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,2)),
                             mean.model = list(armaOrder = c(1,3), include.mean = TRUE),
                             distribution.model = "norm")
arma13.garch12.fit <- ugarchfit(spec = arma13.garch12, data = return, out.sample = 100,
                                solver = "hybrid")
b.13.12 <- infocriteria(arma13.garch12.fit) #BIC = -2.1564

arma13.garch13 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,3)),
                             mean.model = list(armaOrder = c(1,3), include.mean = TRUE),
                             distribution.model = "norm")
arma13.garch13.fit <- ugarchfit(spec = arma13.garch13, data = return, out.sample = 100,
                                solver = "hybrid")
b.13.13 <- infocriteria(arma13.garch13.fit) #BIC = -2.1643

arma13.garch14 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,4)),
                             mean.model = list(armaOrder = c(1,3), include.mean = TRUE),
                             distribution.model = "norm")
arma13.garch14.fit <- ugarchfit(spec = arma13.garch14, data = return, out.sample = 100,
                                solver = "hybrid")
b.13.14 <- infocriteria(arma13.garch14.fit) #BIC = -2.1318

arma13.garch21 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,1)),
                             mean.model = list(armaOrder = c(1,3), include.mean = TRUE),
                             distribution.model = "norm")
arma13.garch21.fit <- ugarchfit(spec = arma13.garch21, data = return, out.sample = 100,
                                solver = "hybrid")
b.13.21 <- infocriteria(arma13.garch21.fit) #BIC = -2.1427

arma13.garch22 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,2)),
                             mean.model = list(armaOrder = c(1,3), include.mean = TRUE),
                             distribution.model = "norm")
arma13.garch22.fit <- ugarchfit(spec = arma13.garch22, data = return, out.sample = 100,
                                solver = "hybrid")
b.13.22 <- infocriteria(arma13.garch22.fit) #BIC = -2.1240

arma13.garch23 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,3)),
                             mean.model = list(armaOrder = c(1,3), include.mean = TRUE),
                             distribution.model = "norm")
arma13.garch23.fit <- ugarchfit(spec = arma13.garch23, data = return, out.sample = 100,
                                solver = "hybrid")
b.13.23 <- infocriteria(arma13.garch23.fit) #BIC = -2.1492

arma13.garch24 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,4)),
                             mean.model = list(armaOrder = c(1,3), include.mean = TRUE),
                             distribution.model = "norm")
arma13.garch24.fit <- ugarchfit(spec = arma13.garch24, data = return, out.sample = 100,
                                solver = "hybrid")
b.13.24 <- infocriteria(arma13.garch24.fit) #BIC = -2.1032

arma13.garch31 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,1)),
                             mean.model = list(armaOrder = c(1,3), include.mean = TRUE),
                             distribution.model = "norm")
arma13.garch31.fit <- ugarchfit(spec = arma13.garch31, data = return, out.sample = 100,
                                solver = "hybrid")
b.13.31 <- infocriteria(arma13.garch31.fit) #BIC = -2.1104

arma13.garch32 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,2)),
                             mean.model = list(armaOrder = c(1,3), include.mean = TRUE),
                             distribution.model = "norm")
arma13.garch32.fit <- ugarchfit(spec = arma13.garch32, data = return, out.sample = 100,
                                solver = "hybrid")
b.13.32 <- infocriteria(arma13.garch32.fit) #BIC = -2.0917

arma13.garch33 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,3)),
                             mean.model = list(armaOrder = c(1,3), include.mean = TRUE),
                             distribution.model = "norm")
arma13.garch33.fit <- ugarchfit(spec = arma13.garch33, data = return, out.sample = 100,
                                solver = "hybrid")
b.13.33 <- infocriteria(arma13.garch33.fit) #BIC = -2.1168

arma13.garch34 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,4)),
                             mean.model = list(armaOrder = c(1,3), include.mean = TRUE),
                             distribution.model = "norm")
arma13.garch34.fit <- ugarchfit(spec = arma13.garch34, data = return, out.sample = 100,
                                solver = "hybrid")
b.13.34 <- infocriteria(arma13.garch34.fit) #BIC = -2.0708

arma13.garch41 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,1)),
                             mean.model = list(armaOrder = c(1,3), include.mean = TRUE),
                             distribution.model = "norm")
arma13.garch41.fit <- ugarchfit(spec = arma13.garch41, data = return, out.sample = 100,
                                solver = "hybrid")
b.13.41 <- infocriteria(arma13.garch41.fit) #BIC = -2.0809

arma13.garch42 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,2)),
                             mean.model = list(armaOrder = c(1,3), include.mean = TRUE),
                             distribution.model = "norm")
arma13.garch42.fit <- ugarchfit(spec = arma13.garch42, data = return, out.sample = 100,
                                solver = "hybrid")
b.13.42 <- infocriteria(arma13.garch42.fit) #BIC = -2.0618

arma13.garch43 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,3)),
                             mean.model = list(armaOrder = c(1,3), include.mean = TRUE),
                             distribution.model = "norm")
arma13.garch43.fit <- ugarchfit(spec = arma13.garch43, data = return, out.sample = 100,
                                solver = "hybrid")
b.13.43 <- infocriteria(arma13.garch43.fit) #BIC = -2.0878

arma13.garch44 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,4)),
                             mean.model = list(armaOrder = c(1,3), include.mean = TRUE),
                             distribution.model = "norm")
arma13.garch44.fit <- ugarchfit(spec = arma13.garch44, data = return, out.sample = 100,
                                solver = "hybrid")
b.13.44 <- infocriteria(arma13.garch44.fit) #BIC = -2.0385
############################
arma14.garch11 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                             mean.model = list(armaOrder = c(1,4), include.mean = TRUE),
                             distribution.model = "norm")
arma14.garch11.fit <- ugarchfit(spec = arma14.garch11, data = return, out.sample = 100,
                                solver = "hybrid")
b.14.11 <- infocriteria(arma14.garch11.fit) #BIC = -2.1693

arma14.garch12 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,2)),
                             mean.model = list(armaOrder = c(1,4), include.mean = TRUE),
                             distribution.model = "norm")
arma14.garch12.fit <- ugarchfit(spec = arma14.garch12, data = return, out.sample = 100,
                                solver = "hybrid")
b.14.12 <- infocriteria(arma14.garch12.fit) #BIC = -2.1633

arma14.garch13 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,3)),
                             mean.model = list(armaOrder = c(1,4), include.mean = TRUE),
                             distribution.model = "norm")
arma14.garch13.fit <- ugarchfit(spec = arma14.garch13, data = return, out.sample = 100,
                                solver = "hybrid")
b.14.13 <- infocriteria(arma14.garch13.fit) #BIC = -2.1862

arma14.garch14 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,4)),
                             mean.model = list(armaOrder = c(1,4), include.mean = TRUE),
                             distribution.model = "norm")
arma14.garch14.fit <- ugarchfit(spec = arma14.garch14, data = return, out.sample = 100,
                                solver = "hybrid")
b.14.14 <- infocriteria(arma14.garch14.fit) #BIC = -2.1528

arma14.garch21 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,1)),
                             mean.model = list(armaOrder = c(1,4), include.mean = TRUE),
                             distribution.model = "norm")
arma14.garch21.fit <- ugarchfit(spec = arma14.garch21, data = return, out.sample = 100,
                                solver = "hybrid")
b.14.21 <- infocriteria(arma14.garch21.fit) #BIC = -2.1369

arma14.garch22 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,2)),
                             mean.model = list(armaOrder = c(1,4), include.mean = TRUE),
                             distribution.model = "norm")
arma14.garch22.fit <- ugarchfit(spec = arma14.garch22, data = return, out.sample = 100,
                                solver = "hybrid")
b.14.22 <- infocriteria(arma14.garch22.fit) #BIC = -2.1309

arma14.garch23 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,3)),
                             mean.model = list(armaOrder = c(1,4), include.mean = TRUE),
                             distribution.model = "norm")
arma14.garch23.fit <- ugarchfit(spec = arma14.garch23, data = return, out.sample = 100,
                                solver = "hybrid")
b.14.23 <- infocriteria(arma14.garch23.fit) #BIC = -2.1701

arma14.garch24 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,4)),
                             mean.model = list(armaOrder = c(1,4), include.mean = TRUE),
                             distribution.model = "norm")
arma14.garch24.fit <- ugarchfit(spec = arma14.garch24, data = return, out.sample = 100,
                                solver = "hybrid")
b.14.24 <- infocriteria(arma14.garch24.fit) #BIC = -2.1204

arma14.garch31 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,1)),
                             mean.model = list(armaOrder = c(1,4), include.mean = TRUE),
                             distribution.model = "norm")
arma14.garch31.fit <- ugarchfit(spec = arma14.garch31, data = return, out.sample = 100,
                                solver = "hybrid")
b.14.31 <- infocriteria(arma14.garch31.fit) #BIC = -2.1045

arma14.garch32 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,2)),
                             mean.model = list(armaOrder = c(1,4), include.mean = TRUE),
                             distribution.model = "norm")
arma14.garch32.fit <- ugarchfit(spec = arma14.garch32, data = return, out.sample = 100,
                                solver = "hybrid")
b.14.32 <- infocriteria(arma14.garch32.fit) #BIC = -2.0986

arma14.garch33 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,3)),
                             mean.model = list(armaOrder = c(1,4), include.mean = TRUE),
                             distribution.model = "norm")
arma14.garch33.fit <- ugarchfit(spec = arma14.garch33, data = return, out.sample = 100,
                                solver = "hybrid")
b.14.33 <- infocriteria(arma14.garch33.fit) #BIC = -2.1377

arma14.garch34 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,4)),
                             mean.model = list(armaOrder = c(1,4), include.mean = TRUE),
                             distribution.model = "norm")
arma14.garch34.fit <- ugarchfit(spec = arma14.garch34, data = return, out.sample = 100,
                                solver = "hybrid")
b.14.34 <- infocriteria(arma14.garch34.fit) #BIC = -2.1053

arma14.garch41 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,1)),
                             mean.model = list(armaOrder = c(1,4), include.mean = TRUE),
                             distribution.model = "norm")
arma14.garch41.fit <- ugarchfit(spec = arma14.garch41, data = return, out.sample = 100,
                                solver = "hybrid")
b.14.41 <- infocriteria(arma14.garch41.fit) #BIC = -2.0722

arma14.garch42 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,2)),
                             mean.model = list(armaOrder = c(1,4), include.mean = TRUE),
                             distribution.model = "norm")
arma14.garch42.fit <- ugarchfit(spec = arma14.garch42, data = return, out.sample = 100,
                                solver = "hybrid")
b.14.42 <- infocriteria(arma14.garch42.fit) #BIC = -2.0662

arma14.garch43 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,3)),
                             mean.model = list(armaOrder = c(1,4), include.mean = TRUE),
                             distribution.model = "norm")
arma14.garch43.fit <- ugarchfit(spec = arma14.garch43, data = return, out.sample = 100,
                                solver = "hybrid")
b.14.43 <- infocriteria(arma14.garch43.fit) #BIC = -2.1053

arma14.garch44 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,4)),
                             mean.model = list(armaOrder = c(1,4), include.mean = TRUE),
                             distribution.model = "norm")
arma14.garch44.fit <- ugarchfit(spec = arma14.garch44, data = return, out.sample = 100,
                                solver = "hybrid")
b.14.44 <- infocriteria(arma14.garch44.fit) #BIC = -2.0557
##########################
arma21.garch11 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                             mean.model = list(armaOrder = c(2,1), include.mean = TRUE),
                             distribution.model = "norm")
arma21.garch11.fit <- ugarchfit(spec = arma21.garch11, data = return, out.sample = 100,
                                solver = "hybrid")
b.21.11 <- infocriteria(arma21.garch11.fit) #BIC = -2.1881

arma21.garch12 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,2)),
                             mean.model = list(armaOrder = c(2,1), include.mean = TRUE),
                             distribution.model = "norm")
arma21.garch12.fit <- ugarchfit(spec = arma21.garch12, data = return, out.sample = 100,
                                solver = "hybrid")
b.21.12 <- infocriteria(arma21.garch12.fit) #BIC = -2.1703

arma21.garch13 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,3)),
                             mean.model = list(armaOrder = c(2,1), include.mean = TRUE),
                             distribution.model = "norm")
arma21.garch13.fit <- ugarchfit(spec = arma21.garch13, data = return, out.sample = 100,
                                solver = "hybrid")
b.21.13 <- infocriteria(arma21.garch13.fit) #BIC = -2.1956

arma21.garch14 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,4)),
                             mean.model = list(armaOrder = c(2,1), include.mean = TRUE),
                             distribution.model = "norm")
arma21.garch14.fit <- ugarchfit(spec = arma21.garch14, data = return, out.sample = 100,
                                solver = "hybrid")
b.21.14 <- infocriteria(arma21.garch14.fit) #BIC = -2.1593

arma21.garch21 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,1)),
                             mean.model = list(armaOrder = c(2,1), include.mean = TRUE),
                             distribution.model = "norm")
arma21.garch21.fit <- ugarchfit(spec = arma21.garch21, data = return, out.sample = 100,
                                solver = "hybrid")
b.21.21 <- infocriteria(arma21.garch21.fit) #BIC = -2.1557

arma21.garch22 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,2)),
                             mean.model = list(armaOrder = c(2,1), include.mean = TRUE),
                             distribution.model = "norm")
arma21.garch22.fit <- ugarchfit(spec = arma21.garch22, data = return, out.sample = 100,
                                solver = "hybrid")
b.21.22 <- infocriteria(arma21.garch22.fit) #BIC = -2.138

arma21.garch23 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,3)),
                             mean.model = list(armaOrder = c(2,1), include.mean = TRUE),
                             distribution.model = "norm")
arma21.garch23.fit <- ugarchfit(spec = arma21.garch23, data = return, out.sample = 100,
                                solver = "hybrid")
b.21.23 <- infocriteria(arma21.garch23.fit) #BIC = -2.1789

arma21.garch24 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,4)),
                             mean.model = list(armaOrder = c(2,1), include.mean = TRUE),
                             distribution.model = "norm")
arma21.garch24.fit <- ugarchfit(spec = arma21.garch24, data = return, out.sample = 100,
                                solver = "hybrid")
b.21.24 <- infocriteria(arma21.garch24.fit) #BIC = -2.1274

arma21.garch31 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,1)),
                             mean.model = list(armaOrder = c(2,1), include.mean = TRUE),
                             distribution.model = "norm")
arma21.garch31.fit <- ugarchfit(spec = arma21.garch31, data = return, out.sample = 100,
                                solver = "hybrid")
b.21.31 <- infocriteria(arma21.garch31.fit) #BIC = -2.1398

arma21.garch32 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,2)),
                             mean.model = list(armaOrder = c(2,1), include.mean = TRUE),
                             distribution.model = "norm")
arma21.garch32.fit <- ugarchfit(spec = arma21.garch32, data = return, out.sample = 100,
                                solver = "hybrid")
b.21.32 <- infocriteria(arma21.garch32.fit) #BIC = -2.1213

arma21.garch33 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,3)),
                             mean.model = list(armaOrder = c(2,1), include.mean = TRUE),
                             distribution.model = "norm")
arma21.garch33.fit <- ugarchfit(spec = arma21.garch33, data = return, out.sample = 100,
                                solver = "hybrid")
b.21.33 <- infocriteria(arma21.garch33.fit) #BIC = -2.1466

arma21.garch34 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,4)),
                             mean.model = list(armaOrder = c(2,1), include.mean = TRUE),
                             distribution.model = "norm")
arma21.garch34.fit <- ugarchfit(spec = arma21.garch34, data = return, out.sample = 100,
                                solver = "hybrid")
b.21.34 <- infocriteria(arma21.garch34.fit) #BIC = -2.0950

arma21.garch41 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,1)),
                             mean.model = list(armaOrder = c(2,1), include.mean = TRUE),
                             distribution.model = "norm")
arma21.garch41.fit <- ugarchfit(spec = arma21.garch41, data = return, out.sample = 100,
                                solver = "hybrid")
b.21.41 <- infocriteria(arma21.garch41.fit) #BIC = -2.1103

arma21.garch42 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,2)),
                             mean.model = list(armaOrder = c(2,1), include.mean = TRUE),
                             distribution.model = "norm")
arma21.garch42.fit <- ugarchfit(spec = arma21.garch42, data = return, out.sample = 100,
                                solver = "hybrid")
b.21.42 <- infocriteria(arma21.garch42.fit) #BIC = -2.0913

arma21.garch43 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,3)),
                             mean.model = list(armaOrder = c(2,1), include.mean = TRUE),
                             distribution.model = "norm")
arma21.garch43.fit <- ugarchfit(spec = arma21.garch43, data = return, out.sample = 100,
                                solver = "hybrid")
b.21.43 <- infocriteria(arma21.garch43.fit) #BIC = -2.1171

arma21.garch44 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,4)),
                             mean.model = list(armaOrder = c(2,1), include.mean = TRUE),
                             distribution.model = "norm")
arma21.garch44.fit <- ugarchfit(spec = arma21.garch44, data = return, out.sample = 100,
                                solver = "hybrid")
b.21.44 <- infocriteria(arma21.garch44.fit) #BIC = -2.0627
#############################
arma22.garch11 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                             mean.model = list(armaOrder = c(2,2), include.mean = TRUE),
                             distribution.model = "norm")
arma22.garch11.fit <- ugarchfit(spec = arma22.garch11, data = return, out.sample = 100,
                                solver = "hybrid")
b.22.11 <- infocriteria(arma22.garch11.fit) #BIC = -2.1881

arma22.garch12 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,2)),
                             mean.model = list(armaOrder = c(2,2), include.mean = TRUE),
                             distribution.model = "norm")
arma22.garch12.fit <- ugarchfit(spec = arma22.garch12, data = return, out.sample = 100,
                                solver = "hybrid")
b.22.12 <- infocriteria(arma22.garch12.fit) #BIC = -2.1703

arma22.garch13 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,3)),
                             mean.model = list(armaOrder = c(2,2), include.mean = TRUE),
                             distribution.model = "norm")
arma22.garch13.fit <- ugarchfit(spec = arma22.garch13, data = return, out.sample = 100,
                                solver = "hybrid")
b.22.13 <- infocriteria(arma22.garch13.fit) #BIC = -2.1956

arma22.garch14 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,4)),
                             mean.model = list(armaOrder = c(2,2), include.mean = TRUE),
                             distribution.model = "norm")
arma22.garch14.fit <- ugarchfit(spec = arma22.garch14, data = return, out.sample = 100,
                                solver = "hybrid")
b.22.14 <- infocriteria(arma22.garch14.fit) #BIC = -2.1593

arma22.garch21 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,1)),
                             mean.model = list(armaOrder = c(2,2), include.mean = TRUE),
                             distribution.model = "norm")
arma22.garch21.fit <- ugarchfit(spec = arma22.garch21, data = return, out.sample = 100,
                                solver = "hybrid")
b.22.21 <- infocriteria(arma22.garch21.fit) #BIC = -2.1557

arma22.garch22 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,2)),
                             mean.model = list(armaOrder = c(2,2), include.mean = TRUE),
                             distribution.model = "norm")
arma22.garch22.fit <- ugarchfit(spec = arma22.garch22, data = return, out.sample = 100,
                                solver = "hybrid")
b.22.22 <- infocriteria(arma22.garch22.fit) #BIC = -2.138

arma22.garch23 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,3)),
                             mean.model = list(armaOrder = c(2,2), include.mean = TRUE),
                             distribution.model = "norm")
arma22.garch23.fit <- ugarchfit(spec = arma22.garch23, data = return, out.sample = 100,
                                solver = "hybrid")
b.22.23 <- infocriteria(arma22.garch23.fit) #BIC = -2.1789

arma22.garch24 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,4)),
                             mean.model = list(armaOrder = c(2,2), include.mean = TRUE),
                             distribution.model = "norm")
arma22.garch24.fit <- ugarchfit(spec = arma22.garch24, data = return, out.sample = 100,
                                solver = "hybrid")
b.22.24 <- infocriteria(arma22.garch24.fit) #BIC = -2.1274

arma22.garch31 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,1)),
                             mean.model = list(armaOrder = c(2,2), include.mean = TRUE),
                             distribution.model = "norm")
arma22.garch31.fit <- ugarchfit(spec = arma22.garch31, data = return, out.sample = 100,
                                solver = "hybrid")
b.22.31 <- infocriteria(arma22.garch31.fit) #BIC = -2.1398

arma22.garch32 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,2)),
                             mean.model = list(armaOrder = c(2,2), include.mean = TRUE),
                             distribution.model = "norm")
arma22.garch32.fit <- ugarchfit(spec = arma22.garch32, data = return, out.sample = 100,
                                solver = "hybrid")
b.22.32 <- infocriteria(arma22.garch32.fit) #BIC = -2.1223

arma22.garch33 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,3)),
                             mean.model = list(armaOrder = c(2,2), include.mean = TRUE),
                             distribution.model = "norm")
arma22.garch33.fit <- ugarchfit(spec = arma22.garch33, data = return, out.sample = 100,
                                solver = "hybrid")
b.22.33 <- infocriteria(arma22.garch33.fit) #BIC = -2.1466

arma22.garch34 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,4)),
                             mean.model = list(armaOrder = c(2,2), include.mean = TRUE),
                             distribution.model = "norm")
arma22.garch34.fit <- ugarchfit(spec = arma22.garch34, data = return, out.sample = 100,
                                solver = "hybrid")
b.22.34 <- infocriteria(arma22.garch34.fit) #BIC = -2.0950

arma22.garch41 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,1)),
                             mean.model = list(armaOrder = c(2,2), include.mean = TRUE),
                             distribution.model = "norm")
arma22.garch41.fit <- ugarchfit(spec = arma22.garch41, data = return, out.sample = 100,
                                solver = "hybrid")
b.22.41 <- infocriteria(arma22.garch41.fit) #BIC = -2.1103

arma22.garch42 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,2)),
                             mean.model = list(armaOrder = c(2,2), include.mean = TRUE),
                             distribution.model = "norm")
arma22.garch42.fit <- ugarchfit(spec = arma22.garch42, data = return, out.sample = 100,
                                solver = "hybrid")
b.22.42 <- infocriteria(arma22.garch42.fit) #BIC = -2.0913

arma22.garch43 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,3)),
                             mean.model = list(armaOrder = c(2,2), include.mean = TRUE),
                             distribution.model = "norm")
arma22.garch43.fit <- ugarchfit(spec = arma22.garch43, data = return, out.sample = 100,
                                solver = "hybrid")
b.22.43 <- infocriteria(arma22.garch43.fit) #BIC = -2.1171

arma22.garch44 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,4)),
                             mean.model = list(armaOrder = c(2,2), include.mean = TRUE),
                             distribution.model = "norm")
arma22.garch44.fit <- ugarchfit(spec = arma22.garch44, data = return, out.sample = 100,
                                solver = "hybrid")
b.22.44 <- infocriteria(arma22.garch44.fit) #BIC = -2.0627
#######################################################
arma23.garch11 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                             mean.model = list(armaOrder = c(2,3), include.mean = TRUE),
                             distribution.model = "norm")
arma23.garch11.fit <- ugarchfit(spec = arma23.garch11, data = return, out.sample = 100,
                                solver = "hybrid")
b.23.11 <- infocriteria(arma23.garch11.fit) #BIC = -2.1881

arma23.garch12 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,2)),
                             mean.model = list(armaOrder = c(2,3), include.mean = TRUE),
                             distribution.model = "norm")
arma23.garch12.fit <- ugarchfit(spec = arma23.garch12, data = return, out.sample = 100,
                                solver = "hybrid")
b.23.12 <- infocriteria(arma23.garch12.fit) #BIC = -2.1703

arma23.garch13 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,3)),
                             mean.model = list(armaOrder = c(2,3), include.mean = TRUE),
                             distribution.model = "norm")
arma23.garch13.fit <- ugarchfit(spec = arma23.garch13, data = return, out.sample = 100,
                                solver = "hybrid")
b.23.13 <- infocriteria(arma23.garch13.fit) #BIC = -2.1956

arma23.garch14 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,4)),
                             mean.model = list(armaOrder = c(2,3), include.mean = TRUE),
                             distribution.model = "norm")
arma23.garch14.fit <- ugarchfit(spec = arma23.garch14, data = return, out.sample = 100,
                                solver = "hybrid")
b.23.14 <- infocriteria(arma23.garch14.fit) #BIC = -2.1593

arma23.garch21 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,1)),
                             mean.model = list(armaOrder = c(2,3), include.mean = TRUE),
                             distribution.model = "norm")
arma23.garch21.fit <- ugarchfit(spec = arma23.garch21, data = return, out.sample = 100,
                                solver = "hybrid")
b.23.21 <- infocriteria(arma23.garch21.fit) #BIC = -2.1557

arma23.garch22 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,2)),
                             mean.model = list(armaOrder = c(2,3), include.mean = TRUE),
                             distribution.model = "norm")
arma23.garch22.fit <- ugarchfit(spec = arma23.garch22, data = return, out.sample = 100,
                                solver = "hybrid")
b.23.22 <- infocriteria(arma23.garch22.fit) #BIC = -2.138

arma23.garch23 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,3)),
                             mean.model = list(armaOrder = c(2,3), include.mean = TRUE),
                             distribution.model = "norm")
arma23.garch23.fit <- ugarchfit(spec = arma23.garch23, data = return, out.sample = 100,
                                solver = "hybrid")
b.23.23 <- infocriteria(arma23.garch23.fit) #BIC = -2.1789

arma23.garch24 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,4)),
                             mean.model = list(armaOrder = c(2,3), include.mean = TRUE),
                             distribution.model = "norm")
arma23.garch24.fit <- ugarchfit(spec = arma23.garch24, data = return, out.sample = 100,
                                solver = "hybrid")
b.23.24 <- infocriteria(arma23.garch24.fit) #BIC = -2.1274

arma23.garch31 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,1)),
                             mean.model = list(armaOrder = c(2,3), include.mean = TRUE),
                             distribution.model = "norm")
arma23.garch31.fit <- ugarchfit(spec = arma23.garch31, data = return, out.sample = 100,
                                solver = "hybrid")
b.23.31 <- infocriteria(arma23.garch31.fit) #BIC = -2.1398

arma23.garch32 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,2)),
                             mean.model = list(armaOrder = c(2,3), include.mean = TRUE),
                             distribution.model = "norm")
arma23.garch32.fit <- ugarchfit(spec = arma23.garch32, data = return, out.sample = 100,
                                solver = "hybrid")
b.23.32 <- infocriteria(arma23.garch32.fit) #BIC = -2.1233

arma23.garch33 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,3)),
                             mean.model = list(armaOrder = c(2,3), include.mean = TRUE),
                             distribution.model = "norm")
arma23.garch33.fit <- ugarchfit(spec = arma23.garch33, data = return, out.sample = 100,
                                solver = "hybrid")
b.23.33 <- infocriteria(arma23.garch33.fit) #BIC = -2.1466

arma23.garch34 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,4)),
                             mean.model = list(armaOrder = c(2,3), include.mean = TRUE),
                             distribution.model = "norm")
arma23.garch34.fit <- ugarchfit(spec = arma23.garch34, data = return, out.sample = 100,
                                solver = "hybrid")
b.23.34 <- infocriteria(arma23.garch34.fit) #BIC = -2.0950

arma23.garch41 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,1)),
                             mean.model = list(armaOrder = c(2,3), include.mean = TRUE),
                             distribution.model = "norm")
arma23.garch41.fit <- ugarchfit(spec = arma23.garch41, data = return, out.sample = 100,
                                solver = "hybrid")
b.23.41 <- infocriteria(arma23.garch41.fit) #BIC = -2.1103

arma23.garch42 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,2)),
                             mean.model = list(armaOrder = c(2,3), include.mean = TRUE),
                             distribution.model = "norm")
arma23.garch42.fit <- ugarchfit(spec = arma23.garch42, data = return, out.sample = 100,
                                solver = "hybrid")
b.23.42 <- infocriteria(arma23.garch42.fit) #BIC = -2.0913

arma23.garch43 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,3)),
                             mean.model = list(armaOrder = c(2,3), include.mean = TRUE),
                             distribution.model = "norm")
arma23.garch43.fit <- ugarchfit(spec = arma23.garch43, data = return, out.sample = 100,
                                solver = "hybrid")
b.23.43 <- infocriteria(arma23.garch43.fit) #BIC = -2.1171

arma23.garch44 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,4)),
                             mean.model = list(armaOrder = c(2,3), include.mean = TRUE),
                             distribution.model = "norm")
arma23.garch44.fit <- ugarchfit(spec = arma23.garch44, data = return, out.sample = 100,
                                solver = "hybrid")
b.23.44 <- infocriteria(arma23.garch44.fit) #BIC = -2.0627
##################################
arma24.garch11 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                             mean.model = list(armaOrder = c(2,4), include.mean = TRUE),
                             distribution.model = "norm")
arma24.garch11.fit <- ugarchfit(spec = arma24.garch11, data = return, out.sample = 100,
                                solver = "hybrid")
b.24.11 <- infocriteria(arma24.garch11.fit) #BIC = -2.1881

arma24.garch12 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,2)),
                             mean.model = list(armaOrder = c(2,4), include.mean = TRUE),
                             distribution.model = "norm")
arma24.garch12.fit <- ugarchfit(spec = arma24.garch12, data = return, out.sample = 100,
                                solver = "hybrid")
b.24.12 <- infocriteria(arma24.garch12.fit) #BIC = -2.1703

arma24.garch13 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,3)),
                             mean.model = list(armaOrder = c(2,4), include.mean = TRUE),
                             distribution.model = "norm")
arma24.garch13.fit <- ugarchfit(spec = arma24.garch13, data = return, out.sample = 100,
                                solver = "hybrid")
b.24.13 <- infocriteria(arma24.garch13.fit) #BIC = -2.1956

arma24.garch14 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,4)),
                             mean.model = list(armaOrder = c(2,4), include.mean = TRUE),
                             distribution.model = "norm")
arma24.garch14.fit <- ugarchfit(spec = arma24.garch14, data = return, out.sample = 100,
                                solver = "hybrid")
b.24.14 <- infocriteria(arma24.garch14.fit) #BIC = -2.1593

arma24.garch21 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,1)),
                             mean.model = list(armaOrder = c(2,4), include.mean = TRUE),
                             distribution.model = "norm")
arma24.garch21.fit <- ugarchfit(spec = arma24.garch21, data = return, out.sample = 100,
                                solver = "hybrid")
b.24.21 <- infocriteria(arma24.garch21.fit) #BIC = -2.1557

arma24.garch22 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,2)),
                             mean.model = list(armaOrder = c(2,4), include.mean = TRUE),
                             distribution.model = "norm")
arma24.garch22.fit <- ugarchfit(spec = arma24.garch22, data = return, out.sample = 100,
                                solver = "hybrid")
b.24.22 <- infocriteria(arma24.garch22.fit) #BIC = -2.138

arma24.garch23 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,3)),
                             mean.model = list(armaOrder = c(2,4), include.mean = TRUE),
                             distribution.model = "norm")
arma24.garch23.fit <- ugarchfit(spec = arma24.garch23, data = return, out.sample = 100,
                                solver = "hybrid")
b.24.23 <- infocriteria(arma24.garch23.fit) #BIC = -2.1789

arma24.garch24 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,4)),
                             mean.model = list(armaOrder = c(2,4), include.mean = TRUE),
                             distribution.model = "norm")
arma24.garch24.fit <- ugarchfit(spec = arma24.garch24, data = return, out.sample = 100,
                                solver = "hybrid")
b.24.24 <- infocriteria(arma24.garch24.fit) #BIC = -2.1274

arma24.garch31 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,1)),
                             mean.model = list(armaOrder = c(2,4), include.mean = TRUE),
                             distribution.model = "norm")
arma24.garch31.fit <- ugarchfit(spec = arma24.garch31, data = return, out.sample = 100,
                                solver = "hybrid")
b.24.31 <- infocriteria(arma24.garch31.fit) #BIC = -2.1398

arma24.garch32 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,2)),
                             mean.model = list(armaOrder = c(2,4), include.mean = TRUE),
                             distribution.model = "norm")
arma24.garch32.fit <- ugarchfit(spec = arma24.garch32, data = return, out.sample = 100,
                                solver = "hybrid")
b.24.32 <- infocriteria(arma24.garch32.fit) #BIC = -2.1243

arma24.garch33 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,3)),
                             mean.model = list(armaOrder = c(2,4), include.mean = TRUE),
                             distribution.model = "norm")
arma24.garch33.fit <- ugarchfit(spec = arma24.garch33, data = return, out.sample = 100,
                                solver = "hybrid")
b.24.33 <- infocriteria(arma24.garch33.fit) #BIC = -2.1466

arma24.garch34 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,4)),
                             mean.model = list(armaOrder = c(2,4), include.mean = TRUE),
                             distribution.model = "norm")
arma24.garch34.fit <- ugarchfit(spec = arma24.garch34, data = return, out.sample = 100,
                                solver = "hybrid")
b.24.34 <- infocriteria(arma24.garch34.fit) #BIC = -2.0950

arma24.garch41 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,1)),
                             mean.model = list(armaOrder = c(2,4), include.mean = TRUE),
                             distribution.model = "norm")
arma24.garch41.fit <- ugarchfit(spec = arma24.garch41, data = return, out.sample = 100,
                                solver = "hybrid")
b.24.41 <- infocriteria(arma24.garch41.fit) #BIC = -2.1103

arma24.garch42 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,2)),
                             mean.model = list(armaOrder = c(2,4), include.mean = TRUE),
                             distribution.model = "norm")
arma24.garch42.fit <- ugarchfit(spec = arma24.garch42, data = return, out.sample = 100,
                                solver = "hybrid")
b.24.42 <- infocriteria(arma24.garch42.fit) #BIC = -2.0913

arma24.garch43 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,3)),
                             mean.model = list(armaOrder = c(2,4), include.mean = TRUE),
                             distribution.model = "norm")
arma24.garch43.fit <- ugarchfit(spec = arma24.garch43, data = return, out.sample = 100,
                                solver = "hybrid")
b.24.43 <- infocriteria(arma24.garch43.fit) #BIC = -2.1171

arma24.garch44 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,4)),
                             mean.model = list(armaOrder = c(2,4), include.mean = TRUE),
                             distribution.model = "norm")
arma24.garch44.fit <- ugarchfit(spec = arma24.garch44, data = return, out.sample = 100,
                                solver = "hybrid")
b.24.44 <- infocriteria(arma24.garch44.fit) #BIC = -2.0627
################################
arma31.garch11 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                             mean.model = list(armaOrder = c(3,1), include.mean = TRUE),
                             distribution.model = "norm")
arma31.garch11.fit <- ugarchfit(spec = arma31.garch11, data = return, out.sample = 100,
                                solver = "hybrid")
b.31.11 <- infocriteria(arma31.garch11.fit) #BIC = -2.1881

arma31.garch12 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,2)),
                             mean.model = list(armaOrder = c(3,1), include.mean = TRUE),
                             distribution.model = "norm")
arma31.garch12.fit <- ugarchfit(spec = arma31.garch12, data = return, out.sample = 100,
                                solver = "hybrid")
b.31.12 <- infocriteria(arma31.garch12.fit) #BIC = -2.1703

arma31.garch13 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,3)),
                             mean.model = list(armaOrder = c(3,1), include.mean = TRUE),
                             distribution.model = "norm")
arma31.garch13.fit <- ugarchfit(spec = arma31.garch13, data = return, out.sample = 100,
                                solver = "hybrid")
b.31.13 <- infocriteria(arma31.garch13.fit) #BIC = -2.1956

arma31.garch14 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,4)),
                             mean.model = list(armaOrder = c(3,1), include.mean = TRUE),
                             distribution.model = "norm")
arma31.garch14.fit <- ugarchfit(spec = arma31.garch14, data = return, out.sample = 100,
                                solver = "hybrid")
b.31.14 <- infocriteria(arma31.garch14.fit) #BIC = -2.1593

arma31.garch21 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,1)),
                             mean.model = list(armaOrder = c(3,1), include.mean = TRUE),
                             distribution.model = "norm")
arma31.garch21.fit <- ugarchfit(spec = arma31.garch21, data = return, out.sample = 100,
                                solver = "hybrid")
b.31.21 <- infocriteria(arma31.garch21.fit) #BIC = -2.1557

arma31.garch22 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,2)),
                             mean.model = list(armaOrder = c(3,1), include.mean = TRUE),
                             distribution.model = "norm")
arma31.garch22.fit <- ugarchfit(spec = arma31.garch22, data = return, out.sample = 100,
                                solver = "hybrid")
b.31.22 <- infocriteria(arma31.garch22.fit) #BIC = -2.138

arma31.garch23 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,3)),
                             mean.model = list(armaOrder = c(3,1), include.mean = TRUE),
                             distribution.model = "norm")
arma31.garch23.fit <- ugarchfit(spec = arma31.garch23, data = return, out.sample = 100,
                                solver = "hybrid")
b.31.23 <- infocriteria(arma31.garch23.fit) #BIC = -2.1789

arma31.garch24 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,4)),
                             mean.model = list(armaOrder = c(3,1), include.mean = TRUE),
                             distribution.model = "norm")
arma31.garch24.fit <- ugarchfit(spec = arma31.garch24, data = return, out.sample = 100,
                                solver = "hybrid")
b.31.24 <- infocriteria(arma31.garch24.fit) #BIC = -2.1274

arma31.garch31 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,1)),
                             mean.model = list(armaOrder = c(3,1), include.mean = TRUE),
                             distribution.model = "norm")
arma31.garch31.fit <- ugarchfit(spec = arma31.garch31, data = return, out.sample = 100,
                                solver = "hybrid")
b.31.31 <- infocriteria(arma31.garch31.fit) #BIC = -2.1398

arma31.garch32 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,2)),
                             mean.model = list(armaOrder = c(3,1), include.mean = TRUE),
                             distribution.model = "norm")
arma31.garch32.fit <- ugarchfit(spec = arma31.garch32, data = return, out.sample = 100,
                                solver = "hybrid")
b.31.32 <- infocriteria(arma31.garch32.fit) #BIC = -2.1313

arma31.garch33 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,3)),
                             mean.model = list(armaOrder = c(3,1), include.mean = TRUE),
                             distribution.model = "norm")
arma31.garch33.fit <- ugarchfit(spec = arma31.garch33, data = return, out.sample = 100,
                                solver = "hybrid")
b.31.33 <- infocriteria(arma31.garch33.fit) #BIC = -2.1466

arma31.garch34 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,4)),
                             mean.model = list(armaOrder = c(3,1), include.mean = TRUE),
                             distribution.model = "norm")
arma31.garch34.fit <- ugarchfit(spec = arma31.garch34, data = return, out.sample = 100,
                                solver = "hybrid")
b.31.34 <- infocriteria(arma31.garch34.fit) #BIC = -2.0950

arma31.garch41 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,1)),
                             mean.model = list(armaOrder = c(3,1), include.mean = TRUE),
                             distribution.model = "norm")
arma31.garch41.fit <- ugarchfit(spec = arma31.garch41, data = return, out.sample = 100,
                                solver = "hybrid")
b.31.41 <- infocriteria(arma31.garch41.fit) #BIC = -2.1103

arma31.garch42 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,2)),
                             mean.model = list(armaOrder = c(3,1), include.mean = TRUE),
                             distribution.model = "norm")
arma31.garch42.fit <- ugarchfit(spec = arma31.garch42, data = return, out.sample = 100,
                                solver = "hybrid")
b.31.42 <- infocriteria(arma31.garch42.fit) #BIC = -2.0913

arma31.garch43 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,3)),
                             mean.model = list(armaOrder = c(3,1), include.mean = TRUE),
                             distribution.model = "norm")
arma31.garch43.fit <- ugarchfit(spec = arma31.garch43, data = return, out.sample = 100,
                                solver = "hybrid")
b.31.43 <- infocriteria(arma31.garch43.fit) #BIC = -2.1171

arma31.garch44 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,4)),
                             mean.model = list(armaOrder = c(3,1), include.mean = TRUE),
                             distribution.model = "norm")
arma31.garch44.fit <- ugarchfit(spec = arma31.garch44, data = return, out.sample = 100,
                                solver = "hybrid")
b.31.44 <- infocriteria(arma31.garch44.fit) #BIC = -2.0627
#############################
arma32.garch11 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                             mean.model = list(armaOrder = c(3,2), include.mean = TRUE),
                             distribution.model = "norm")
arma32.garch11.fit <- ugarchfit(spec = arma32.garch11, data = return, out.sample = 100,
                                solver = "hybrid")
b.32.11 <- infocriteria(arma32.garch11.fit) #BIC = -2.1881

arma32.garch12 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,2)),
                             mean.model = list(armaOrder = c(3,2), include.mean = TRUE),
                             distribution.model = "norm")
arma32.garch12.fit <- ugarchfit(spec = arma32.garch12, data = return, out.sample = 100,
                                solver = "hybrid")
b.32.12 <- infocriteria(arma32.garch12.fit) #BIC = -2.1703

arma32.garch13 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,3)),
                             mean.model = list(armaOrder = c(3,2), include.mean = TRUE),
                             distribution.model = "norm")
arma32.garch13.fit <- ugarchfit(spec = arma32.garch13, data = return, out.sample = 100,
                                solver = "hybrid")
b.32.13 <- infocriteria(arma32.garch13.fit) #BIC = -2.1956

arma32.garch14 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,4)),
                             mean.model = list(armaOrder = c(3,2), include.mean = TRUE),
                             distribution.model = "norm")
arma32.garch14.fit <- ugarchfit(spec = arma32.garch14, data = return, out.sample = 100,
                                solver = "hybrid")
b.32.14 <- infocriteria(arma32.garch14.fit) #BIC = -2.1593

arma32.garch21 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,1)),
                             mean.model = list(armaOrder = c(3,2), include.mean = TRUE),
                             distribution.model = "norm")
arma32.garch21.fit <- ugarchfit(spec = arma32.garch21, data = return, out.sample = 100,
                                solver = "hybrid")
b.32.21 <- infocriteria(arma32.garch21.fit) #BIC = -2.1557

arma32.garch22 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,2)),
                             mean.model = list(armaOrder = c(3,2), include.mean = TRUE),
                             distribution.model = "norm")
arma32.garch22.fit <- ugarchfit(spec = arma32.garch22, data = return, out.sample = 100,
                                solver = "hybrid")
b.32.22 <- infocriteria(arma32.garch22.fit) #BIC = -2.138

arma32.garch23 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,3)),
                             mean.model = list(armaOrder = c(3,2), include.mean = TRUE),
                             distribution.model = "norm")
arma32.garch23.fit <- ugarchfit(spec = arma32.garch23, data = return, out.sample = 100,
                                solver = "hybrid")
b.32.23 <- infocriteria(arma32.garch23.fit) #BIC = -2.1789

arma32.garch24 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,4)),
                             mean.model = list(armaOrder = c(3,2), include.mean = TRUE),
                             distribution.model = "norm")
arma32.garch24.fit <- ugarchfit(spec = arma32.garch24, data = return, out.sample = 100,
                                solver = "hybrid")
b.32.24 <- infocriteria(arma32.garch24.fit) #BIC = -2.1274

arma32.garch31 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,1)),
                             mean.model = list(armaOrder = c(3,2), include.mean = TRUE),
                             distribution.model = "norm")
arma32.garch31.fit <- ugarchfit(spec = arma32.garch31, data = return, out.sample = 100,
                                solver = "hybrid")
b.32.31 <- infocriteria(arma32.garch31.fit) #BIC = -2.1398

arma32.garch32 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,2)),
                             mean.model = list(armaOrder = c(3,2), include.mean = TRUE),
                             distribution.model = "norm")
arma32.garch32.fit <- ugarchfit(spec = arma32.garch32, data = return, out.sample = 100,
                                solver = "hybrid")
b.32.32 <- infocriteria(arma32.garch32.fit) #BIC = -2.1323

arma32.garch33 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,3)),
                             mean.model = list(armaOrder = c(3,2), include.mean = TRUE),
                             distribution.model = "norm")
arma32.garch33.fit <- ugarchfit(spec = arma32.garch33, data = return, out.sample = 100,
                                solver = "hybrid")
b.32.33 <- infocriteria(arma32.garch33.fit) #BIC = -2.1466

arma32.garch34 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,4)),
                             mean.model = list(armaOrder = c(3,2), include.mean = TRUE),
                             distribution.model = "norm")
arma32.garch34.fit <- ugarchfit(spec = arma32.garch34, data = return, out.sample = 100,
                                solver = "hybrid")
b.32.34 <- infocriteria(arma32.garch34.fit) #BIC = -2.0950

arma32.garch41 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,1)),
                             mean.model = list(armaOrder = c(3,2), include.mean = TRUE),
                             distribution.model = "norm")
arma32.garch41.fit <- ugarchfit(spec = arma32.garch41, data = return, out.sample = 100,
                                solver = "hybrid")
b.32.41 <- infocriteria(arma32.garch41.fit) #BIC = -2.1103

arma32.garch42 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,2)),
                             mean.model = list(armaOrder = c(3,2), include.mean = TRUE),
                             distribution.model = "norm")
arma32.garch42.fit <- ugarchfit(spec = arma32.garch42, data = return, out.sample = 100,
                                solver = "hybrid")
b.32.42 <- infocriteria(arma32.garch42.fit) #BIC = -2.0913

arma32.garch43 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,3)),
                             mean.model = list(armaOrder = c(3,2), include.mean = TRUE),
                             distribution.model = "norm")
arma32.garch43.fit <- ugarchfit(spec = arma32.garch43, data = return, out.sample = 100,
                                solver = "hybrid")
b.32.43 <- infocriteria(arma32.garch43.fit) #BIC = -2.1171

arma32.garch44 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,4)),
                             mean.model = list(armaOrder = c(3,2), include.mean = TRUE),
                             distribution.model = "norm")
arma32.garch44.fit <- ugarchfit(spec = arma32.garch44, data = return, out.sample = 100,
                                solver = "hybrid")
b.32.44 <- infocriteria(arma32.garch44.fit) #BIC = -2.0627
########################################
arma33.garch11 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                             mean.model = list(armaOrder = c(3,3), include.mean = TRUE),
                             distribution.model = "norm")
arma33.garch11.fit <- ugarchfit(spec = arma33.garch11, data = return, out.sample = 100,
                                solver = "hybrid")
b.33.11 <- infocriteria(arma33.garch11.fit) #BIC = -2.1881

arma33.garch12 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,2)),
                             mean.model = list(armaOrder = c(3,3), include.mean = TRUE),
                             distribution.model = "norm")
arma33.garch12.fit <- ugarchfit(spec = arma33.garch12, data = return, out.sample = 100,
                                solver = "hybrid")
b.33.12 <- infocriteria(arma33.garch12.fit) #BIC = -2.1703

arma33.garch13 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,3)),
                             mean.model = list(armaOrder = c(3,3), include.mean = TRUE),
                             distribution.model = "norm")
arma33.garch13.fit <- ugarchfit(spec = arma33.garch13, data = return, out.sample = 100,
                                solver = "hybrid")
b.33.13 <- infocriteria(arma33.garch13.fit) #BIC = -2.1956

arma33.garch14 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,4)),
                             mean.model = list(armaOrder = c(3,3), include.mean = TRUE),
                             distribution.model = "norm")
arma33.garch14.fit <- ugarchfit(spec = arma33.garch14, data = return, out.sample = 100,
                                solver = "hybrid")
b.33.14 <- infocriteria(arma33.garch14.fit) #BIC = -2.1593

arma33.garch21 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,1)),
                             mean.model = list(armaOrder = c(3,3), include.mean = TRUE),
                             distribution.model = "norm")
arma33.garch21.fit <- ugarchfit(spec = arma33.garch21, data = return, out.sample = 100,
                                solver = "hybrid")
b.33.21 <- infocriteria(arma33.garch21.fit) #BIC = -2.1557

arma33.garch22 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,2)),
                             mean.model = list(armaOrder = c(3,3), include.mean = TRUE),
                             distribution.model = "norm")
arma33.garch22.fit <- ugarchfit(spec = arma33.garch22, data = return, out.sample = 100,
                                solver = "hybrid")
b.33.22 <- infocriteria(arma33.garch22.fit) #BIC = -2.138

arma33.garch23 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,3)),
                             mean.model = list(armaOrder = c(3,3), include.mean = TRUE),
                             distribution.model = "norm")
arma33.garch23.fit <- ugarchfit(spec = arma33.garch23, data = return, out.sample = 100,
                                solver = "hybrid")
b.33.23 <- infocriteria(arma33.garch23.fit) #BIC = -2.1789

arma33.garch24 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,4)),
                             mean.model = list(armaOrder = c(3,3), include.mean = TRUE),
                             distribution.model = "norm")
arma33.garch24.fit <- ugarchfit(spec = arma33.garch24, data = return, out.sample = 100,
                                solver = "hybrid")
b.33.24 <- infocriteria(arma33.garch24.fit) #BIC = -2.1274

arma33.garch31 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,1)),
                             mean.model = list(armaOrder = c(3,3), include.mean = TRUE),
                             distribution.model = "norm")
arma33.garch31.fit <- ugarchfit(spec = arma33.garch31, data = return, out.sample = 100,
                                solver = "hybrid")
b.33.31 <- infocriteria(arma33.garch31.fit) #BIC = -2.1398

arma33.garch32 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,2)),
                             mean.model = list(armaOrder = c(3,3), include.mean = TRUE),
                             distribution.model = "norm")
arma33.garch32.fit <- ugarchfit(spec = arma33.garch32, data = return, out.sample = 100,
                                solver = "hybrid")
b.33.32 <- infocriteria(arma33.garch32.fit) #BIC = -2.1333

arma33.garch33 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,3)),
                             mean.model = list(armaOrder = c(3,3), include.mean = TRUE),
                             distribution.model = "norm")
arma33.garch33.fit <- ugarchfit(spec = arma33.garch33, data = return, out.sample = 100,
                                solver = "hybrid")
b.33.33 <- infocriteria(arma33.garch33.fit) #BIC = -2.1466

arma33.garch34 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,4)),
                             mean.model = list(armaOrder = c(3,3), include.mean = TRUE),
                             distribution.model = "norm")
arma33.garch34.fit <- ugarchfit(spec = arma33.garch34, data = return, out.sample = 100,
                                solver = "hybrid")
b.33.34 <- infocriteria(arma33.garch34.fit) #BIC = -2.0950

arma33.garch41 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,1)),
                             mean.model = list(armaOrder = c(3,3), include.mean = TRUE),
                             distribution.model = "norm")
arma33.garch41.fit <- ugarchfit(spec = arma33.garch41, data = return, out.sample = 100,
                                solver = "hybrid")
b.33.41 <- infocriteria(arma33.garch41.fit) #BIC = -2.1103

arma33.garch42 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,2)),
                             mean.model = list(armaOrder = c(3,3), include.mean = TRUE),
                             distribution.model = "norm")
arma33.garch42.fit <- ugarchfit(spec = arma33.garch42, data = return, out.sample = 100,
                                solver = "hybrid")
b.33.42 <- infocriteria(arma33.garch42.fit) #BIC = -2.0913

arma33.garch43 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,3)),
                             mean.model = list(armaOrder = c(3,3), include.mean = TRUE),
                             distribution.model = "norm")
arma33.garch43.fit <- ugarchfit(spec = arma33.garch43, data = return, out.sample = 100,
                                solver = "hybrid")
b.33.43 <- infocriteria(arma33.garch43.fit) #BIC = -2.1171

arma33.garch44 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,4)),
                             mean.model = list(armaOrder = c(3,3), include.mean = TRUE),
                             distribution.model = "norm")
arma33.garch44.fit <- ugarchfit(spec = arma33.garch44, data = return, out.sample = 100,
                                solver = "hybrid")
b.33.44 <- infocriteria(arma33.garch44.fit) #BIC = -2.0627
##################################
arma34.garch11 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                             mean.model = list(armaOrder = c(3,4), include.mean = TRUE),
                             distribution.model = "norm")
arma34.garch11.fit <- ugarchfit(spec = arma34.garch11, data = return, out.sample = 100,
                                solver = "hybrid")
b.34.11 <- infocriteria(arma34.garch11.fit) #BIC = -2.1881

arma34.garch12 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,2)),
                             mean.model = list(armaOrder = c(3,4), include.mean = TRUE),
                             distribution.model = "norm")
arma34.garch12.fit <- ugarchfit(spec = arma34.garch12, data = return, out.sample = 100,
                                solver = "hybrid")
b.34.12 <- infocriteria(arma34.garch12.fit) #BIC = -2.1703

arma34.garch13 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,3)),
                             mean.model = list(armaOrder = c(3,4), include.mean = TRUE),
                             distribution.model = "norm")
arma34.garch13.fit <- ugarchfit(spec = arma34.garch13, data = return, out.sample = 100,
                                solver = "hybrid")
b.34.13 <- infocriteria(arma34.garch13.fit) #BIC = -2.1956

arma34.garch14 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,4)),
                             mean.model = list(armaOrder = c(3,4), include.mean = TRUE),
                             distribution.model = "norm")
arma34.garch14.fit <- ugarchfit(spec = arma34.garch14, data = return, out.sample = 100,
                                solver = "hybrid")
b.34.14 <- infocriteria(arma34.garch14.fit) #BIC = -2.1593

arma34.garch21 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,1)),
                             mean.model = list(armaOrder = c(3,4), include.mean = TRUE),
                             distribution.model = "norm")
arma34.garch21.fit <- ugarchfit(spec = arma34.garch21, data = return, out.sample = 100,
                                solver = "hybrid")
b.34.21 <- infocriteria(arma34.garch21.fit) #BIC = -2.1557

arma34.garch22 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,2)),
                             mean.model = list(armaOrder = c(3,4), include.mean = TRUE),
                             distribution.model = "norm")
arma34.garch22.fit <- ugarchfit(spec = arma34.garch22, data = return, out.sample = 100,
                                solver = "hybrid")
b.34.22 <- infocriteria(arma34.garch22.fit) #BIC = -2.138

arma34.garch23 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,3)),
                             mean.model = list(armaOrder = c(3,4), include.mean = TRUE),
                             distribution.model = "norm")
arma34.garch23.fit <- ugarchfit(spec = arma34.garch23, data = return, out.sample = 100,
                                solver = "hybrid")
b.34.23 <- infocriteria(arma34.garch23.fit) #BIC = -2.1789

arma34.garch24 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,4)),
                             mean.model = list(armaOrder = c(3,4), include.mean = TRUE),
                             distribution.model = "norm")
arma34.garch24.fit <- ugarchfit(spec = arma34.garch24, data = return, out.sample = 100,
                                solver = "hybrid")
b.34.24 <- infocriteria(arma34.garch24.fit) #BIC = -2.1274

arma34.garch31 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,1)),
                             mean.model = list(armaOrder = c(3,4), include.mean = TRUE),
                             distribution.model = "norm")
arma34.garch31.fit <- ugarchfit(spec = arma34.garch31, data = return, out.sample = 100,
                                solver = "hybrid")
b.34.31 <- infocriteria(arma34.garch31.fit) #BIC = -2.1398

arma34.garch32 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,2)),
                             mean.model = list(armaOrder = c(3,4), include.mean = TRUE),
                             distribution.model = "norm")
arma34.garch32.fit <- ugarchfit(spec = arma34.garch32, data = return, out.sample = 100,
                                solver = "hybrid")
b.34.32 <- infocriteria(arma34.garch32.fit) #BIC = -2.1343

arma34.garch33 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,3)),
                             mean.model = list(armaOrder = c(3,4), include.mean = TRUE),
                             distribution.model = "norm")
arma34.garch33.fit <- ugarchfit(spec = arma34.garch33, data = return, out.sample = 100,
                                solver = "hybrid")
b.34.33 <- infocriteria(arma34.garch33.fit) #BIC = -2.1466

arma34.garch34 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,4)),
                             mean.model = list(armaOrder = c(3,4), include.mean = TRUE),
                             distribution.model = "norm")
arma34.garch34.fit <- ugarchfit(spec = arma34.garch34, data = return, out.sample = 100,
                                solver = "hybrid")
b.34.34 <- infocriteria(arma34.garch34.fit) #BIC = -2.0950

arma34.garch41 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,1)),
                             mean.model = list(armaOrder = c(3,4), include.mean = TRUE),
                             distribution.model = "norm")
arma34.garch41.fit <- ugarchfit(spec = arma34.garch41, data = return, out.sample = 100,
                                solver = "hybrid")
b.34.41 <- infocriteria(arma34.garch41.fit) #BIC = -2.1103

arma34.garch42 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,2)),
                             mean.model = list(armaOrder = c(3,4), include.mean = TRUE),
                             distribution.model = "norm")
arma34.garch42.fit <- ugarchfit(spec = arma34.garch42, data = return, out.sample = 100,
                                solver = "hybrid")
b.34.42 <- infocriteria(arma34.garch42.fit) #BIC = -2.0913

arma34.garch43 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,3)),
                             mean.model = list(armaOrder = c(3,4), include.mean = TRUE),
                             distribution.model = "norm")
arma34.garch43.fit <- ugarchfit(spec = arma34.garch43, data = return, out.sample = 100,
                                solver = "hybrid")
b.34.43 <- infocriteria(arma34.garch43.fit) #BIC = -2.1171

arma34.garch44 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,4)),
                             mean.model = list(armaOrder = c(3,4), include.mean = TRUE),
                             distribution.model = "norm")
arma34.garch44.fit <- ugarchfit(spec = arma34.garch44, data = return, out.sample = 100,
                                solver = "hybrid")
b.34.44 <- infocriteria(arma34.garch44.fit) #BIC = -2.0627
##############################
arma41.garch11 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                             mean.model = list(armaOrder = c(4,1), include.mean = TRUE),
                             distribution.model = "norm")
arma41.garch11.fit <- ugarchfit(spec = arma41.garch11, data = return, out.sample = 100,
                                solver = "hybrid")
b.41.11 <- infocriteria(arma41.garch11.fit) #BIC = -2.2232

arma41.garch12 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,2)),
                             mean.model = list(armaOrder = c(4,1), include.mean = TRUE),
                             distribution.model = "norm")
arma41.garch12.fit <- ugarchfit(spec = arma41.garch12, data = return, out.sample = 100,
                                solver = "hybrid")
b.41.12 <- infocriteria(arma41.garch12.fit) #BIC = -2.2068

arma41.garch13 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,3)),
                             mean.model = list(armaOrder = c(4,1), include.mean = TRUE),
                             distribution.model = "norm")
arma41.garch13.fit <- ugarchfit(spec = arma41.garch13, data = return, out.sample = 100,
                                solver = "hybrid")
b.41.13 <- infocriteria(arma41.garch13.fit) #BIC = -2.2169

arma41.garch14 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,4)),
                             mean.model = list(armaOrder = c(4,1), include.mean = TRUE),
                             distribution.model = "norm")
arma41.garch14.fit <- ugarchfit(spec = arma41.garch14, data = return, out.sample = 100,
                                solver = "hybrid")
b.41.14 <- infocriteria(arma41.garch14.fit) #BIC = -2.1956

arma41.garch21 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,1)),
                             mean.model = list(armaOrder = c(4,1), include.mean = TRUE),
                             distribution.model = "norm")
arma41.garch21.fit <- ugarchfit(spec = arma41.garch21, data = return, out.sample = 100,
                                solver = "hybrid")
b.41.21 <- infocriteria(arma41.garch21.fit) #BIC = -2.1922

arma41.garch22 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,2)),
                             mean.model = list(armaOrder = c(4,1), include.mean = TRUE),
                             distribution.model = "norm")
arma41.garch22.fit <- ugarchfit(spec = arma41.garch22, data = return, out.sample = 100,
                                solver = "hybrid")
b.41.22 <-infocriteria(arma41.garch22.fit) #BIC = -2.1745

arma41.garch23 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,3)),
                             mean.model = list(armaOrder = c(4,1), include.mean = TRUE),
                             distribution.model = "norm")
arma41.garch23.fit <- ugarchfit(spec = arma41.garch23, data = return, out.sample = 100,
                                solver = "hybrid")
b.41.23 <- infocriteria(arma41.garch23.fit) #BIC = -2.2021

arma41.garch24 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,4)),
                             mean.model = list(armaOrder = c(4,1), include.mean = TRUE),
                             distribution.model = "norm")
arma41.garch24.fit <- ugarchfit(spec = arma41.garch24, data = return, out.sample = 100,
                                solver = "hybrid")
b.41.24 <- infocriteria(arma41.garch24.fit) #BIC = -2.1634

arma41.garch31 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,1)),
                             mean.model = list(armaOrder = c(4,1), include.mean = TRUE),
                             distribution.model = "norm")
arma41.garch31.fit <- ugarchfit(spec = arma41.garch31, data = return, out.sample = 100,
                                solver = "hybrid")
b.41.31 <- infocriteria(arma41.garch31.fit) #BIC = -2.1783

arma41.garch32 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,2)),
                             mean.model = list(armaOrder = c(4,1), include.mean = TRUE),
                             distribution.model = "norm")
arma41.garch32.fit <- ugarchfit(spec = arma41.garch32, data = return, out.sample = 100,
                                solver = "hybrid")
b.41.32 <- infocriteria(arma41.garch32.fit) #BIC = -2.1593

arma41.garch33 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,3)),
                             mean.model = list(armaOrder = c(4,1), include.mean = TRUE),
                             distribution.model = "norm")
arma41.garch33.fit <- ugarchfit(spec = arma41.garch33, data = return, out.sample = 100,
                                solver = "hybrid")
b.41.33 <- infocriteria(arma41.garch33.fit) #BIC = -2.1697

arma41.garch34 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,4)),
                             mean.model = list(armaOrder = c(4,1), include.mean = TRUE),
                             distribution.model = "norm")
arma41.garch34.fit <- ugarchfit(spec = arma41.garch34, data = return, out.sample = 100,
                                solver = "hybrid")
b.41.34 <-infocriteria(arma41.garch34.fit) #BIC = -2.131

arma41.garch41 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,1)),
                             mean.model = list(armaOrder = c(4,1), include.mean = TRUE),
                             distribution.model = "norm")
arma41.garch41.fit <- ugarchfit(spec = arma41.garch41, data = return, out.sample = 100,
                                solver = "hybrid")
b.41.41 <-infocriteria(arma41.garch41.fit) #BIC = -2.1444

arma41.garch42 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,2)),
                             mean.model = list(armaOrder = c(4,1), include.mean = TRUE),
                             distribution.model = "norm")
arma41.garch42.fit <- ugarchfit(spec = arma41.garch42, data = return, out.sample = 100,
                                solver = "hybrid")
b.41.42 <-infocriteria(arma41.garch42.fit) #BIC = -2.1287

arma41.garch43 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,3)),
                             mean.model = list(armaOrder = c(4,1), include.mean = TRUE),
                             distribution.model = "norm")
arma41.garch43.fit <- ugarchfit(spec = arma41.garch43, data = return, out.sample = 100,
                                solver = "hybrid")
b.41.43 <-infocriteria(arma41.garch43.fit) #BIC = -2.1416

arma41.garch44 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,4)),
                             mean.model = list(armaOrder = c(4,1), include.mean = TRUE),
                             distribution.model = "norm")
arma41.garch44.fit <- ugarchfit(spec = arma41.garch44, data = return, out.sample = 100,
                                solver = "hybrid")
b.41.44 <-infocriteria(arma41.garch44.fit) #BIC = -2.0986
######################################
arma42.garch11 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                             mean.model = list(armaOrder = c(4,2), include.mean = TRUE),
                             distribution.model = "norm")
arma42.garch11.fit <- ugarchfit(spec = arma42.garch11, data = return, out.sample = 100,
                                solver = "hybrid")
b.42.11 <- infocriteria(arma42.garch11.fit) #BIC = -2.2232

arma42.garch12 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,2)),
                             mean.model = list(armaOrder = c(4,2), include.mean = TRUE),
                             distribution.model = "norm")
arma42.garch12.fit <- ugarchfit(spec = arma42.garch12, data = return, out.sample = 100,
                                solver = "hybrid")
b.42.12 <- infocriteria(arma42.garch12.fit) #BIC = -2.2068

arma42.garch13 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,3)),
                             mean.model = list(armaOrder = c(4,2), include.mean = TRUE),
                             distribution.model = "norm")
arma42.garch13.fit <- ugarchfit(spec = arma42.garch13, data = return, out.sample = 100,
                                solver = "hybrid")
b.42.13 <- infocriteria(arma42.garch13.fit) #BIC = -2.2169

arma42.garch14 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,4)),
                             mean.model = list(armaOrder = c(4,2), include.mean = TRUE),
                             distribution.model = "norm")
arma42.garch14.fit <- ugarchfit(spec = arma42.garch14, data = return, out.sample = 100,
                                solver = "hybrid")
b.42.14 <- infocriteria(arma42.garch14.fit) #BIC = -2.1956

arma42.garch21 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,1)),
                             mean.model = list(armaOrder = c(4,2), include.mean = TRUE),
                             distribution.model = "norm")
arma42.garch21.fit <- ugarchfit(spec = arma42.garch21, data = return, out.sample = 100,
                                solver = "hybrid")
b.42.21 <- infocriteria(arma42.garch21.fit) #BIC = -2.1922

arma42.garch22 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,2)),
                             mean.model = list(armaOrder = c(4,2), include.mean = TRUE),
                             distribution.model = "norm")
arma42.garch22.fit <- ugarchfit(spec = arma42.garch22, data = return, out.sample = 100,
                                solver = "hybrid")
b.42.22 <-infocriteria(arma42.garch22.fit) #BIC = -2.1745

arma42.garch23 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,3)),
                             mean.model = list(armaOrder = c(4,2), include.mean = TRUE),
                             distribution.model = "norm")
arma42.garch23.fit <- ugarchfit(spec = arma42.garch23, data = return, out.sample = 100,
                                solver = "hybrid")
b.42.23 <- infocriteria(arma42.garch23.fit) #BIC = -2.2021

arma42.garch24 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,4)),
                             mean.model = list(armaOrder = c(4,2), include.mean = TRUE),
                             distribution.model = "norm")
arma42.garch24.fit <- ugarchfit(spec = arma42.garch24, data = return, out.sample = 100,
                                solver = "hybrid")
b.42.24 <- infocriteria(arma42.garch24.fit) #BIC = -2.1634

arma42.garch31 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,1)),
                             mean.model = list(armaOrder = c(4,2), include.mean = TRUE),
                             distribution.model = "norm")
arma42.garch31.fit <- ugarchfit(spec = arma42.garch31, data = return, out.sample = 100,
                                solver = "hybrid")
b.42.31 <- infocriteria(arma42.garch31.fit) #BIC = -2.1783

arma42.garch32 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,2)),
                             mean.model = list(armaOrder = c(4,2), include.mean = TRUE),
                             distribution.model = "norm")
arma42.garch32.fit <- ugarchfit(spec = arma42.garch32, data = return, out.sample = 100,
                                solver = "hybrid")
b.42.32 <- infocriteria(arma42.garch32.fit) #BIC = -2.1593

arma42.garch33 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,3)),
                             mean.model = list(armaOrder = c(4,2), include.mean = TRUE),
                             distribution.model = "norm")
arma42.garch33.fit <- ugarchfit(spec = arma42.garch33, data = return, out.sample = 100,
                                solver = "hybrid")
b.42.33 <- infocriteria(arma42.garch33.fit) #BIC = -2.1697

arma42.garch34 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,4)),
                             mean.model = list(armaOrder = c(4,2), include.mean = TRUE),
                             distribution.model = "norm")
arma42.garch34.fit <- ugarchfit(spec = arma42.garch34, data = return, out.sample = 100,
                                solver = "hybrid")
b.42.34 <-infocriteria(arma42.garch34.fit) #BIC = -2.131

arma42.garch41 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,1)),
                             mean.model = list(armaOrder = c(4,2), include.mean = TRUE),
                             distribution.model = "norm")
arma42.garch41.fit <- ugarchfit(spec = arma42.garch41, data = return, out.sample = 100,
                                solver = "hybrid")
b.42.41 <-infocriteria(arma42.garch41.fit) #BIC = -2.1444

arma42.garch42 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,2)),
                             mean.model = list(armaOrder = c(4,2), include.mean = TRUE),
                             distribution.model = "norm")
arma42.garch42.fit <- ugarchfit(spec = arma42.garch42, data = return, out.sample = 100,
                                solver = "hybrid")
b.42.42 <-infocriteria(arma42.garch42.fit) #BIC = -2.1287

arma42.garch43 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,3)),
                             mean.model = list(armaOrder = c(4,2), include.mean = TRUE),
                             distribution.model = "norm")
arma42.garch43.fit <- ugarchfit(spec = arma42.garch43, data = return, out.sample = 100,
                                solver = "hybrid")
b.42.43 <-infocriteria(arma42.garch43.fit) #BIC = -2.1416

arma42.garch44 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,4)),
                             mean.model = list(armaOrder = c(4,2), include.mean = TRUE),
                             distribution.model = "norm")
arma42.garch44.fit <- ugarchfit(spec = arma42.garch44, data = return, out.sample = 100,
                                solver = "hybrid")
b.42.44 <-infocriteria(arma42.garch44.fit) #BIC = -2.0986
##############################
arma43.garch11 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                             mean.model = list(armaOrder = c(4,3), include.mean = TRUE),
                             distribution.model = "norm")
arma43.garch11.fit <- ugarchfit(spec = arma43.garch11, data = return, out.sample = 100,
                                solver = "hybrid")
b.43.11 <- infocriteria(arma43.garch11.fit) #BIC = -2.2232

arma43.garch12 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,2)),
                             mean.model = list(armaOrder = c(4,3), include.mean = TRUE),
                             distribution.model = "norm")
arma43.garch12.fit <- ugarchfit(spec = arma43.garch12, data = return, out.sample = 100,
                                solver = "hybrid")
b.43.12 <- infocriteria(arma43.garch12.fit) #BIC = -2.2068

arma43.garch13 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,3)),
                             mean.model = list(armaOrder = c(4,3), include.mean = TRUE),
                             distribution.model = "norm")
arma43.garch13.fit <- ugarchfit(spec = arma43.garch13, data = return, out.sample = 100,
                                solver = "hybrid")
b.43.13 <- infocriteria(arma43.garch13.fit) #BIC = -2.2169

arma43.garch14 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,4)),
                             mean.model = list(armaOrder = c(4,3), include.mean = TRUE),
                             distribution.model = "norm")
arma43.garch14.fit <- ugarchfit(spec = arma43.garch14, data = return, out.sample = 100,
                                solver = "hybrid")
b.43.14 <- infocriteria(arma43.garch14.fit) #BIC = -2.1956

arma43.garch21 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,1)),
                             mean.model = list(armaOrder = c(4,3), include.mean = TRUE),
                             distribution.model = "norm")
arma43.garch21.fit <- ugarchfit(spec = arma43.garch21, data = return, out.sample = 100,
                                solver = "hybrid")
b.43.21 <- infocriteria(arma43.garch21.fit) #BIC = -2.1922

arma43.garch22 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,2)),
                             mean.model = list(armaOrder = c(4,3), include.mean = TRUE),
                             distribution.model = "norm")
arma43.garch22.fit <- ugarchfit(spec = arma43.garch22, data = return, out.sample = 100,
                                solver = "hybrid")
b.43.22 <-infocriteria(arma43.garch22.fit) #BIC = -2.1745

arma43.garch23 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,3)),
                             mean.model = list(armaOrder = c(4,3), include.mean = TRUE),
                             distribution.model = "norm")
arma43.garch23.fit <- ugarchfit(spec = arma43.garch23, data = return, out.sample = 100,
                                solver = "hybrid")
b.43.23 <- infocriteria(arma43.garch23.fit) #BIC = -2.2021

arma43.garch24 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,4)),
                             mean.model = list(armaOrder = c(4,3), include.mean = TRUE),
                             distribution.model = "norm")
arma43.garch24.fit <- ugarchfit(spec = arma43.garch24, data = return, out.sample = 100,
                                solver = "hybrid")
b.43.24 <- infocriteria(arma43.garch24.fit) #BIC = -2.1634

arma43.garch31 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,1)),
                             mean.model = list(armaOrder = c(4,3), include.mean = TRUE),
                             distribution.model = "norm")
arma43.garch31.fit <- ugarchfit(spec = arma43.garch31, data = return, out.sample = 100,
                                solver = "hybrid")
b.43.31 <- infocriteria(arma43.garch31.fit) #BIC = -2.1783

arma43.garch32 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,2)),
                             mean.model = list(armaOrder = c(4,3), include.mean = TRUE),
                             distribution.model = "norm")
arma43.garch32.fit <- ugarchfit(spec = arma43.garch32, data = return, out.sample = 100,
                                solver = "hybrid")
b.43.32 <- infocriteria(arma43.garch32.fit) #BIC = -2.1593

arma43.garch33 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,3)),
                             mean.model = list(armaOrder = c(4,3), include.mean = TRUE),
                             distribution.model = "norm")
arma43.garch33.fit <- ugarchfit(spec = arma43.garch33, data = return, out.sample = 100,
                                solver = "hybrid")
b.43.33 <- infocriteria(arma43.garch33.fit) #BIC = -2.1697

arma43.garch34 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,4)),
                             mean.model = list(armaOrder = c(4,3), include.mean = TRUE),
                             distribution.model = "norm")
arma43.garch34.fit <- ugarchfit(spec = arma43.garch34, data = return, out.sample = 100,
                                solver = "hybrid")
b.43.34 <-infocriteria(arma43.garch34.fit) #BIC = -2.131

arma43.garch41 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,1)),
                             mean.model = list(armaOrder = c(4,3), include.mean = TRUE),
                             distribution.model = "norm")
arma43.garch41.fit <- ugarchfit(spec = arma43.garch41, data = return, out.sample = 100,
                                solver = "hybrid")
b.43.41 <-infocriteria(arma43.garch41.fit) #BIC = -2.1444

arma43.garch42 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,2)),
                             mean.model = list(armaOrder = c(4,3), include.mean = TRUE),
                             distribution.model = "norm")
arma43.garch42.fit <- ugarchfit(spec = arma43.garch42, data = return, out.sample = 100,
                                solver = "hybrid")
b.43.42 <-infocriteria(arma43.garch42.fit) #BIC = -2.1287

arma43.garch43 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,3)),
                             mean.model = list(armaOrder = c(4,3), include.mean = TRUE),
                             distribution.model = "norm")
arma43.garch43.fit <- ugarchfit(spec = arma43.garch43, data = return, out.sample = 100,
                                solver = "hybrid")
b.43.43 <-infocriteria(arma43.garch43.fit) #BIC = -2.1416

arma43.garch44 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,4)),
                             mean.model = list(armaOrder = c(4,3), include.mean = TRUE),
                             distribution.model = "norm")
arma43.garch44.fit <- ugarchfit(spec = arma43.garch44, data = return, out.sample = 100,
                                solver = "hybrid")
b.43.44 <-infocriteria(arma43.garch44.fit) #BIC = -2.0986
##################################
arma44.garch11 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                             mean.model = list(armaOrder = c(4,4), include.mean = TRUE),
                             distribution.model = "norm")
arma44.garch11.fit <- ugarchfit(spec = arma44.garch11, data = return, out.sample = 100,
                                solver = "hybrid")
b.44.11 <- infocriteria(arma44.garch11.fit) #BIC = -2.2232

arma44.garch12 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,2)),
                             mean.model = list(armaOrder = c(4,4), include.mean = TRUE),
                             distribution.model = "norm")
arma44.garch12.fit <- ugarchfit(spec = arma44.garch12, data = return, out.sample = 100,
                                solver = "hybrid")
b.44.12 <- infocriteria(arma44.garch12.fit) #BIC = -2.2068

arma44.garch13 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,3)),
                             mean.model = list(armaOrder = c(4,4), include.mean = TRUE),
                             distribution.model = "norm")
arma44.garch13.fit <- ugarchfit(spec = arma44.garch13, data = return, out.sample = 100,
                                solver = "hybrid")
b.44.13 <- infocriteria(arma44.garch13.fit) #BIC = -2.2169

arma44.garch14 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,4)),
                             mean.model = list(armaOrder = c(4,4), include.mean = TRUE),
                             distribution.model = "norm")
arma44.garch14.fit <- ugarchfit(spec = arma44.garch14, data = return, out.sample = 100,
                                solver = "hybrid")
b.44.14 <- infocriteria(arma44.garch14.fit) #BIC = -2.1956

arma44.garch21 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,1)),
                             mean.model = list(armaOrder = c(4,4), include.mean = TRUE),
                             distribution.model = "norm")
arma44.garch21.fit <- ugarchfit(spec = arma44.garch21, data = return, out.sample = 100,
                                solver = "hybrid")
b.44.21 <- infocriteria(arma44.garch21.fit) #BIC = -2.1922

arma44.garch22 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,2)),
                             mean.model = list(armaOrder = c(4,4), include.mean = TRUE),
                             distribution.model = "norm")
arma44.garch22.fit <- ugarchfit(spec = arma44.garch22, data = return, out.sample = 100,
                                solver = "hybrid")
b.44.22 <-infocriteria(arma44.garch22.fit) #BIC = -2.1745

arma44.garch23 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,3)),
                             mean.model = list(armaOrder = c(4,4), include.mean = TRUE),
                             distribution.model = "norm")
arma44.garch23.fit <- ugarchfit(spec = arma44.garch23, data = return, out.sample = 100,
                                solver = "hybrid")
b.44.23 <- infocriteria(arma44.garch23.fit) #BIC = -2.2021

arma44.garch24 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,4)),
                             mean.model = list(armaOrder = c(4,4), include.mean = TRUE),
                             distribution.model = "norm")
arma44.garch24.fit <- ugarchfit(spec = arma44.garch24, data = return, out.sample = 100,
                                solver = "hybrid")
b.44.24 <- infocriteria(arma44.garch24.fit) #BIC = -2.1634

arma44.garch31 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,1)),
                             mean.model = list(armaOrder = c(4,4), include.mean = TRUE),
                             distribution.model = "norm")
arma44.garch31.fit <- ugarchfit(spec = arma44.garch31, data = return, out.sample = 100,
                                solver = "hybrid")
b.44.31 <- infocriteria(arma44.garch31.fit) #BIC = -2.1783

arma44.garch32 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,2)),
                             mean.model = list(armaOrder = c(4,4), include.mean = TRUE),
                             distribution.model = "norm")
arma44.garch32.fit <- ugarchfit(spec = arma44.garch32, data = return, out.sample = 100,
                                solver = "hybrid")
b.44.32 <- infocriteria(arma44.garch32.fit) #BIC = -2.1593

arma44.garch33 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,3)),
                             mean.model = list(armaOrder = c(4,4), include.mean = TRUE),
                             distribution.model = "norm")
arma44.garch33.fit <- ugarchfit(spec = arma44.garch33, data = return, out.sample = 100,
                                solver = "hybrid")
b.44.33 <- infocriteria(arma44.garch33.fit) #BIC = -2.1697

arma44.garch34 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,4)),
                             mean.model = list(armaOrder = c(4,4), include.mean = TRUE),
                             distribution.model = "norm")
arma44.garch34.fit <- ugarchfit(spec = arma44.garch34, data = return, out.sample = 100,
                                solver = "hybrid")
b.44.34 <-infocriteria(arma44.garch34.fit) #BIC = -2.131

arma44.garch41 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,1)),
                             mean.model = list(armaOrder = c(4,4), include.mean = TRUE),
                             distribution.model = "norm")
arma44.garch41.fit <- ugarchfit(spec = arma44.garch41, data = return, out.sample = 100,
                                solver = "hybrid")
b.44.41 <-infocriteria(arma44.garch41.fit) #BIC = -2.1444

arma44.garch42 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,2)),
                             mean.model = list(armaOrder = c(4,4), include.mean = TRUE),
                             distribution.model = "norm")
arma44.garch42.fit <- ugarchfit(spec = arma44.garch42, data = return, out.sample = 100,
                                solver = "hybrid")
b.44.42 <-infocriteria(arma44.garch42.fit) #BIC = -2.1287

arma44.garch43 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,3)),
                             mean.model = list(armaOrder = c(4,4), include.mean = TRUE),
                             distribution.model = "norm")
arma44.garch43.fit <- ugarchfit(spec = arma44.garch43, data = return, out.sample = 100,
                                solver = "hybrid")
b.44.43 <-infocriteria(arma44.garch43.fit) #BIC = -2.1416

arma44.garch44 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4,4)),
                             mean.model = list(armaOrder = c(4,4), include.mean = TRUE),
                             distribution.model = "norm")
arma44.garch44.fit <- ugarchfit(spec = arma44.garch44, data = return, out.sample = 100,
                                solver = "hybrid")
b.44.44 <-infocriteria(arma44.garch44.fit) #BIC = -2.0986

BIC.all <- cbind(b.11.11[2], b.11.12[2], b.11.13[2], b.11.14[2], b.11.21[2], b.11.22[2], b.11.23[2], b.11.24[2], b.11.31[2], 
                 b.11.32[2], b.11.33[2], b.11.34[2], b.11.41[2], b.11.42[2], b.11.43[2], b.11.44[2],
                 b.12.11[2], b.12.12[2], b.12.13[2], b.12.14[2], b.12.21[2], b.12.22[2], b.12.23[2], b.12.24[2], b.12.31[2], 
                 b.12.32[2], b.12.33[2], b.12.34[2], b.12.41[2], b.12.42[2], b.12.43[2], b.12.44[2],
                 b.13.11[2], b.13.12[2], b.13.13[2], b.13.14[2], b.13.21[2], b.13.22[2], b.13.23[2], b.13.24[2], b.13.31[2], 
                 b.13.32[2], b.13.33[2], b.13.34[2], b.13.41[2], b.13.42[2], b.13.43[2], b.13.44[2],
                 b.14.11[2], b.14.12[2], b.14.13[2], b.14.14[2], b.14.21[2], b.14.22[2], b.14.23[2], b.14.24[2], b.14.31[2], 
                 b.14.32[2], b.14.33[2], b.14.34[2], b.14.41[2], b.14.42[2], b.14.43[2], b.14.44[2],
                 b.21.11[2], b.21.12[2], b.21.13[2], b.21.14[2], b.21.21[2], b.21.22[2], b.21.23[2], b.21.24[2], b.21.31[2], 
                 b.21.32[2], b.21.33[2], b.21.34[2], b.21.41[2], b.21.42[2], b.21.43[2], b.21.44[2],
                 b.22.11[2], b.22.12[2], b.22.13[2], b.22.14[2], b.22.21[2], b.22.22[2], b.22.23[2], b.22.24[2], b.22.31[2], 
                 b.22.32[2], b.22.33[2], b.22.34[2], b.22.41[2], b.22.42[2], b.22.43[2], b.22.44[2],
                 b.23.11[2], b.23.12[2], b.23.13[2], b.23.14[2], b.23.21[2], b.23.22[2], b.23.23[2], b.23.24[2], b.23.31[2], 
                 b.23.32[2], b.23.33[2], b.23.34[2], b.23.41[2], b.23.42[2], b.23.43[2], b.23.44[2],
                 b.24.11[2], b.24.12[2], b.24.13[2], b.24.14[2], b.24.21[2], b.24.22[2], b.24.23[2], b.24.24[2], b.24.31[2], 
                 b.24.32[2], b.24.33[2], b.24.34[2], b.24.41[2], b.24.42[2], b.24.43[2], b.24.44[2],
                 b.31.11[2], b.31.12[2], b.31.13[2], b.31.14[2], b.31.21[2], b.31.22[2], b.31.23[2], b.31.24[2], b.31.31[2], 
                 b.31.32[2], b.31.33[2], b.31.34[2], b.31.41[2], b.31.42[2], b.31.43[2], b.31.44[2],
                 b.32.11[2], b.32.12[2], b.32.13[2], b.32.14[2], b.32.21[2], b.32.22[2], b.32.23[2], b.32.24[2], b.32.31[2], 
                 b.32.32[2], b.32.33[2], b.32.34[2], b.32.41[2], b.32.42[2], b.32.43[2], b.32.44[2],
                 b.33.11[2], b.33.12[2], b.33.13[2], b.33.14[2], b.33.21[2], b.33.22[2], b.33.23[2], b.33.24[2], b.33.31[2], 
                 b.33.32[2], b.33.33[2], b.33.34[2], b.33.41[2], b.33.42[2], b.33.43[2], b.33.44[2],
                 b.34.11[2], b.34.12[2], b.34.13[2], b.34.14[2], b.34.21[2], b.34.22[2], b.34.23[2], b.34.24[2], b.34.31[2], 
                 b.34.32[2], b.34.33[2], b.34.34[2], b.34.41[2], b.34.42[2], b.34.43[2], b.34.44[2],
                 b.41.11[2], b.41.12[2], b.41.13[2], b.41.14[2], b.41.21[2], b.41.22[2], b.41.23[2], b.41.24[2], b.41.31[2], 
                 b.41.32[2], b.41.33[2], b.41.34[2], b.41.41[2], b.41.42[2], b.41.43[2], b.41.44[2],
                 b.42.11[2], b.42.12[2], b.42.13[2], b.42.14[2], b.42.21[2], b.42.22[2], b.42.23[2], b.42.24[2], b.42.31[2], 
                 b.42.32[2], b.42.33[2], b.42.34[2], b.42.41[2], b.42.42[2], b.42.43[2], b.42.44[2],
                 b.43.11[2], b.43.12[2], b.43.13[2], b.43.14[2], b.43.21[2], b.43.42[2], b.43.23[2], b.43.24[2], b.43.31[2], 
                 b.43.32[2], b.43.33[2], b.43.34[2], b.43.41[2], b.43.42[2], b.43.43[2], b.43.44[2],
                 b.44.11[2], b.44.12[2], b.44.13[2], b.44.14[2], b.44.21[2], b.44.22[2], b.44.23[2], b.44.24[2], b.44.31[2], 
                 b.44.32[2], b.44.33[2], b.44.34[2], b.44.41[2], b.44.42[2], b.44.43[2], b.44.44[2])
which.min(BIC.all) #Entry 146 = ARMA(3,2)-GARCH(1,2) BIC = -3.73889