
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
eacf(abs.14) 
#GARCH(0,4), (1,1), (1,5), (1,6), (2,1), (2,2), (2,7), (3,1), (3,2), (3,3), (3,4), (3,6), (3,7),
#(4,0) --- (4,4), (4,7), (5,0) --- (5,7)

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
eacf(abs.15) 
#GARCH(1,1), (1,2), (2,2), (3,1), (3,2), (3,3), (4,1) --- (4,4), (5,1) --- (5,5)






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
ar4_fit_sep2018 <- arfimafit(spec = ar_spec_ar4_sep2018, data = return.sep2018, 
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



ar4.sep18.forecast <- arfimaforecast(ar4_fit_sep2018, n.ahead = 5, n.roll = out_of_sample.sep2018-1, 
                                     out.sample = out_of_sample.sep2018, data = sep2018)
plot(return.sep2018, ylim = c(-0.0001, 0.0001), col = "blue"); lines(ar4.sep18.forecast@forecast$seriesFor[1,], col = "red")
plot(y = ar4.sep18.forecast@forecast$seriesFor, x = sep2018, type = "l")
plot(sep2018); lines(fitted(ar4.sep18.forecast), col = "red", lty = 2)
plot(fitted(ar4.sep18.forecast))


rolling <- arfimaroll(spec = ar_spec_ar4_sep2018, data = return.sep2018, n.ahead = 1, forecast.length = 5, 
                      refit.every = 3, refit.window = "moving", solver = "hybrid", calculate.VaR = TRUE, VaR.alpha = c(0.01, 0.05))
rolling.fore <- rolling@forecast$density$Mu


####################### 
first <- as.Date("2018-09-17")
last <- as.Date("2018-09-19")
part1 <- window(sep2018, end = last)
part2 <- window(sep2018, start = last+1, end = as.Date("2018-09-30"))

ar4.test <- Arima(part1, order = c(4,0,0))
ar4.f <- forecast::forecast(ar4.test, length(part2))

plot(ar4.f, main = "AR(4) Model Multistep Forecasts", type = "o")
lines(ar4.f$mean, type = "p", pch = 16, lty = "dashed", col = "green")
lines(part2, type = "o", pch = 16, lty = "dotted", col = "black")

arima2014 <- auto.arima(t2, stepwise=FALSE, allowmean = FALSE, allowdrift = FALSE, trace=TRUE, 
                        approximation=FALSE) # AR(2) ar1 = -0.03357, ar2 = -0.12005

ar2014 <- ar.mle(t2, aic = TRUE, order.max = NULL, intercept = TRUE) # AR(2) ar1 = -0.0415, ar2 = -0.1280

RMSE.arima2014 <- RMSE(t2, fitted(arima2014)) # RMSE = 0.0393
RMSE.ar2014 <- RMSE(t2[3:365], fitted(ar2014)[3:365]) #R MSE = 0.0392

return2014 <- return[92:456,] 

par(mfrow = c(1,1))
McLeod.Li.test(y = return2014, main = "McLeod-Li test statistics for Returns 2014")
# Signs of volatility

abs.14 <- abs(return2014)
sqr.14 <- return2014^2

par(mfrow=c(1,2))
acf(abs.14, ci.type = "ma", main = "ACF for abs. returns")
pacf(abs.14, main = "PACF plot for abs.returns")

par(mfrow=c(1,2))
acf(sqr.14, ci.type = "ma", main = "ACF  for sqr. return")
pacf(sqr.14, main = "PACF for sqr. return")

ar2.garch04 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(0,4)), 
                          mean.model = list(armaOrder = c(2, 0), include.mean = TRUE), 
                          distribution.model = "norm")

ar2garch04 <- ugarchfit(spec = ar2.garch04, data = return2014, out.sample = 265)
plot(ar2garch04, which = "all")
armagarch

RMSE.armagarch2014 <- RMSE(return2014, fitted(ar2garch04)) # RMSE = 0.0473

# Forecasting of ARMA-GARCH for 2014
forecast_ar2garch04 <- ugarchforecast(ar2garch04, data = return2014, n.ahead = 5, n.roll = 250)
print(forecast_ar2garch04)
plot(forecast_ar2garch04, which = "all")

#arima2015 <- auto.arima(t3, stepwise=FALSE, allowmean = FALSE, allowdrift = FALSE, trace=TRUE, 
#approximation=FALSE) 
# AR(5), ar1 = -0.02244, ar2 = -0.1092, ar3 = 0.0343, ar4 = -0.0588, ar5 = 0.1303

#ar2015 <- ar.mle(t3, aic = TRUE, order.max = NULL, intercept = TRUE) 
# AR(8), ar1 = -0.0334, ar2 = -0.0842, ar3 = 0.0360, ar4 = -0.0489, ar5 = 0.1240, 
# ar6 = 0.0620, ar7 = -0.1158, ar8 = -0.0856

#RMSE.arima2015 <- RMSE(t3, fitted(arima2015)) #RMSE = 0.0371
#RMSE.ar2015 <- RMSE(t3[9:365], fitted(ar2015)[9:365]) #RMSE = 0.0363



#par(mfrow = c(1,1))
#McLeod.Li.test(y = return2015, main = "McLeod-Li test statistics for Returns 2015")
# Signs of volatility

#abs.15 <- abs(return2015)
#sqr.15 <- return2015^2

#par(mfrow=c(1,2))
#acf(abs.15, ci.type = "ma", main = "ACF for abs. returns")
#pacf(abs.15, main = "PACF plot for abs.returns")

#par(mfrow=c(1,2))
#acf(sqr.15, ci.type = "ma", main = "ACF  for sqr. return")
#pacf(sqr.15, main = "PACF for sqr. return")

#ar5.garch11 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)), 
#  mean.model = list(armaOrder = c(5, 0), include.mean = TRUE), 
# distribution.model = "norm")

#ar5garch11 <- ugarchfit(spec = ar5.garch11, data = return2015, out.sample = 265)
#plot(ar5garch11, which = "all")
#ar5garch11

#RMSE.armagarch2015 <- RMSE(return2015, fitted(ar5garch11)) # RMSE = 0.0463

# Forecast of AR(5)-GARCH(1,1) model for 2015
#forecast_ar5garch11 <- ugarchforecast(ar5garch11, data = return2015, n.ahead = 5, n.roll = 250)
#print(forecast_ar5garch11)
#plot(forecast_ar5garch11)

#ar8.garch11 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)), 
#                         mean.model = list(armaOrder = c(8, 0), include.mean = TRUE), 
#                        distribution.model = "norm")

#ar8garch11 <- ugarchfit(spec = ar8.garch11, data = return2015, out.sample = 265)
#plot(ar8garch11, which = "all")
#ar8garch11

#RMSE.armagarch2015.2 <- RMSE(return2015, fitted(ar8garch11)) #RMSE = 0.0462

#forecast_ar8garch11 <- ugarchforecast(ar8garch11, data = return2015, n.ahead = 5, n.roll = 250)
#print(forecast_ar8garch11)
#plot(forecast_ar8garch11)




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



return.may2018 <- return[2161:2190,] 

par(mfrow = c(1,1))
McLeod.Li.test(y = return.may2018, main = "McLeod-Li test statistics for Returns May 2018")
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



t.2020 <- window(return, start = as.Date("2020-01-01"), end = as.Date("2020-12-31"))
arma.mod2020 <- auto.arima(t.2020, approximation = FALSE, stepwise = FALSE, allowmean = FALSE, 
                           allowdrift = FALSE); coeftest(arma.mod2020) # ARMA(1,2)
t.21 <- window(return, start = as.Date("2021-01-01"), end = as.Date("2021-04-16"))
arma.forecast2020 <- forecast::forecast(arma.mod2020, h = 10, level = 95)
plot(arma.forecast2020, xaxs = "i")
lines(t.21, col = "red", lty = 2)

McLeod.Li.test(y = t.2020)

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

covar1 <- ar1.1$asy.var.coef; covar2 <- ar2.2$asy.var.coef; covar3 <- ar3.3$asy.var.coef 
covar4 <- 0; covar5 <- ar5.5$asy.var.coef; covar6 <- 0; covar7 <- 0; 
covar8 <- ar8.8$asy.var.coef; covar9 <- 0

U1 <- chol(covar1)
T1 <- t(U1)
InvT1 <- solve(T1)
beta1 <- ar1.1$ar
beta.stand1 <- InvT1 %*% beta1
mimt1 <- (sum(abs(beta.stand1)))/(1+sum(abs(beta.stand1))) #0.8626

U2 <- chol(covar2)
T2 <- t(U2)
InvT2 <- solve(T2)
beta2 <- ar2.2$ar
beta.stand2 <- InvT2 %*% beta2
mimt2 <- (sum(abs(beta.stand2)))/(1+sum(abs(beta.stand2))) #0.7701

U3 <- chol(covar3)
T3 <- t(U3)
InvT3 <- solve(T3)
beta3 <- ar3.3$ar
beta.stand3 <- InvT3 %*% beta3
mimt3 <- (sum(abs(beta.stand3)))/(1+sum(abs(beta.stand3))) #0.9225

mimt4 <- 0

U5 <- chol(covar5)
T5 <- t(U5)
InvT5 <- solve(T5)
beta5 <- ar5.5$ar
beta.stand5 <- InvT5 %*% beta5
mimt5 <- (sum(abs(beta.stand5)))/(1+sum(abs(beta.stand5))) #0.9652

mimt6 <- 0; mimt7 <- 0

U8 <- chol(covar8)
T8 <- t(U8)
InvT8 <- solve(T8)
beta8 <- ar8.8$ar
beta.stand8 <- InvT8 %*% beta8
mimt8 <- (sum(abs(beta.stand8)))/(1+sum(abs(beta.stand8))) #0.6309

mimt9 <- 0
mim <- rbind(mimt1, mimt2, mimt3, mimt4, mimt5, mimt6, mimt7, mimt8, mimt9)

years <- lubridate::year(dates)
years <- cbind(years[1], years[93], years[458], years[823], years[1189], years[1554], years[1920],
               years[2286], years[2652])

plot(y = mim, x = years, type = "l", ylab = "MIM_t", main = "Yearly MIM", xlab = "Years")

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

plot(y = AMIM, x = years, type = "l", 
     main = "Yearly AMIM", xlab = "Years"); abline(h = 0, col = "blue", lty = 3)