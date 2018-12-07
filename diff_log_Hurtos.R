library(plyr)
library(readxl)
library(RcppRoll)
library(reshape2)
library(tidyverse)
library(lubridate)
library(forecast)
library(ggplot2)
library(ggplot)
library(xlsx)
library(formattable)
library(plyr)
library(readxl)
library(RcppRoll)
library(reshape2)
library(tidyverse)
library(lubridate)
library(forecast)
library(ggplot2)
library(ggplot)
library(xlsx)
library(formattable)
library(zoo)
library(lmtest)
library(sarima)



series1 <- read.csv("~/Eco.csv", sep=";")
View(series1)
str(series1)

Hurtos=ts(series1$HURTOS.A.PERSONAS,frequency = 12,start = 2013)
tasa=ts(series1$TASA.DE.DESEMPLEO,frequency = 12,start = 2013)


ggtsdisplay(Hurtos)
ggtsdisplay(log(Hurtos))
#Aplicamos log por que disminuye la varianza praaa
######################################HURTOS################################
summary(t.m1 <- Arima(Hurtos, order = c(1,0,1),seasonal = c(1,0,0),include.constant = F,method="ML")) # SARIMA(0,0,0)(1,0,0)[4C1))
coeftest(t.m1)
checkresiduals(t.m1) 
acf(resid(t.m1)^2)



# Efecto ARCH
r_1 <- residuals(t.m1)

ggtsdisplay(r_1, plot.type = "partial")

ggtsdisplay(r_1^2, plot.type = "partial")

Box.test(r_1^2, lag = 12, type = "Ljung-Box")

library(fGarch)
m2.0 <- garchFit(~ garch(1,1), data = r_1, trace = FALSE)
summary(m2.0)
plot(m2.0)

# Pronóstico
horiz = 24
f_ipc <- forecast(t.m1, h = horiz)
plot(f_ipc)
###################################################TASA#########################



ggtsdisplay(tasa)

summary(t.m2 <- Arima(tasa, order = c(2,0,1),seasonal = c(1,0,1),include.mean = F,method="ML"))
coeftest(t.m1)
checkresiduals(t.m1) 

# Efecto ARCH
r_1 <- residuals(t.m1)

ggtsdisplay(r_1, plot.type = "partial")

ggtsdisplay(r_1^2, plot.type = "partial")

Box.test(r_1^2, lag = 12, type = "Ljung-Box")

m2.0 <- garchFit(~ garch(1,1), data = r_1, trace = FALSE)
summary(m2.0)
plot(m2.0)


# Dickey Fuller Test:
library(urca)
summary(ur.df(diff(Hurtos),type="trend",lags=13))
 # si p value < 0,05 rechazamos la hipotesis nula

perro = PP.test(Hurtos)

#################3
series2 = series1
series2$AÑO = NULL
series2$HURTOS.A.PERSONAS=ts(series2$HURTOS.A.PERSONAS,frequency = 12,start = 2013)
series2$TASA.DE.DESEMPLEO=ts(series2$TASA.DE.DESEMPLEO,frequency = 12,start = 2013)
plot.ts(series2)
library(vars)

VARselect(series2)
var.1 <- VAR(series2,2, type= "both", exogen = ,lag.max = 5,season = 12)

summary(var.1)
windows()
graphics.off()# Salida del ajuste
plot(var.1)

# Bondad de ajuste

(ser11 <- serial.test(var.1, lags.pt = 12, type = "PT.asymptotic")) # Autocorrelación
(norm1 <- normality.test(var.1, multivariate.only = T)) # Normalidad
(arch1 <- arch.test(var.1, lags.multi = 12)) # Efecto ARCH

stab1 <- stability(var.1) # Estabilidad
plot(stab1) # el CUMSUM tiene que mantenerse dentro de las bandas de confianza
fore <- predict(var.1, n.ahead = 12,frequency=12,start=2018)
windows()
plot(fore)



# load library
fit = nnetar(Hurtos,xreg = tasa,lambda=0.5)
fcast <- forecast(fit, xreg=tasa,PI=T,h=6)
autoplot(fcast)
fit2 = nnetar(tasa,lambada=0)
fcast2 <- forecast(fit2)

## Breakpoints

(break_point <- breakpoints(Hurtos~ 1))
plot(break_point)
summary(break_point)

break_date = breakdates(break_point)
win_1 <- subset(Hurtos, start = 1,end=36)
win_2 <- subset(Hurtos, start=37,end=55)
win_3 <- subset(Hurtos, start=56,end=65)

t.test(win_2,win_3) # TEST DIFERENCIA 
break_point$breakpoints

trend2 <- c(c(rep(0, break_point$breakpoints[1]),rep(0, break_point$breakpoints[2]-break_point$breakpoints[1]), 
              rep(46:64,1)))

trend1 <- c(c(rep(0, break_point$breakpoints[1]),rep(36:45,1), 
              rep(0, length(Hurtos) - break_point$breakpoints[2])))

level1 <- c(c(rep(0, break_point$breakpoints[1]),rep(1, break_point$breakpoints[2]-break_point$breakpoints[1]), 
              rep(0, length(Hurtos) - break_point$breakpoints[2])))

level2 <- c(c(rep(0, break_point$breakpoints[1]),rep(0, break_point$breakpoints[2]-break_point$breakpoints[1]), 
            rep(1, length(Hurtos) - break_point$breakpoints[2])))

reg=data.frame(level1,level2,trend1,trend2)

reg2=data.frame(level1,level2,trend2)

model1 <- Arima(Hurtos, order = c(1,0,0), 
                 seasonal = list(order = c(1,0,1), period = 12), 
                 xreg = reg, include.mean = T)


model2 <- Arima(Hurtos, order = c(1,0,0), 
                seasonal = list(order = c(1,0,1), period = 12), 
                xreg = reg2, include.mean = T)


library(vars)

VARselect(series2)
var.1 <- VAR(series2,2, type= "both", exogen = reg2,lag.max = 5,season = 12)

## the BIC chooses 5 breakpoints; plot the graph with breakdates and their confidence intervals
plot(bp.rice)
plot(Hurtos,main="Cambios Estructurales")
lines(bp.rice)
## confidence intervals
ci.rice <- confint(bp.rice)
ci.rice
lines(ci.rice)












































































































############### ARIMA CON BREAK POINTS ##################### # NOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO####################


Hurtos1 = as.numeric(Hurtos)
Hurtos1 = cbind(Hurtos, lag(Hurtos1, k = -1), lag(Hurtos1, k = -12),lag(as.numeric(t.m1$residuals),k=-1))


colnames(Hurtos1)<-c("y","ylag1","ylag12","ulag1")


## testing
re.seat <- efp(y ~ ylag1 + ylag12+ulag1, data = Hurtos1, type = "RE")
plot(re.seat)
## dating
bp.seat <- breakpoints(y ~ ylag1 + ylag12+ulag1, data = Hurtos1, h = 0.1)
summary(bp.seat)
lines(bp.seat, breaks = 2)


## minimum BIC partition
plot(bp.seat)
breakpoints(bp.seat)



## breakpoints and fit corresponding models
bp.seat2 <- breakpoints(bp.seat, breaks = 2)
fm0 <- lm(y ~ ylag1 + ylag12+ulag1, data = Hurtos1)
fm1 <- lm(y ~ breakfactor(bp.seat2)/(ylag1 + ylag12 + ulag1) - 2, data = Hurtos1)

summary(bp.seat2)

######################################## NOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO##################
