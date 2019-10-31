rm(list = ls())
ls()

library(astsa)
library(forecast)
library(fma)

# Import data

# Índice Accionario de Capitalización Bursatil, Bolsa de Valores de Colombia (daily 15/01/2008 - 22/06/2016)
data <- read.table("/Users/Santiago/Dropbox/Teaching/Time Series/data/colcap.txt", sep="\t", header=TRUE)
x <- ts(data[,2], start=c(2008,15,1), end=c(2016,22,6), frequency=256.25)

postscript("/Users/Santiago/Dropbox/Teaching/Time Series/Slides/colcap.eps",width = 15.5, height = 8.5, horizontal = TRUE, onefile = FALSE, paper = "a4")
par(mfrow=c(1,1), mar=c(2,4,0.2,0.2), las=1, cex.axis=1.5, cex.lab=1.5)
ts.plot(x, ylab="Index", xlab ="")
dev.off()

# Índice mensual de actividad económica -IMACO- (monthly 01/1992 - 05/2015)
data <- read.table("/Users/Santiago/Dropbox/Teaching/Time Series/data/imaco.txt", sep="\t", header=TRUE)
x <- ts(data[,3], start=c(1992,1), end=c(2015,5), frequency=12)

postscript("/Users/Santiago/Dropbox/Teaching/Time Series/Slides/imaco.eps",width = 15.5, height = 8.5, horizontal = TRUE, onefile = FALSE, paper = "a4")
par(mfrow=c(1,1), mar=c(2,4,0.2,0.2), las=1, cex.axis=1.5, cex.lab=1.5)
ts.plot(x, ylab="Index", xlab ="")
dev.off()

# Consumption Price Index (monthly jul/54 - may/16)
data <- read.table("/Users/Santiago/Dropbox/Teaching/Time Series/data/ipc.txt", sep="\t", header=TRUE)
x <- ts(data[,2], start=c(1954,7), end=c(2016,5), frequency=12)

postscript("/Users/Santiago/Dropbox/Teaching/Time Series/Slides/ipc.eps",width = 15.5, height = 8.5, horizontal = TRUE, onefile = FALSE, paper = "a4")
par(mfrow=c(1,1), mar=c(2,4,0.2,0.2), las=1, cex.axis=1.5, cex.lab=1.5)
ts.plot(x, ylab="Index", xlab ="")
dev.off()

# Industrial Production Index (monthly 01/1980 - 04/2016)
data <- read.table("/Users/Santiago/Dropbox/Teaching/Time Series/data/ipi.txt", sep="\t", header=TRUE)
x <- ts(data[,2], start=c(1980,1), end=c(2016,4), frequency=12)

postscript("/Users/Santiago/Dropbox/Teaching/Time Series/Slides/ipi.eps",width = 15.5, height = 8.5, horizontal = TRUE, onefile = FALSE, paper = "a4")
par(mfrow=c(1,1), mar=c(2,4,0.2,0.2), las=1, cex.axis=1.5, cex.lab=1.5)
ts.plot(x, ylab="Index", xlab ="")
dev.off()

# Gross Domestic Product (quarterly 2000Q1-2015Q4)
data <- read.table("/Users/Santiago/Dropbox/Teaching/Time Series/data/gdp.txt", sep="\t", header=TRUE)
x <- ts(data[,2]/1000, start=c(2000,1), end=c(2015,4), frequency=4)

postscript("/Users/Santiago/Dropbox/Teaching/Time Series/Slides/gdp.eps",width = 15.5, height = 8.5, horizontal = TRUE, onefile = FALSE, paper = "a4")
par(mfrow=c(1,1), mar=c(2,4.5,0.2,0.2), las=1, cex.axis=1.5, cex.lab=1.5)
ts.plot(x, ylab="Thousands of millions (pesos)", xlab ="")
dev.off()

# Exchange rate (pesos/dollar) (daily 27/11/1991-24/06/2016)
data <- read.table("/Users/Santiago/Dropbox/Teaching/Time Series/data/trm.txt", sep="\t", header=TRUE)
x <- ts(data[,2], start=c(1991,11), end=c(2016,6), frequency=359.24)

postscript("/Users/Santiago/Dropbox/Teaching/Time Series/Slides/trm.eps",width = 15.5, height = 8.5, horizontal = TRUE, onefile = FALSE, paper = "a4")
par(mfrow=c(1,2), mar=c(2,4,0.2,0.2), las=1, cex.axis=1.5, cex.lab=1.5)
ts.plot(x, ylab="", xlab ="")
ts.plot(diff(log(x)), ylab="", xlab ="")
dev.off()

# West Texas Intermediate - WTI - Crude Oil Price (1986 -2015). Source: http://www.eia.gov/
library(xlsx)

data_d <- read.xlsx("/Users/santiagogallon/Dropbox/Teaching/Time Series/data/wti.xls", header=TRUE, sheetIndex = 1)
data_w <- read.xlsx("/Users/santiagogallon/Dropbox/Teaching/Time Series/data/wti.xls", header=TRUE, sheetIndex = 2)
data_m <- read.xlsx("/Users/santiagogallon/Dropbox/Teaching/Time Series/data/wti.xls", header=TRUE, sheetIndex = 3)
data_y <- read.xlsx("/Users/santiagogallon/Dropbox/Teaching/Time Series/data/wti.xls", header=TRUE, sheetIndex = 4)

x_d <- ts(data_d[1:7568,2], start=c(1986,1,2), frequency=365)
x_w <- ts(data_w[1:1565,2], start=c(1986,1), end=c(2015,52), frequency=53)
x_m <- ts(data_m[1:360,2], start=c(1986,1), end=c(2015,12), frequency=12)
x_y <- ts(data_y[,2], start=c(1986), end=2015, frequency=1)

postscript("/Users/santiagogallon/Dropbox/Teaching/Time Series/Slides/wti.eps",width = 15.5, height = 8.5, horizontal = TRUE, onefile = FALSE, paper = "a4")
par(mfrow=c(2,2), mar=c(2,4.2,1.2,0.2), las=1, cex.axis=1.5, cex.lab=1.5)
ts.plot(x_d, main= "Daily", ylab="US dollars per barrel", xlab ="")
ts.plot(x_w, main= "Weekly", ylab="US dollars per barrel", xlab ="")
ts.plot(x_m, main= "Monthly", ylab="US dollars per barrel", xlab ="")
ts.plot(x_y, main= "Yearly", ylab="US dollars per barrel", xlab ="")
dev.off()

# Monthly totals (in thousands) of international airline passengers between 1949 and 1960. Source: Box-Jenkins
postscript("/Users/santiagogallon/Dropbox/Teaching/Time Series/Slides/air.eps",width = 15.5, height = 8.5, horizontal = TRUE, onefile = FALSE, paper = "a4")
par(mfrow=c(1,1), mar = c(2,5.4,0.2,0.2), las=1, cex.axis=1.5, cex.lab=1.5)
ts.plot(AirPassengers, ylab="", xlab="")
title(ylab="No. of passengers (in thousands)", line=4)
dev.off()

# L.A. Pollution Study. The scales are different, mortality, temperature, and emissions (weekly 1970 - 1980)
postscript("/Users/santiagogallon/Dropbox/Teaching/Time Series/Slides/pollu.eps",width = 15.5, height = 8.5, horizontal = TRUE, onefile = FALSE, paper = "a4")
par(mfrow=c(3,1), mar=c(0,4.8,0,0.2), oma=c(4,0,0.2,0), las=1, cex.axis=1.5, cex.lab=1.5, tcl=-.3)
plot(cmort, ylab="No. of Deaths", xaxt="no", type='n')
#grid(lty=1, col=gray(.9))
lines(cmort, col="blue")
#text(1974, 132, 'Bad Year', col=rgb(.5,0,.5), cex=1.25)   # just for fun
#arrows(1973.5, 130, 1973, 127, length=0.05,  angle=30, col=rgb(.5,0,.5))   
plot(tempr, ylab=expression(~Temperature~(degree~F)), xaxt="no", type='n')
#grid(lty=1, col=gray(.9))
lines(tempr, col="red")
plot(part, ylab="Emissions (PPM)")
#grid(lty=1, col=gray(.9))
title(xlab="Time (week)", outer=TRUE)
dev.off()

# Seasonal series
postscript("/Users/Santiago/Dropbox/Teaching/Time Series/Slides/seas.eps",width = 15.5, height = 8.5, horizontal = TRUE, onefile = FALSE, paper = "a4")
par(mfrow=c(3,2), mar=c(4,4.3,0.7,0.7), las=0, cex.axis=1.5, cex.lab=1.5, tcl=-.3)
plot(gas, ylab="AU monthly gas production")      # Australian monthly gas production: 1956–1995
plot(taylor, ylab="UK half-hourly electricity demand")   # Half-hourly electricity demand in England and Wales from Monday 5 June 2000 to Sunday 27 August 2000
plot(wineind, ylab="AU monthly wine sales")  # Australian total wine sales by wine makers in bottles <= 1 litre. Jan 1980 – Aug 1994
plot(USAccDeaths, ylab="US monthly accidental deaths") # Accidental Deaths in the US 1973-1978
plot(milk, ylab="Monthly milk production") #Monthly milk production per cow
plot(part, ylab="LA weekly particulate levels") # Particulate levels from the LA pollution study
#plot(birth, ylab="US Monthly live births") # Monthly live births (adjusted) in thousands for the United States, 1948-1979.
#plot(woolyrnq) # Quarterly production of woollen yarn in Australia: tonnes. Mar 1965 – Sep 1994
#plot(ldeaths)
#plot(unemp)# Monthly U.S. Unemployment series 1948-1978
dev.off()

# Simulated AR(1) processes
x_1 <- arima.sim(list(order=c(1,0,0), ar= 0.8), n=250)
x_2 <- arima.sim(list(order=c(1,0,0), ar=-0.8), n=250)

postscript("/Users/Santiago/Dropbox/Teaching/Time Series/Slides/ar_1.eps",width = 15.5, height = 8.5, horizontal = TRUE, onefile = FALSE, paper = "a4")
par(mfcol=c(3,2), mar=c(4.2,4.5,1.5,0.2), las=1, cex.axis=1.5, cex.lab=1.5)
ts.plot(x_1, ylab=expression(X[t]), main=(expression(AR(1)~~~phi==0.8)))
Acf(x_1, main="", xlab=expression(k))
Pacf(x_1, main="", ylab="PACF", xlab=expression(k))
#
ts.plot(x_2, ylab=expression(X[t]), main=(expression(AR(1)~~~phi==-0.8)))
Acf(x_2, main="", xlab=expression(k))
Pacf(x_2, main="", ylab="PACF", xlab=expression(k))
#
dev.off()

# Simulated AR(2) processes
x_1 <- arima.sim(list(order=c(2,0,0), ar=c(0.5,-0.8)), n=250)
x_2 <- arima.sim(list(order=c(2,0,0), ar=c(0.6,0.3)),  n=250)

postscript("/Users/Santiago/Dropbox/Teaching/Time Series/Slides/ar_2.eps",width = 15.5, height = 8.5, horizontal = TRUE, onefile = FALSE, paper = "a4")
par(mfcol=c(3,2), mar=c(4.2,4.5,1.5,0.2), las=1, cex.axis=1.5, cex.lab=1.5)
ts.plot(x_1, ylab=expression(X[t]), main=(expression(AR(2)~~~phi[1]==0.5~~phi[2]==-0.8)))
Acf(x_1, main="", xlab=expression(k))
Pacf(x_1, main="", ylab="PACF", xlab=expression(k))
#
ts.plot(x_2, ylab=expression(X[t]), main=(expression(AR(2)~~~phi[1]==0.6~~phi[2]==0.3)))
Acf(x_2, main="", xlab=expression(k))
Pacf(x_2, main="", ylab="PACF", xlab=expression(k))
#
dev.off()

# Stationary regions for an AR(2) process
postscript("/Users/Santiago/Dropbox/Teaching/Time Series/Slides/roots.eps",width = 15.5, height = 8.5, horizontal = TRUE, onefile = FALSE, paper = "a4")
par(mfcol=c(1,1), mar=c(4,4.4,0.5,0.2), las=1, cex.axis=1.5, cex.lab=1.5)
curve(1+x, -2,2, ylim=c(-0.929,0.929), xlab=expression(phi[1]), ylab=expression(phi[2]))
curve(1-x, -2,2, add=TRUE)
curve((-x^2)/4, -2,2, add=TRUE)
text(0,.4,"Real roots", cex=1.5)
text(0,.32,expression(abs(lambda[1])<1~~~abs(lambda[2])<1), cex=1.5)
text(-1.05,.4,expression(phi[2]==1+phi[1]), cex=1.5)
arrows(-0.8,0.4, -0.6,0.4, length=0.1, angle=15)
text(1.05,.4,expression(phi[2]==1-phi[1]), cex=1.5)
arrows(0.8,0.4, 0.6,0.4, length=0.1, angle=15)
text(0, -.5,"Complex roots", cex=1.5)
text(0,-0.59,expression(abs(lambda[1])<1~~~abs(lambda[2])<1), cex=1.5)
text(0,-0.13,expression(phi[1]^2+4*phi[2]==0), cex=1.5)
arrows(0,-0.08, 0,0, length=0.1, angle=15)
abline(0.0015,0, col="gray70", lty=2)
dev.off()

# Simulated MA(1) processes
x_1 <- arima.sim(list(order=c(0,0,1), ma= 0.8), n=250)
x_2 <- arima.sim(list(order=c(0,0,1), ma=-0.8), n=250)

postscript("/Users/Santiago/Dropbox/Teaching/Time Series/Slides/ma_1.eps",width = 15.5, height = 8.5, horizontal = TRUE, onefile = FALSE, paper = "a4")
par(mfcol=c(3,2), mar=c(4.2,4.5,1.5,0.2), las=1, cex.axis=1.5, cex.lab=1.5)
ts.plot(x_1, ylab=expression(X[t]), main=(expression(MA(1)~~~theta==0.8)))
Acf(x_1, main="", xlab=expression(k))
Pacf(x_1, main="", ylab="PACF", xlab=expression(k))
#
ts.plot(x_2, ylab=expression(X[t]), main=(expression(MA(1)~~~theta==-0.8)))
Acf(x_2, main="", xlab=expression(k))
Pacf(x_2, main="", ylab="PACF", xlab=expression(k))
#
dev.off()

# Simulated MA(2) processes
x_1 <- arima.sim(list(order=c(0,0,2), ma=c(0.3,0.3)), n=250)
x_2 <- arima.sim(list(order=c(0,0,2), ma=c(0.6,-0.4)),  n=250)

postscript("/Users/Santiago/Dropbox/Teaching/Time Series/Slides/ma_2.eps",width = 15.5, height = 8.5, horizontal = TRUE, onefile = FALSE, paper = "a4")
par(mfcol=c(3,2), mar=c(4.2,4.5,1.5,0.2), las=1, cex.axis=1.5, cex.lab=1.5)
ts.plot(x_1, ylab=expression(X[t]), main=(expression(MA(2)~~~theta[1]==0.3~~theta[2]==0.3)))
Acf(x_1, main="", xlab=expression(k))
Pacf(x_1, main="", ylab="PACF", xlab=expression(k))
#
ts.plot(x_2, ylab=expression(X[t]), main=(expression(MA(2)~~~theta[1]==0.6~~theta[2]==-0.4)))
Acf(x_2, main="", xlab=expression(k))
Pacf(x_2, main="", ylab="PACF", xlab=expression(k))
#
dev.off()

# Simulated ARMA(1,1) processes
x_1 <- arima.sim(list(order=c(1,0,1), ar=0.8, ma=0.6), n=250)
x_2 <- arima.sim(list(order=c(1,0,1), ar=0.9, ma=-0.5),  n=250) # ar=0.9, ma=-0.5

postscript("/Users/Santiago/Dropbox/Teaching/Time Series/Slides/arma_1_1.eps",width = 15.5, height = 8.5, horizontal = TRUE, onefile = FALSE, paper = "a4")
par(mfcol=c(3,2), mar=c(4.2,4.5,1.5,0.2), las=1, cex.axis=1.5, cex.lab=1.5)
ts.plot(x_1, ylab=expression(X[t]), main=(expression(ARMA(1,1)~~~phi[1]==0.8~~theta[1]==0.6)))
Acf(x_1, main="", xlab=expression(k))
Pacf(x_1, main="", ylab="PACF", xlab=expression(k))
#
ts.plot(x_2, ylab=expression(X[t]), main=(expression(ARMA(1,1)~~~phi[1]==0.9~~theta[1]==-0.5)))
Acf(x_2, main="", xlab=expression(k))
Pacf(x_2, main="", ylab="PACF", xlab=expression(k))
#
dev.off()

# Stationary AR(1) and Random walk processes with drift alpha
n <- 250
delta <- 0.5
phi <- 0.9
#
x <- y <- z <- numeric(n)
t <- seq(1,n,1)
for(i in 2:n){
  x[i] <- delta + x[i-1] + rnorm(1,0,1)
  y[i] <- delta + phi*y[i-1] + rnorm(1,0,1)
}

# Deterministic trend
for(i in 1:n){
  z[i] <- delta*t[i] + rnorm(1,0,2)
}

postscript("/Users/Santiago/Dropbox/Teaching/Time Series/Slides/rw.eps",width = 15.5, height = 8.5, horizontal = TRUE, onefile = FALSE, paper = "a4")
par(mfcol=c(1,1), mar=c(4.2,3.3,0.2,0.5), las=1, cex.axis=1.5, cex.lab=1.5)
ts.plot(x, ylab="", main="")
lines(delta*t, lty=2, col ="gray70")
text(190,107,expression(E(Y[t])==delta~t), cex=1.1)
arrows(194,105, 200,101, length=0.1, angle=15)
lines(y, lty=5)
abline(h=mean(y), lty=2, col ="gray70")
text(110,12,expression(E(X[t])==delta/(1-phi[1])), cex=1.1)
arrows(106,10, 110,6, length=0.1, angle=15)
legend("topleft",title="",legend=c("Random Walk with Drift","Stationary AR(1) with Drift"), lty=c(1,5), cex=1.2, bty="n")
dev.off()

postscript("/Users/Santiago/Dropbox/Teaching/Time Series/Slides/acf_rw.eps",width = 15.5, height = 8.5, horizontal = TRUE, onefile = FALSE, paper = "a4")
par(mfcol=c(1,2), mar=c(4.2,4.5,3,0.5), las=1, cex.axis=1.5, cex.lab=1.5)
Acf(x, ylab="ACF", xlab=expression(k), main="Random Walk with Drift")
Acf(y, ylab="ACF", xlab=expression(k), main="Stationary AR(1) with Drift", ylim=c(-0.2,1))
dev.off()

postscript("/Users/Santiago/Dropbox/Teaching/Time Series/Slides/dt.eps",width = 15.5, height = 8.5, horizontal = TRUE, onefile = FALSE, paper = "a4")
par(mfcol=c(1,2), mar=c(4.2,4.5,1.5,0.3), las=1, cex.axis=1.5, cex.lab=1.5)
ts.plot(z, ylab=expression(X[t]), main="Trend-stationary")
lines(predict(lm(z~t-1)), col=2)
ts.plot(residuals(lm(z~t-1)), ylab=expression(X[t]-DT[t]), main="Detrending")
dev.off()

summary(lm(z~t-1))

postscript("/Users/Santiago/Dropbox/Teaching/Time Series/Slides/rw_ts.eps",width = 15.5, height = 8.5, horizontal = TRUE, onefile = FALSE, paper = "a4")
par(mfcol=c(1,2), mar=c(4.2,4.5,1.5,0.3), las=1, cex.axis=1.5, cex.lab=1.5)
ts.plot(z, ylab=expression(X[t]), main="Trend-stationary")
ts.plot(x, ylab="", main="Random walk with drift")
dev.off()

postscript("/Users/Santiago/Dropbox/Teaching/Time Series/Slides/rw_diff.eps",width = 15.5, height = 8.5, horizontal = TRUE, onefile = FALSE, paper = "a4")
par(mfcol=c(1,2), mar=c(4.2,4.5,1.5,0.5), las=1, cex.axis=1.5, cex.lab=1.5)
ts.plot(x, ylab=(expression(X[t])), main="Random Walk with Drift")
ts.plot(diff(x), ylab=(expression(Delta~X[t])), main="First difference")
dev.off()

postscript("/Users/Santiago/Dropbox/Teaching/Time Series/Slides/acf_rw_diff.eps",width = 15.5, height = 8.5, horizontal = TRUE, onefile = FALSE, paper = "a4")
par(mfcol=c(1,2), mar=c(4.2,4.5,3,0.5), las=1, cex.axis=1.5, cex.lab=1.5)
Acf(x, ylab="ACF", xlab=expression(k), main="Random Walk with Drift")
Acf(diff(x), ylab="ACF", xlab=expression(k), main="First difference", ylim=c(-0.2,1))
dev.off()

# x <- arima.sim(list(order=c(1,0,0), ar=0.9), mean=0.5, n=250)
# y <- arima.sim(list(order=c(0,1,0)), mean=0.5, n=250)
# y <- 1:101 * b +arima.sim(list(order=c(1,1,0), ar=0), n=250)

# Simulated ARIMA(1,1,1) processes
x_1 <- arima.sim(list(order=c(1,1,0), ar=0.8), n=250)           # ARIMA(1,1,0) or ARI(1,1)
x_2 <- arima.sim(list(order=c(0,1,1), ma=0.75), n=250)          # ARIMA(0,1,1) or IMA(1,1)
x_3 <- arima.sim(list(order=c(1,1,1), ar=0.9, ma=0.5), n=250)   # ARIMA(1,1,1)

postscript("/Users/Santiago/Dropbox/Teaching/Time Series/Slides/arima_1_1_1.eps",width = 15.5, height = 8.5, horizontal = TRUE, onefile = FALSE, paper = "a4")
par(mfcol=c(3,3), mar=c(4.2,4.5,1.5,0.2), las=1, cex.axis=1.5, cex.lab=1.5)
ts.plot(x_1, ylab=expression(X[t]), main=(expression(ARIMA(1,1,0)~~~phi[1]==0.8)))
Acf(x_1, main="", xlab=expression(k))
Pacf(x_1, main="", ylab="PACF", xlab=expression(k))
#
ts.plot(x_2, ylab=expression(X[t]), main=(expression(ARIMA(0,1,1)~~~theta[1]==0.75)))
Acf(x_2, main="", xlab=expression(k))
Pacf(x_2, main="", ylab="PACF", xlab=expression(k))
#
ts.plot(x_3, ylab=expression(X[t]), main=(expression(ARIMA(1,1,1)~~~phi[1]==0.9~~theta[1]==0.5)))
Acf(x_3, main="", xlab=expression(k))
Pacf(x_3, main="", ylab="PACF", xlab=expression(k))
#
dev.off()

postscript("/Users/Santiago/Dropbox/Teaching/Time Series/Slides/diff_arima_1_1_1.eps",width = 15.5, height = 8.5, horizontal = TRUE, onefile = FALSE, paper = "a4")
par(mfcol=c(3,3), mar=c(4.2,4.5,1.5,0.2), las=1, cex.axis=1.5, cex.lab=1.5)
ts.plot(diff(x_1), ylab=expression(Delta~X[t]), main=(expression(Delta~X[t]~~of~~ARIMA(1,1,0)~~~phi[1]==0.8)))
Acf(diff(x_1), main="", xlab=expression(k))
Pacf(diff(x_1), main="", ylab="PACF", xlab=expression(k))
#
ts.plot(diff(x_2), ylab=expression(Delta~X[t]), main=(expression(Delta~X[t]~~of~~ARIMA(0,1,1)~~~theta[1]==0.75)))
Acf(diff(x_2), main="", xlab=expression(k))
Pacf(diff(x_2), main="", ylab="PACF", xlab=expression(k))
#
ts.plot(diff(x_3), ylab=expression(Delta~X[t]), main=(expression(Delta~X[t]~~of~~ARIMA(1,1,1)~~~phi[1]==0.9~~theta[1]==0.5)))
Acf(diff(x_3), main="", xlab=expression(k))
Pacf(diff(x_3), main="", ylab="PACF", xlab=expression(k))
#
dev.off()

# Seasonal Processes

# Seasonal AR(1) model
Phi <- c(rep(0,11),0.9)
sAR <- arima.sim(list(order=c(12,0,0), ar=Phi), n=37)
sAR <- ts(sAR, freq=12)

dev.off()
postscript("/Users/Santiago/Dropbox/Teaching/Time Series/Slides/sar_1.eps",width = 15.5, height = 8.5, horizontal = TRUE, onefile = FALSE, paper = "a4")
layout(matrix(c(1,2, 1,3), nc=2))
par(mar=c(3,3.5,2,0.2), mgp=c(1.6,.6,0), las=0, cex.axis=1.5, cex.lab=1.5)
plot(sAR, axes=FALSE, main="Seasonal AR(1)", ylab=expression(X[t]), xlab="year", type="c")
months = c("J","F","M","A","M","J","J","A","S","O","N","D")
points(sAR, pch=months, cex=1, font=1, col=1:12)
axis(1, 1:4)
abline(v=1:4, lty=2, col=gray(.6))
axis(2)
box()
ACF  <- ARMAacf(ar=Phi, ma=0, 100)[-1]
PACF <- ARMAacf(ar=Phi, ma=0, 100, pacf=TRUE)
plot(ACF, axes=FALSE, type="h", xlab=expression(k), ylim=c(-1,1))
axis(1, seq(0,96,12))
axis(2)
abline(h=0)
box()
plot(PACF, axes=FALSE, type="h", xlab=expression(k), ylim=c(-1,1))
axis(1, seq(0,96,12))
axis(2)
abline(h=0)
box()
dev.off()

# Seasonal MA(1) model
Theta <- c(rep(0,11),0.5)
sMA <- arima.sim(list(order=c(0,0,12), ma=Theta), n=37)
sMA <- ts(sMA, freq=12)

postscript("/Users/Santiago/Dropbox/Teaching/Time Series/Slides/sma_1.eps",width = 15.5, height = 8.5, horizontal = TRUE, onefile = FALSE, paper = "a4")
layout(matrix(c(1,2, 1,3), nc=2))
par(mar=c(3,3.5,2,0.2), mgp=c(1.6,.6,0), las=0, cex.axis=1.5, cex.lab=1.5)
plot(sMA, axes=FALSE, main="Seasonal MA(1)", ylab=expression(X[t]), xlab="year", type="c")
months = c("J","F","M","A","M","J","J","A","S","O","N","D")
points(sMA, pch=months, cex=1, font=1, col=1:12)
axis(1, 1:4)
abline(v=1:4, lty=2, col=gray(.6))
axis(2)
box()
ACF  <- ARMAacf(ar=0, ma=Theta, 100)[-1]
PACF <- ARMAacf(ar=0, ma=Theta, 100, pacf=TRUE)
plot(ACF, axes=FALSE, type="h", xlab=expression(k), ylim=c(-1,1))
axis(1, seq(0,96,12))
axis(2)
abline(h=0)
box()
plot(PACF, axes=FALSE, type="h", xlab=expression(k), ylim=c(-1,1))
axis(1, seq(0,96,12))
axis(2)
abline(h=0)
box()
dev.off()

# Seasonal ARMA(1,1) model
Phi <- c(rep(0,11),0.8)
Theta <- c(rep(0,11),-0.5)
sARMA <- arima.sim(list(order=c(12,0,12), ar=Phi, ma=Theta), n=37)
sARMA <- ts(sARMA, freq=12)

postscript("/Users/Santiago/Dropbox/Teaching/Time Series/Slides/sarma_1_1.eps",width = 15.5, height = 8.5, horizontal = TRUE, onefile = FALSE, paper = "a4")
layout(matrix(c(1,2, 1,3), nc=2))
par(mar=c(3,3.5,2,0.2), mgp=c(1.6,.6,0), las=0, cex.axis=1.5, cex.lab=1.5)
plot(sARMA, axes=FALSE, main="Seasonal ARMA(1,1)", ylab=expression(X[t]), xlab="year", type="c")
months = c("J","F","M","A","M","J","J","A","S","O","N","D")
points(sARMA, pch=months, cex=1, font=1, col=1:12)
axis(1, 1:4)
abline(v=1:4, lty=2, col=gray(.6))
axis(2)
box()
ACF  <- ARMAacf(ar=Phi, ma=Theta, 100)[-1]
PACF <- ARMAacf(ar=Phi, ma=Theta, 100, pacf=TRUE)
plot(ACF, axes=FALSE, type="h", xlab=expression(k), ylim=c(-1,1))
axis(1, seq(0,96,12))
axis(2)
abline(h=0)
box()
plot(PACF, axes=FALSE, type="h", xlab=expression(k), ylim=c(-1,1))
axis(1, seq(0,96,12))
axis(2)
abline(h=0)
box()
dev.off()

# Seasonal Multiplicative ARMA(0,1)x(1,0)_12
Phi <- c(rep(0,11),0.8)
theta <- -0.5
smARMA <- arima.sim(list(order=c(12,0,1), ar=Phi, ma=theta), n=37)
smARMA <- ts(smARMA, freq=12)

postscript("/Users/Santiago/Dropbox/Teaching/Time Series/Slides/sarma_1_1_1_0.eps",width = 15.5, height = 8.5, horizontal = TRUE, onefile = FALSE, paper = "a4")
layout(matrix(c(1,2, 1,3), nc=2))
par(mar=c(3,3.5,2,0.2), mgp=c(1.6,.6,0), las=0, cex.axis=1.5, cex.lab=1.5)
plot(smARMA, axes=FALSE, main="Seasonal Multiplicative ARMA(0,1)x(1,0)_12", ylab=expression(X[t]), xlab="year", type="c")
months = c("J","F","M","A","M","J","J","A","S","O","N","D")
points(smARMA, pch=months, cex=1, font=1, col=1:12)
axis(1, 1:4)
abline(v=1:4, lty=2, col=gray(.6))
axis(2)
box()
ACF  <- ARMAacf(ar=Phi, ma=theta, 100)[-1]
PACF <- ARMAacf(ar=Phi, ma=theta, 100, pacf=TRUE)
plot(ACF, axes=FALSE, type="h", xlab=expression(k), ylim=c(-1,1))
axis(1, seq(0,96,12))
axis(2)
abline(h=0)
box()
plot(PACF, axes=FALSE, type="h", xlab=expression(k), ylim=c(-1,1))
axis(1, seq(0,96,12))
axis(2)
abline(h=0)
box()
dev.off()

# Deterministic Seasonallity
n <- 160
x <- numeric(n)
t <- ts(seq(1,n,1), freq=4)
S <- seasonaldummy(t)
s4 <- 1-rowSums(S)
S <- cbind(S,s4)

for(i in 1:n){
  x[i] <- 6*S[i,1] + 8*S[i,2] - 4*S[i,3] + 5*S[i,4] + rnorm(1,0,2)
}

postscript("/Users/Santiago/Dropbox/Teaching/Time Series/Slides/ds.eps",width = 15.5, height = 8.5, horizontal = TRUE, onefile = FALSE, paper = "a4")
par(mfrow=c(2,2), mar=c(4.2,4.5,2.7,0.3), las=1, cex.axis=1.5, cex.lab=1.5)
ts.plot(x, main=expression(X[t]), ylab="")
lines(predict(lm(x~S-1)), col=2)
ts.plot(residuals(lm(x~t+S-1)), main=expression(X[t]-sum(hat(beta)[j]~D[jt], j==1, s)), ylab="")
Acf(x, ylab="ACF", xlab=expression(k), main="", ylim=c(-1,1))
Acf(residuals(lm(x~t+S-1)), ylab="ACF", xlab=expression(k), main="", ylim=c(-1,1))
dev.off()

# Seasonal Multiplicative ARIMA(0,1,1)x(0,1,1)_12
library(forecast)

model <- Arima(ts(rnorm(100),freq=12), order=c(0,1,1), seasonal=c(0,1,1), fixed=c(theta=-0.4, Theta=-0.6))
smARIMA <- simulate(model, nsim=48)
smARIMA <- ts(smARIMA[9:44], freq=12)

dev.off()
layout(matrix(c(1,2, 1,3), nc=2))
par(mar=c(3,3.5,2,0.2), mgp=c(1.6,.6,0), las=0, cex.axis=1.5, cex.lab=1.5)
plot(smARIMA, axes=FALSE, main="Seasonal Multiplicative ARMA(0,1)x(1,0)_12", ylab=expression(X[t]), xlab="year", type="c")
months = c("J","F","M","A","M","J","J","A","S","O","N","D")
points(smARIMA, pch=months, cex=1, font=1, col=1:12)
axis(1, 1:4)
abline(v=1:4, lty=2, col=gray(.6))
axis(2)
box()
Acf(smARIMA, main="", xlab=expression(k), ylim=c(-1,1))
Pacf(smARIMA, main="", ylab="PACF", xlab=expression(k), ylim=c(-1,1))

# Also the "sarima" package can be used
library(sarima)
sAR     <-sim_sarima(n=48, model=list(sar=0.9, nseasons=12)) # SAR(1)
sMA     <-sim_sarima(n=48, model=list(sma=0.5, nseasons=12))  # SMA(1)
sARMA   <-sim_sarima(n=48, model=list(sar=0.8, sma=-0.5, nseasons=12)) # SARMA(1,1)
smARMA  <-sim_sarima(n=48, model=list(ma=-0.5, sar=0.8, nseasons=12)) # Seasonal Multiplicative ARMA(0,1)x(1,0)_12
smARIMA <- sim_sarima(n=48, model=list(ma=-0.4, iorder=1, siorder=1, sma=-0.6, nseasons=12)) # Seasonal Multiplicative ARIMA(0,1,1)x(0,1,1)_12

plot(smARIMA[13:48], axes=FALSE, main="Seasonal AR(1)", ylab=expression(X[t]), xlab="time", type="c")
months = c("J","F","M","A","M","J","J","A","S","O","N","D")
points(smARIMA[13:48], pch=months, cex=1, font=1, col=1:12)
axis(1, 0:36)
abline(v=c(12,24,36), lty=2, col=gray(.6))
axis(2)
box()

# Spurious Regression
# Le be two random walk processes
n <- 250
#
x <- y <- numeric(n)
t <- seq(1,n,1)
for(i in 2:n){
  x[i] <- x[i-1] + rnorm(1,0,1)
  y[i] <- y[i-1] + rnorm(1,0,1)
}


postscript("/Users/Santiago/Dropbox/Teaching/Time Series/Slides/spurious.eps",width = 15.5, height = 8.5, horizontal = TRUE, onefile = FALSE, paper = "a4")
par(mfrow=c(2,2), mar=c(4,4.2,1,0.2), las=1, cex.axis=1.5, cex.lab=1.5)
ts.plot(y, main=expression(y[t]==y[t-1]+u[t]), ylab="", xlab="")
ts.plot(x, main=expression(x[t]==x[t-1]+v[t]), ylab="", xlab="")
plot(x,y, xlab = expression(x[t]), ylab=expression(y[t]))
abline(lm(y~x), col="red")
ts.plot(residuals(lm(y~x)), main=expression(epsilon[t]==y[t]+2.6917-0.5055*x[t]), ylab="", xlab="")
dev.off()


#fit <- Arima(foo, order=c(0,1,1), seasonal=c(0,1,1))
#model <- Arima(ts(rnorm(100),freq=4), order=c(1,1,1), seasonal=c(1,1,1), fixed=c(phi=0.5, theta=-0.4, Phi=0.3, Theta=-0.2))
# http://stackoverflow.com/questions/20273104/simulating-a-basic-sarima-model-in-r
#library(gmwm)
# Specify a SARIMA(2,1,1)(1,1,1)[12] 
#mod = SARIMA(ar=c(.3,.5), i=1, ma=.1, sar=.2, si = 1, sma = .4, s = 12, sigma2 = 1.5)
# Generate the data
#xt2 = gen.gts(mod, 1e3)

