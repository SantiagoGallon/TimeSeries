rm(list = ls())
ls()

library(astsa)
library(forecast)
library(fma)

# Seasonal Processes

# Seasonal AR(2)_12 model
Phi <- c(rep(0,11),-0.5,rep(0,11),0.4)
sAR <- arima.sim(list(order=c(24,0,0), ar=Phi), n=37)
sAR <- ts(sAR, freq=12)

postscript("/Users/Santiago/Dropbox/Teaching/Time Series/Parciales/sar_2.eps",width = 15.5, height = 8.5, horizontal = TRUE, onefile = FALSE, paper = "a4")
par(mfrow=c(1,2), mar=c(4.2,4.2,0.5,0.3), las=1, cex.axis=1.5, cex.lab=1.5)
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
Theta <- c(rep(0,11),-0.9)
sMA <- arima.sim(list(order=c(0,0,12), ma=Theta), n=37)
sMA <- ts(sMA, freq=12)

postscript("/Users/Santiago/Dropbox/Teaching/Time Series/Parciales/sma_1.eps",width = 15.5, height = 8.5, horizontal = TRUE, onefile = FALSE, paper = "a4")
par(mfrow=c(1,2), mar=c(4.2,4.2,0.5,0.3), las=1, cex.axis=1.5, cex.lab=1.5)
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

# Seasonal Multiplicative ARMA(0,1)x(1,0)_12
Phi <- c(rep(0,11),0.8)
theta <- -0.5
smARMA <- arima.sim(list(order=c(12,0,1), ar=Phi, ma=theta), n=37)
smARMA <- ts(smARMA, freq=12)

postscript("/Users/Santiago/Dropbox/Teaching/Time Series/Parciales/sarma_1_1_1_0.eps",width = 15.5, height = 8.5, horizontal = TRUE, onefile = FALSE, paper = "a4")
par(mfrow=c(1,2), mar=c(4.2,4.2,0.5,0.3), las=1, cex.axis=1.5, cex.lab=1.5)
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
