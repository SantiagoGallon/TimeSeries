rm(list = ls())
ls()

# Install and load the "easypackages" to load and install multiple packages
install.packages("easypackages")
library(easypackages)

# Intalling and loading multiple packages at once (using the "easypackages" package)
install_packages("astsa", "TSA", "forecast", "urca", "moments", "normtest", "fUnitRoots", "tseries")
libraries("astsa", "TSA", "forecast", "urca", "moments", "normtest", "tseries")

# Loading packages one at the time
library(astsa)        # Applied Statistical Time Series Analysis
library(TSA)          # Time Series Analysis
library(forecast)     # Forecasting Functions for Time Series and Linear Models
library(urca)         # Unit Root and Cointegration Tests for Time Series Data
library(moments)      # Moments, cumulants, skewness, kurtosis and related tests
library(normtest)     # Tests for Normality
library(vioplot)      # Violin plots
library(fUnitRoots)  # Trends and Unit Roots
library(tseries)
# library(fGarch)

# Time series analysis of the seasonally adjusted US Gross Domestic Product -GDP- (real billions of 1996, quarterly 1947Q1-2002Q3)
# Source: Federal Reserve Bank of St. Louis (http://research.stlouisfed.org/)
x <- gnp

save(x, file="/Users/santiago/Dropbox/Teaching/Time Series/data/pib.RData") # Save data in R format
write.table(x, file="/Users/santiago/Dropbox/Teaching/Time Series/data/pib.txt",sep="\t") # Save data in text format
load("/Users/santiago/Dropbox/Teaching/Time Series/data/pib.RData") # Load data in R format 
x <- read.table("/Users/santiago/Dropbox/Teaching/Time Series/data/pib.txt", header=TRUE, sep="\t") # Read a data frame
x <- ts(x, start = c(1947,1), end = c(2002,3), frequency = 4) # Create time-series objects
###################### Descriptive Statistics ######################
summary(x) # mean, median, 25th and 75th quartiles, min, max
mean(x)     # mean
median(x)   # median
var(x)      # variance
sd(x)       # standar deviation
min(x)      # minimum
max(x)      # maximum
quantile(x, probs = seq(0, 1, 0.25), names = TRUE, type = 7) # quartiles

# Skewness and curtosis (implemented in the "moments" package)
skewness(x) # skewness
kurtosis(x) # curtosis

# Histogram and box-plots
hist(x, prob=TRUE, xlab="GDP", main="")
lines(density(x), xlim=range(x), col=2)
par(mfcol=c(1,2))
boxplot(x, ylab="GDP", horizontal = FALSE)
vioplot(x, ylab="GDP", names="", col=gray(0.7), pchMed=20, colMed = "white" , border="black", rectCol = 1)
par(mfcol=c(1,2))
boxplot(x, xlab="GDP", horizontal = TRUE)
vioplot(x, names="", col=gray(0.7), horizontal = TRUE)
# Portmanteau test for no autocorrelation in the data
Box.test(x, lag=20, type = "Ljung-Box", fitdf = 0)

########################## Stationarity ############################
# Plotting data
ts.plot(x, xlab="", ylab="GDP", main="")
ts.plot(log(x), xlab="", ylab="GDP", main="") # log scale

# ACF of the data (is the series stationary?) using the Acf() function in the "forecast" package
Acf(x, main="")

# Difference-stationary to achieve stationarity in mean and variance
par(mfcol=c(1,2))
ts.plot(diff(x),      xlab="", ylab="First difference of the GDP", main="")   # First difference of the data
ts.plot(diff(log(x)), xlab="", ylab="GDP growth rate", main="")               # GDP growth rate (i.e. first difference of the log(GDP)

par(mfcol=c(1,2))
Acf(x, main="GDP")
Acf(diff(log(x)), main="GDP growth rate ")

####################### Model Identification #######################
# ACF and PACF of the GDP growth rate (model identification)
par(mfcol=c(1,2))
Acf(diff(log(x)),  main="", ylim=c(-1,1))
Pacf(diff(log(x)), main="", ylim=c(-1,1))

######################### Model Estimation #########################
# Estimation of tentative models. Note: the arima() function does not include the constant term in the models if d is different of zero
fit_1 <- arima(log(x), order = c(1,1,0)) # log_x ~ ARIMA(1,1,0) = ARI(1,1)
fit_2 <- arima(log(x), order = c(0,1,2)) # log_x ~ ARIMA(0,1,2) = IMA(1,2)
summary(fit_1)
summary(fit_2)

# To include the intercept using the arima() function, then:
fit_1 <- arima(diff(log(x)), order = c(1,0,0), include.mean = TRUE) # ARMA(1,0,0) on the first difference of the log(GDP)
fit_2 <- arima(diff(log(x)), order = c(0,0,2), include.mean = TRUE) # ARMA(0,0,2) on the first difference of the log(GDP)
summary(fit_1)
summary(fit_2)

# # The same models but including the constant (drift) term. NOTE: use the sarima() function in the "astsa" package
# fit_1 <- sarima(log(x), p=1, d=1, q=0, no.constant=FALSE)  # ARIMA(1,1,0)
# fit_2 <- sarima(log(x), p=0, d=1, q=2, no.constant=FALSE)  # ARIMA(0,1,2)
# fit_1
# fit_2
# 
# # Convert ARMA process to infinite MA process
# psi <- ARMAtoMA(ar=fit_1$fit$coef[1], ma=0, 10)
# dev.off()
# plot(psi, type="o", ylab = expression(psi[j]), xlab = "j", las=1)

# The models can also be estimated by using the Arima() function in the "forecast" package
fit_1 <- Arima(log(x), order = c(1,1,0), include.constant = TRUE) # ARIMA(1,1,0)
fit_2 <- Arima(log(x), order = c(0,1,2), include.constant = TRUE) # ARIMA(0,1,2)
summary(fit_1)
summary(fit_2)

# Convert ARMA process to infinite MA process 
psi_ar <- ARMAtoMA(ar=fit_1$coef[1],   ma=0, lag.max=10)
psi_ma <- ARMAtoMA(ma=fit_2$coef[1:2], ar=0, lag.max=10)
par(mfcol=c(1,2))
plot(psi_ar, type="o", ylab = expression(psi[j]), xlab = "j", las=1, main="ARIMA(1,1,0)")
plot(psi_ma, type="o", ylab = expression(psi[j]), xlab = "j", las=1, main="ARIMA(0,1,2)")

# Fitted values
x_hat_1 <- fit_1$fitted #x_hat_1 <- fitted(fit_1) # also x_hat <- fitted.Arima(fit_1) by using the "forecast" package
x_hat_2 <- fit_2$fitted #x_hat_2 <- fitted(fit_2)
ts.plot(x, ylab="Billions of US dollars", main="ARIMA(1,1,0)")
lines(exp(x_hat_1), col = 2)
legend("topleft",title="",legend=c("Observed GDP","Fitted GDP"), lty=c(1,1), col=c(1,2), bty="n")
ts.plot(x, ylab="Billions of US dollars", main="ARIMA(0,1,2)")
lines(exp(x_hat_2), col = 2)
legend("topleft",title="",legend=c("Observed GDP","Fitted GDP"), lty=c(1,1), col=c(1,2), bty="n")

######################### Diagnostic checking #########################
# Residuals
e_1 <- residuals(fit_1) # also e <- fit_1$residuals
e_2 <- residuals(fit_2)

# Standardized residuals: residuals/sigma. NOTE: use the standard() function in the "TSA" package
sr_1 <- rstandard(fit_1)  # also sr_1 <- residuals(fit_1)/sqrt(fit_1$sigma2) 
sr_2 <- rstandard(fit_2)

# Plot of residuals
par(mfcol=c(3,2))
ts.plot(sr_1, xlab="t", ylab="Standardized residuals", main="ARIMA(1,1,0)")
Acf(sr_1, main="")
Pacf(sr_1, main="")
ts.plot(sr_2, xlab="t", ylab="Standardized residuals", main="ARIMA(0,1,2)")
Acf(sr_2, main="")
Pacf(sr_2, main="")

# Descriptive statistics of residuals
summary(sr_1); summary(sr_2)
sd(sr_1); sd(sr_2)
skewness(sr_1); skewness(sr_2)
kurtosis(sr_1); kurtosis(sr_2)

# Pormanteau test
Box.test(sr_1, lag=20, type="Box-Pierce", fitdf=2) # where fitdf = p+q+1
Box.test(sr_2, lag=20, type="Ljung-Box",  fitdf=3)

# Histogram
par(mfcol=c(1,2))
hist(sr_1, prob=TRUE, ylim=c(0,0.45), xlab = "Standardized residuals", main="ARIMA(1,1,0)")
lines(seq(min(sr_1),max(sr_1),length=100), dnorm(seq(min(sr_1),max(sr_1),length=100),mean(sr_1), sd(sr_1)), col=2)
#
hist(sr_2, prob=TRUE, ylim=c(0,0.45), xlab = "Standardized residuals", main="ARIMA(0,1,2)")
lines(seq(min(sr_2),max(sr_2),length=100), dnorm(seq(min(sr_2),max(sr_2),length=100),mean(sr_2), sd(sr_2)), col=2)

# Jarque–Bera test for normality in the innovations (implemented in the "moments" package)
jarque.test(as.vector(sr_1))
jarque.test(as.vector(sr_2))

# In the "normtest" package
jb.norm.test(sr_1)
jb.norm.test(sr_2)

# QQ plot
par(mfcol=c(1,2))
qqnorm(sr_1, main="ARIMA(1,1,0)")
abline(0,1, col=2)
qqnorm(sr_2, main="ARIMA(0,1,2)")
abline(0,1, col=2)

# Kolmogorov-Smirnov Tests
ks.test(sr_1,"pnorm", mean(sr_1), sd(sr_1))   # Kolmogorov-Smirnoff test
ks.test(sr_2,"pnorm", mean(sr_2), sd(sr_2))

#tsdiag(fit_1)
############################# Forecasting #############################
x_forecasts_1 <- forecast(log(x), model=fit_1, h=9) # correspond to nine quarters (i.e. two years and one quarter)
x_forecasts_2 <- forecast(log(x), model=fit_2, h=9) # correspond to nine quarters (i.e. two years and one quarter)
summary(x_forecasts_1)
summary(x_forecasts_2)

par(mfcol=c(1,2))
plot(x_forecasts_1, ylab="log GDP", main="")
plot(x_forecasts_2, ylab="log GDP", main="")
dev.off()

# Accuracy measures for a forecast model (in the "forecast" package)
accu_1 <- accuracy(x_forecasts_1$mean, log(x[215:223]))
accu_2 <- accuracy(x_forecasts_2$mean, log(x[215:223]))

# Diebold-Mariano test for predictive accuracy
dm.test(residuals(x_forecasts_1), residuals(x_forecasts_2), alternative="two.sided", h=9)
dm.test(residuals(x_forecasts_1), residuals(x_forecasts_2), alternative="less", h=9)
dm.test(residuals(x_forecasts_1), residuals(x_forecasts_2), alternative="greater", h=9)

n <- length(x)
h <- 9
X <- matrix(NA,n+h,3)
X[n+1:h,1] <- exp(x_forecasts_1$lower[,2])
X[1:n,2] <- x
X[n+1:h,2] <- exp(x_forecasts_1$mean)
X[n+1:h,3] <- exp(x_forecasts_1$upper[,2])

ts.plot(X, lty=c(2,1,2), col=c(2,1,2), ylab="GDP", main="")
abline(v=n, lty=2)

ts.plot(cbind(exp(x_forecasts_1$lower[,2]), exp(x_forecasts_1$mean), exp(x_forecasts_1$upper[,2])), lty=c(2,1,2), col=c(2,1,2))

#########################################################################################################
# Monthly international airline passengers, 1949-1960, Source: Box & Jenkins (1970).
x <- AirPassengers
log_x <- log(x); dlog_x <- diff(log_x); Ddlog_x <- diff(dlog_x, 12)

postscript("/Users/Santiago/Dropbox/Teaching/Time Series/Slides/seas_diff.eps",width = 15.5, height = 8.5, horizontal = TRUE, onefile = FALSE, paper = "a4")
par(mfrow=c(2,2), mar=c(2.5,4.5,0.7,0.3))
ts.plot(x, ylab=expression(X[t]))
ts.plot(log_x, ylab=expression(log(X[t])))
ts.plot(dlog_x, ylab=expression(Delta~log(X[t])))
ts.plot(Ddlog_x, ylab=expression(Delta[12]~Delta~log(X[t])))
dev.off()
#plot.ts(cbind(x,log_x,dlog_x, Ddlog_x), yax.flip=TRUE, main="")

# below of interest for showing seasonal RW (not shown here):
#par(mfrow=c(2,1))
#monthplot(dlx); monthplot(ddlx)

par(mfcol=c(1,2), mar=c(4,4,0.3,0.3))
Acf(Ddlog_x, 50, ylim=c(-1,1), main="")
Pacf(Ddlog_x, 50, ylim=c(-1,1), main="")

dev.off()
# Estimation by using the sarima() function in the "astsa" package
model_1 <- sarima(log_x, p=1, d=1, q=1, P=0, D=1, Q=1, S=12) # ARIMA(1,1,1)x(0,1,1)_12
model_1

model_2 <- sarima(log_x, p=0, d=1, q=1, P=0, D=1, Q=1, S=12) # ARIMA(0,1,1)x(0,1,1)_12
model_3 <- sarima(log_x, p=1, d=1, q=0, P=0, D=1, Q=1, S=12) # ARIMA(1,1,0)x(0,1,1)_12
model_2
model_3

# The models can also be estimated by using the Arima() function in the "forecast" package
fit_1 <- Arima(ts(log_x, freq=12), order=c(1,1,1), seasonal=c(0,1,1))
fit_2 <- Arima(ts(log_x, freq=12), order=c(0,1,1), seasonal=c(0,1,1))
fit_3 <- Arima(ts(log_x, freq=12), order=c(1,1,0), seasonal=c(0,1,1))
summary(fit_1)

# Forecasting using the model 2
h <- 36
x_forecasts <- sarima.for(log_x, n.ahead=h, p=0, d=1, q=1, P=0, D=1, Q=1, S=12)

n <- length(x)
X <- matrix(NA,n+h,4)
X[1:n,1] <- x
X[n+1:h,2] <- exp(x_forecasts$pred-x_forecasts$se)
X[n+1:h,3] <- exp(x_forecasts$pred)
X[n+1:h,4] <- exp(x_forecasts$pred+x_forecasts$se)

ts.plot(X, lty=c(1,2,1,2), col=c(1,"gray",2,"gray"), ylab="Air Passengers", main="")
abline(v=n, lty=2)

#########################################################################################################
######################### Unit Root Tests #########################
x <- gnp

# Determination of lag p for for the ADF test
floor(12*(length(x)/100)^0.25)
floor(4*(length(x)/100)^0.25)
floor((length(x)-1)^(1/3))

# Dickey-Fuller test
df1 <- ur.df(log(x), type = "trend", lags=6, selectlags="BIC") # ur.df() function in the "urca" package
summary(df1)
plot(df1)

df2 <- ur.df(log(x), type = "drift", lags=6, selectlags="BIC")
summary(df2)
plot(df2)

df3 <- ur.df(log(x), type = "none", lags=6, selectlags="BIC")
summary(df3)
plot(df3)

# Also with the urdfTest() function in the "fUnitRoots" package
df_1 <- urdfTest(log(x), lags = 1, type = "ct") 
df_1
padf(-2.6185, trend = "ct") # Dickey-Fuller p-values

df_2 <- urdfTest(log(x), lags = 1, type = "c")
df_2
padf(-1.2097, trend = "c") # Dickey-Fuller p-values

df_3 <- urdfTest(log(x), lags = 1, type = "nc")
df_3
padf(6.2907 , trend = "nc") # Dickey-Fuller p-values

# Also with the adf.test() function in the "tseries" package. Note: it incorporates a constant and a linear trend
df <- adf.test(log(x), k=1)
df

#ndiffs(x, alpha=0.05, test="adf", max.d=1)

# Phillips-Perron test
pp1 <- ur.pp(log(x), type = "Z-tau", model = "trend", lags = "long") # ur.pp() function in the "urca" package
summary(pp1)
plot(pp1)

pp2 <- ur.pp(log(x), type = "Z-tau", model = "constant", lags = "long")
summary(pp2)
plot(pp2)


# Also with the urppTest() function in the "fUnitRoots" package
pp_1 <- urppTest(log(x), type = "Z-tau", model = "trend", lags = "long")
pp_1

pp_2 <- urppTest(log(x), type = "Z-tau", model = "constant", lags = "long")
pp_2

# KPSS test
kpss1 <- ur.kpss(log(x), type = "tau", lags = "long")
summary(kpss1)
plot(kpss1)

kpss2 <- ur.kpss(log(x), type = "mu", lags = "long")
summary(kpss2)
plot(kpss2)

# Also with the urTest() function in the "fUnitRoots" package
kpss_1 <- urkpssTest(log(x), type = "tau", lags = "long")
kpss_1

kpss_2 <- urkpssTest(log(x), type = "mu", lags = "long")
kpss_2
#########################################################################################################
# Gross Domestic Product (quarterly 2000Q1-2015Q4)
# data <- read.table("/Users/Santiago/Dropbox/Teaching/Time Series/data/gdp.txt", sep="\t", header=TRUE)
# x <- ts(data[,2]/1000, start=c(2000,1), end=c(2015,4), frequency=4)
# postscript("/Users/Santiago/Dropbox/Teaching/Time Series/Slides/gdp.eps",width = 15.5, height = 8.5, horizontal = TRUE, onefile = FALSE, paper = "a4")
# dev.off()
#########################################################################################################
# Getting some information on a dataset
# ls()          # list objects in the working environment
# names(x)      # list the variables in mydata
# dim(x)        # dimensions of an object
# class(x)      # class of an object (numeric, matrix, data frame, etc)
# x             # print mydata 
# head(x, n=10) # print first 10 rows of mydata
# tail(x, n=10) # print last 10 rows of mydata
#########################################################################################################
