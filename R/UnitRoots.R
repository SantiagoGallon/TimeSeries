rm(list = ls())
ls()

# Install and load the "easypackages" to load and install multiple packages
install.packages("easypackages")
library(easypackages)

# Intalling and loading multiple packages at once (using the "easypackages" package)
install_packages("astsa", "TSA", "forecast", "urca", "moments", "normtest", "tseries")
libraries("astsa", "TSA", "forecast", "urca", "moments", "normtest", "tseries")

# Or Loading packages one at the time
library(astsa)        # Applied Statistical Time Series Analysis
library(TSA)          # Time Series Analysis
library(forecast)     # Forecasting Functions for Time Series and Linear Models
library(urca)         # Unit Root and Cointegration Tests for Time Series Data
library(moments)      # Moments, cumulants, skewness, kurtosis and related tests
library(normtest)     # Tests for Normality
library(tseries)
# library(fGarch)

# Time series analysis of the seasonally adjusted US Gross Domestic Product -GDP- (real billions of 1996, quarterly 1947Q1-2002Q3)
# Source: Federal Reserve Bank of St. Louis (http://research.stlouisfed.org/)
x <- gnp
y <- cbind(as.vector(x), as.vector(log(x)))
# Exporting data (to a tab-delimited text file)
write.table(y, file="/Users/Santiago/Dropbox/Teaching/Time Series/data/gdp.txt", sep="\t")

# Plot of the data
ts.plot(x, xlab="", ylab="GDP", main="")

# ACF of the data (is the series stationary?) using the Acf() function in the "forecast" package
Acf(x, main="")

# Difference-stationary to achieve stationarity in mean and variance
par(mfcol=c(1,2))
ts.plot(diff(x),      xlab="", ylab="First difference of the GDP", main="")   # first difference of the data
ts.plot(diff(log(x)), xlab="", ylab="GDP growth rate", main="")               # GDP growth rate (i.e. first difference of the log(GDP)

par(mfcol=c(1,2))
Acf(x, main="GDP")
Acf(diff(log(x)), main="GDP growth rate ")

#########################################################################################################
# Unit Root Tests

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

# KPSS test
kpss1 <- ur.kpss(log(x), type = "tau", lags = "long")
summary(kpss1)
plot(kpss1)

kpss2 <- ur.kpss(log(x), type = "mu", lags = "long")
summary(kpss2)
plot(kpss2)