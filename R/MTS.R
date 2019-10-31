rm(list = ls())
ls()

library(astsa)        # Applied Statistical Time Series Analysis
library(TSA)          # Time Series Analysis
library(forecast)     # Forecasting Functions for Time Series and Linear Models
library(urca)         # Unit Root and Cointegration Tests for Time Series Data
library(moments)      # Moments, cumulants, skewness, kurtosis and related tests
library(normtest)     # Tests for Normality
#library(fUnitRoots)   # Trends and Unit Roots
#library(tseries)
# library(fGarch)
library(MTS)
library(vars)

# Examples
# Quarterly 1960Q1-1982Q4, seasonally adjusted, West German fixed investment, disposable income, consumption expenditures in billions of DM, source: Deutsche Bundesbank
data <- read.table("/Users/Santiago/Dropbox/Teaching/Time Series/data/german.txt", sep="\t", header=TRUE)
german <- ts(data, start=c(1960,1), end=c(1982,4), frequency=4)
#
postscript("/Users/Santiago/Dropbox/Teaching/Time Series/Slides/german.eps",width = 15.5, height = 8.5, horizontal = TRUE, onefile = FALSE, paper = "a4")
par(mfrow=c(1,1), mar=c(2,4.2,0.2,0.2), las=0, cex.axis=1.5, cex.lab=1.5)
ts.plot(german, lty=c(1,2,3), ylab="Billions of Deutsche Marks")
legend("topleft",title="",legend=c("Investment", "Income", "Consumption"), lty=c(1,2,3), cex=1.2, bty="n")
dev.off()

save(german, file="/Users/Santiago/Dropbox/Teaching/Time Series/data/german.RData")
#load("/Users/Santiago/Dropbox/Teaching/Time Series/data/german.RData")

# Seasonally adjusted U.S.real GDP (in logarithm) and unemployment rate, 1948Q1-2011Q4
data <- read.table("/Users/Santiago/Dropbox/Teaching/Time Series/data/q-gdpunemp.txt", sep="\t", header=TRUE)
x <- ts(data, start=c(1948,1), end=c(2011,4), frequency=4)
#
postscript("/Users/Santiago/Dropbox/Teaching/Time Series/Slides/gdpunemp.eps",width = 15.5, height = 8.5, horizontal = TRUE, onefile = FALSE, paper = "a4")
par(mfrow=c(2,1), mar=c(2,4.2,0.2,0.2), las=0, cex.axis=1.5, cex.lab=1.5)
ts.plot(log(x[,1]), ylab="log(GDP)")
ts.plot(x[,2], ylab="Unemployment rate")
dev.off()

# Canadian employment (in 1000 persons), labour productivity, manufacturing real wage, and unemployment rate, 1980Q1-2000Q4 
# "vars" package is required 
data(Canada)
Canada

postscript("/Users/Santiago/Dropbox/Teaching/Time Series/Slides/canada.eps",width = 15.5, height = 8.5, horizontal = TRUE, onefile = FALSE, paper = "a4")
par(mfrow=c(2,2), mar=c(2,4.2,0.2,0.2), las=0, cex.axis=1.5, cex.lab=1.5)
ts.plot(Canada[,1], ylab="Employment in 1000 persons")
ts.plot(Canada[,2], ylab="Labour productivity")
ts.plot(Canada[,3], ylab="Manufacturing real wage")
ts.plot(Canada[,4], ylab="Unemployment rate in %")
dev.off()

# Money demand function of Denmark, logarithm of real money M2 (LRM), logarithm of real income (LRY), logarithm of price deflator (LPY), bond rate (IBO) and bank deposit rate (IDE). 1974Q1-1987Q3 
# "vars" package is required 
data(denmark)
denmark

postscript("/Users/Santiago/Dropbox/Teaching/Time Series/Slides/canada.eps",width = 15.5, height = 8.5, horizontal = TRUE, onefile = FALSE, paper = "a4")
par(mfrow=c(2,2), mar=c(2,4.2,0.2,0.2), las=0, cex.axis=1.5, cex.lab=1.5)
ts.plot(denmark[,"LRM"], ylab="Logarithm of real money")
ts.plot(denmark[,"LRY"], ylab="Logarithm of real income")
#ts.plot(diff(denmark[,"LPY"]), ylab="Logarithm of price deflator")
ts.plot(denmark[,"IBO"], ylab="Bond rate")
ts.plot(denmark[,"IDE"], ylab="Bank deposit rate")
dev.off()

denmark <- cbind(exp(denmark[,"LRM"]), exp(denmark[,"LRY"]), denmark[,"IBO"], denmark[,"IDE"])
colnames(denmark) <- c("RM", "RY", "IBO", "IDE")
denmark <- ts(denmark[, c("RM", "RY", "IBO", "IDE")], start=c(1974,1), end=c(1987,3), frequency=4)
save(denmark, file="/Users/Santiago/Dropbox/Teaching/Time Series/data/denmark.RData")
#load("/Users/Santiago/Dropbox/Teaching/Time Series/data/denmark.RData")

# Money demand function of Finland, logarithm of real money M1 (lrm1), logarithm of real income (lny), marginal rate of interest (lnmr), infrlation rate (difp). 1958Q2-1984Q3 
# "urca" package is required 
data(finland)
finland

par(mfrow=c(2,2), mar=c(2,4.2,0.2,0.2), las=0, cex.axis=1.5, cex.lab=1.5)
ts.plot(finland[,"lrm1"], ylab="Logarithm of real money")
ts.plot(finland[,"lny"], ylab="Logarithm of real income")
ts.plot(finland[,"lnmr"], ylab="Marginal rate of interest")
ts.plot(finland[,"difp"], ylab="Inflation rate")
dev.off()

finland <- cbind(exp(finland[,"lrm1"]), exp(finland[,"lny"]), finland[,"lnmr"], finland[,"difp"])
colnames(finland) <- c("RM", "RY", "I", "IF")
finland <- ts(finland[, c("RM", "RY", "I", "IF")], start=c(1958,2), end=c(1984,3), frequency=4)
save(finland, file="/Users/Santiago/Dropbox/Teaching/Time Series/data/finland.RData")

# Monthly simple returns of the stocks of International Business Machines (IBM) and Coca Cola (KO) and the S&P Composite index (SP), January 1961 to December 2011
data("mts-examples",package="MTS")
x <- ts(log(ibmspko[,2:4]+1), start=c(1961,1), end=c(2011,12), frequency=12)

postscript("/Users/Santiago/Dropbox/Teaching/Time Series/Slides/returns.eps",width = 15.5, height = 8.5, horizontal = TRUE, onefile = FALSE, paper = "a4")
par(mfrow=c(3,1), mar=c(2,4.2,0.2,0.2), las=0, cex.axis=1.5, cex.lab=1.5)
ts.plot(x[,1], ylab="IBM")
ts.plot(x[,2], ylab="SP")
ts.plot(x[,3], ylab="KO")
dev.off()

# 
# Interest rates paid on 3-month treasury bill (TBILL), 5-year (R5), and 10-year (R10) interest rates of US government securities. 1960Q1 to 2012Q4.
data <- read.table("/Users/Santiago/Dropbox/Teaching/Time Series/data/interest.txt", sep="\t", header=TRUE)
interest <- ts(data, start=c(1960,1), end=c(2012,4), frequency=4)
#
postscript("/Users/Santiago/Dropbox/Teaching/Time Series/Slides/german.eps",width = 15.5, height = 8.5, horizontal = TRUE, onefile = FALSE, paper = "a4")
par(mfrow=c(1,1), mar=c(2,4.2,0.2,0.2), las=0, cex.axis=1.5, cex.lab=1.5)
ts.plot(interest, lty=c(1,2,3), ylab="Interest rates")
legend("topleft",title="",legend=c("Treasury bill", "5-year", "10-year"), lty=c(1,2,3), cex=1.2, bty="n")
dev.off()

save(interest, file="/Users/Santiago/Dropbox/Teaching/Time Series/data/interest.RData")
#load("/Users/Santiago/Dropbox/Teaching/Time Series/data/interest.RData")

# Bivariate VAR(1) process
n <- 300
#phi_1 <- matrix(c(0.5,0.1,0.4,0.8),2,2)
phi_1 <- matrix(c(0.2,-0.6,0.3,1.1),2,2)
#Sig <- matrix(c(1,0.6,0.6,1),2,2)
Sig <- matrix(c(1,0.8,0.8,2),2,2)
delta <- as.vector(c(5,3))
var_1 <- VARMAsim(n, arlags = c(1), cnst = delta, phi = phi_1, skip = 200, sigma=Sig)
x_t <- var_1$series
par(mfrow=c(2,1), mar=c(2,4.5,0.2,0.2), las=0, cex.axis=1.5, cex.lab=1.5)
ts.plot(x_t[,1], ylab=expression(X[1][t]))
ts.plot(x_t[,2], ylab=expression(X[2][t]))

eigen(phi_1)$values
ccm(x_t, lags = 3, level = TRUE, output = TRUE)

# Bivariate VMA(1) process
n <- 300
theta_1 <- matrix(c(-0.5,0,0,-0.6),2,2)
Sig <- matrix(c(1,0.8,0.8,2),2,2)
mu <- as.vector(c(2,1))
vma_1 <- VARMAsim(n, malags = c(1), cnst = delta, theta=theta_1, skip = 200, sigma=Sig)
x_t <- vma_1$series
par(mfrow=c(2,1), mar=c(2,4.5,0.2,0.2), las=0, cex.axis=1.5, cex.lab=1.5)
ts.plot(x_t[,1], ylab=expression(X[1][t]))
ts.plot(x_t[,2], ylab=expression(X[2][t]))

eigen(phi_1)$values
ccm(x_t, lags = 3, level = TRUE, output = TRUE)

# Bivariate VARMA(1,1) process
n <- 300
phi_1=matrix(c(0.2,-0.6,0.3,1,1),2,2)
sig=matrix(c(4,0.8,0.8,1),2,2)
theta_1=matrix(c(-0.5,0,0,-0.6),2,2)
varma_1_1 <- VARMAsim(n, arlags=c(1), malags=c(1), phi=phi_1, theta=theta_1, sigma=Sig)
x_t <- varma_1_1$series
par(mfrow=c(2,1), mar=c(2,4.5,0.2,0.2), las=0, cex.axis=1.5, cex.lab=1.5)
ts.plot(x_t[,1], ylab=expression(X[1][t]))
ts.plot(x_t[,2], ylab=expression(X[2][t]))

eigen(phi_1)$values
eigen(theta_1)$values
ccm(x_t, lags = 3, level = TRUE, output = TRUE)

#################################### Applications #################################### 
# Canadian employment (in 1000 persons), labour productivity, manufacturing real wage, and unemployment rate, 1980Q1-2000Q4 
# "vars" package is required
library(vars)
data("Canada")
Canada
summary(Canada)

canada <- Canada[, c("rw", "prod", "e", "U")]
save(canada, file="/Users/Santiago/Dropbox/Teaching/Time Series/data/canada.RData")
#load("/Users/Santiago/Dropbox/Teaching/Time Series/data/canada.RData")

#### Graphs ####
par(mfrow=c(2,2), mar=c(2,4.2,0.2,0.2), las=0, cex.axis=1.5, cex.lab=1.5)
ts.plot(Canada[,"e"], ylab="Employment in 1000 persons")
ts.plot(Canada[,"prod"], ylab="Labour productivity")
ts.plot(Canada[,"rw"], ylab="Manufacturing real wage")
ts.plot(Canada[,"U"], ylab="Unemployment rate in %")

#### Unit root tests for each variable (ADF test) ####
library(urca)         # Unit Root and Cointegration Tests for Time Series Data
# Employment
df_e <- ur.df(Canada[,"e"], type = "trend", lags=6, selectlags="BIC") # ur.df() function in the "urca" package
summary(df_e)
plot(df_e)
# Productivity
df_prod <- ur.df(Canada[,"prod"], type = "trend", lags=6, selectlags="BIC") # ur.df() function in the "urca" package
summary(df_prod)
plot(df_prod)
# Real wage
df_rw <- ur.df(Canada[,"rw"], type = "trend", lags=6, selectlags="BIC") # ur.df() function in the "urca" package
summary(df_rw)
plot(df_rw)
# Unemployment rate
df_u <- ur.df(Canada[,"U"], type = "trend", lags=6, selectlags="BIC") # ur.df() function in the "urca" package
summary(df_u)
plot(df_u)

#### Optimal lag length of the VAR model ####
VARselect(Canada, lag.max = 10, type = "both")

VARorder(Canada, maxp=10)  # also with the 'MTS' package
#### Estimation of VAR(1) model ####
Canada <- Canada[, c("prod", "e", "U", "rw")]
var_1 <- VAR(Canada, p = 1, include.mean = TRUE)
summary(var_1)

var_1a <- VAR(Canada, p=1) # also with the 'MTS' package
#### Diagnostics ####
plot(var_1, names = "prod")
plot(var_1, names = "e")
plot(var_1, names = "U")
plot(var_1, names = "rw")

# Pormanteau test
ser <- serial.test(var_1, lags.pt = 16, type = "PT.adjusted")
ser

mq(var_1a$residuals, lag= 16, adj=16) # also with the 'MTS' package

# Jarque-Bera multivariate normality test
norm <- normality.test(var_1)
norm

# Multivariate ARCH-LM test
arch1 <- arch.test(var_1, lags.multi = 5)
plot(arch1, names="prod")
plot(arch1, names="e")
plot(arch1, names="U")
plot(arch1, names="rw")

#### Johansen's Cointegration test ####
summary(ca.jo(Canada, type = "trace", ecdet = "trend", K = 3, spec = "transitory"))
# summary(ca.jo(Canada, type = "eigen", ecdet = "trend", K = 3, spec = "transitory"))

vecm <- ca.jo(Canada[, c("rw", "prod", "e", "U")], type = "trace", ecdet = "trend", K = 3, spec = "transitory")
summary(vecm)
vecm.r1 <- cajorls(vecm, r = 1)
summary(vecm.r1$rlm)

## Calculation of t-values for alpha and beta
##
alpha <- coef(vecm.r1$rlm)[1, ]
names(alpha) <- c("rw", "prod", "e", "U")
alpha
beta <- vecm.r1$beta
beta
resids <- resid(vecm.r1$rlm)
N <- nrow(resids)
sigma <- crossprod(resids) / N
## t-stats for alpha (calculated by hand)
alpha.se <- sqrt(solve(crossprod(cbind(vecm@ZK %*% beta, vecm@Z1)))[1, 1] * diag(sigma))
names(alpha.se) <-  c("rw", "prod", "e", "U")
alpha.t <- alpha / alpha.se
alpha.t
## Differ slightly from coef(summary(vecm.r1$rlm))
## due to degrees of freedom adjustment 
coef(summary(vecm.r1$rlm))
## t-stats for beta
beta.se <- sqrt(diag(kronecker(solve(crossprod(vecm@RK[, -1])),solve(t(alpha) %*% solve(sigma) %*% alpha))))
beta.t <- c(NA, beta[-1] / beta.se)
names(beta.t) <- rownames(vecm.r1$beta)
beta.t

######

rel_coint <- Canada[, "rw"]+0.54487553*Canada[, "prod"]-0.01299605*Canada[, "e"]+1.72657188*Canada[, "U"]
plot(rel_coint)

df_rel_coint <- ur.df(rel_coint, type = "trend", lags=6, selectlags="BIC") # ur.df() function in the "urca" package
summary(df_rel_coint)
plot(df_rel_coint)

#
var <- vec2var(vecm, r = 1)
svec.irf <- irf(var, response = "U", n.ahead = 48, boot = TRUE)
plot(svec.irf, plot.type = c("multiple"))
