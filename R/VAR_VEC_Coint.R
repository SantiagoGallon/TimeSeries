rm(list = ls())

library(moments)      # Moments, cumulants, skewness, kurtosis and related tests
library(normtest)     # Tests for Normality
library(forecast)     # Forecasting Functions for Time Series and Linear Models
library(urca)         # Unit Root and Cointegration Tests for Time Series Data
library(vars)
#library(astsa)        # Applied Statistical Time Series Analysis
#library(TSA)          # Time Series Analysis
#library(MTS)
#library(tseries)

#################################### Applications #################################### 
# Canadian employment (in 1000 persons), labour productivity, manufacturing real wage, and unemployment rate, 1980Q1-2000Q4 
# "vars" package is required
data("Canada")
Canada

#canada <- Canada[, c("rw", "prod", "e", "U")]
#save(canada, file="/Users/Santiago/Dropbox/Teaching/Time Series/data/canada.RData")
#load("/Users/Santiago/Dropbox/Teaching/Time Series/data/canada.RData")

summary(Canada)
# Skewness and curtosis (implemented in the "moments" package)
skewness(Canada) # skewness
kurtosis(Canada) # curtosis

# Histograms
par(mfrow=c(2,2), mar=c(4,4.2,1,0.7), las=0, cex.axis=1.5, cex.lab=1.5)
for (m in 1:4) {
  hist(Canada[,m], prob=TRUE, xlab = colnames(Canada)[m], main="")
}
dev.off()

# Box-plots
par(mfrow=c(2,2), mar=c(1,4.2,0.2,0.2), las=0, cex.axis=1.5, cex.lab=1.5)
for (m in 1:4) {
  boxplot(Canada[,m], ylab=colnames(Canada)[m], horizontal = FALSE)
}
dev.off()

# Portmanteau test for no autocorrelation in the data
Box.test(Canada[,"e"], lag=20, type = "Ljung-Box", fitdf = 0)
Box.test(Canada[,"prod"], lag=20, type = "Ljung-Box", fitdf = 0)
Box.test(Canada[,"rw"], lag=20, type = "Ljung-Box", fitdf = 0)
Box.test(Canada[,"U"], lag=20, type = "Ljung-Box", fitdf = 0)

#### Graphs ####
par(mfrow=c(2,2), mar=c(2,4.2,0.2,0.2), las=0, cex.axis=1.5, cex.lab=1.5)
ts.plot(Canada[,"e"], ylab="Employment in 1000 persons")
ts.plot(Canada[,"prod"], ylab="Labour productivity")
ts.plot(Canada[,"rw"], ylab="Manufacturing real wage")
ts.plot(Canada[,"U"], ylab="Unemployment rate in %")
dev.off()

#### Unit root tests for each variable (ADF test) ####
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
# Determination of lag p for for the ADF test
p_max <- floor(12*(dim(Canada)[1]/100)^0.25)
#p_max <- floor(4*(dim(Canada)[1]/100)^0.25)
#p_max <-floor((dim(Canada)[1]-1)^(1/3))

VARselect(Canada, lag.max = p_max, type = "both")

#### Estimation of VAR(p) model ####
Canada <- Canada[, c("prod", "e", "U", "rw")]
var <- VAR(Canada, p = 2, type = "both")
var
summary(var)

#### Diagnostics ####
plot(var, names = "prod")
plot(var, names = "e")
plot(var, names = "U")
plot(var, names = "rw")

# Pormanteau test
serial.test(var, lags.pt = 16, type = "PT.adjusted")

# Jarque-Bera multivariate normality test
normality.test(var)

# Multivariate ARCH-LM test
arch <- arch.test(var, lags.multi = 5)
arch
plot(arch, names="prod")
plot(arch, names="e")
plot(arch, names="U")
plot(arch, names="rw")

#### Johansen's Cointegration test ####
summary(ca.jo(Canada, type = "trace", ecdet = "trend", K = 3, spec = "transitory"))
#summary(ca.jo(Canada, type = "eigen", ecdet = "trend", K = 3, spec = "transitory"))

vecm <- ca.jo(Canada[, c("rw", "prod", "e", "U")], type = "trace", ecdet = "trend", K = 3, spec = "transitory")
summary(vecm)
# OLS regression of VECM model
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
sigma <- crossprod(resids) / nrow(resids)
## t-stats for alpha (calculated by hand)
alpha.se <- sqrt(solve(crossprod(cbind(vecm@ZK %*% beta, vecm@Z1)))[1, 1] * diag(sigma))
names(alpha.se) <-  c("rw", "prod", "e", "U")
alpha.t <- alpha / alpha.se
alpha.t
## Differ slightly from coef(summary(vecm.r1$rlm)) due to degrees of freedom adjustment 
coef(summary(vecm.r1$rlm))
## t-stats for beta
beta.se <- sqrt(diag(kronecker(solve(crossprod(vecm@RK[, -1])),solve(t(alpha) %*% solve(sigma) %*% alpha))))
beta.t <- c(NA, beta[-1] / beta.se)
names(beta.t) <- rownames(vecm.r1$beta)
beta.t

# Cointegration's relation
rel_coint <- Canada[, "rw"]+0.54487553*Canada[, "prod"]-0.01299605*Canada[, "e"]+1.72657188*Canada[, "U"]
plot(rel_coint)

df_rel_coint <- ur.df(rel_coint, type = "trend", lags=6, selectlags="BIC") # ur.df() function in the "urca" package
summary(df_rel_coint)
plot(df_rel_coint)

# VEC to VAR
var <- vec2var(vecm, r = 1)
# Impulse response coefficients of a VAR(p) (or transformed VECM to VAR(p))
#svec.irf <- irf(var, response = "U", n.ahead = 48, boot = TRUE)
#plot(svec.irf, plot.type = c("multiple"))
