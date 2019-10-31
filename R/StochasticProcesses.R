rm(list = ls())
ls()

library(sde)   # Simulation and Inference for Stochastic Differential Equations With R Examples (Iacus, 2008)
library(astsa) # Applied Statistical Time Series Analysis (David Stoffer, 2014)

# Gaussian process
set.seed(123)
npaths <- 10
N <- 1000
gp <- matrix(NA, N, npaths)
for(i in 1:npaths){
  gp[,i] <- rnorm(N,0,1)
}
par(mfrow=c(1,1), mar=c(4,4,0.2,0.2), las=1, cex.axis=1.5, cex.lab=1.5)
matplot(gp, type="l", lty=1, ylab="X(t)", xlab ="t", xaxt="n")
axis(1, at=c(0, 200, 400, 600, 800, 1000), labels=c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0))

postscript("/Users/Santiago/Dropbox/Teaching/Time Series/Slides/WN.eps",width = 15.5, height = 8.5, horizontal = TRUE, onefile = FALSE, paper = "a4")
par(mfrow=c(1,1), mar=c(4,4,0.2,0.2), las=1, cex.axis=1.5, cex.lab=1.5)
matplot(rnorm(N,0,1), type="l", lty=1, ylab="X(t)", xlab ="t", xaxt="n")
axis(1, at=c(0, 200, 400, 600, 800, 1000), labels=c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0))
dev.off()

# Simulated paths of Brownian motion without drift
nu <- -0.7
X <-sde.sim(t0=0, T=1, X0=0, drift=expression(0), sigma=expression(1), pred=F, N=N, M=npaths)
Y <- X + nu*time(X)

postscript("/Users/Santiago/Dropbox/Teaching/Time Series/Slides/BM.eps",width = 15.5, height = 8.5, horizontal = TRUE, onefile = FALSE, paper = "a4")
par(mfrow=c(1,1), mar=c(4,4,0.2,0.2), las=1, cex.axis=1.5, cex.lab=1.5)
matplot(time(X),Y, type ="l",lty =1, col =" black ", ylab="X(t)", xlab ="t")
dev.off()

# other way
bm <- matrix(NA, N+1, npaths)
for(i in 1:npaths){
  bm[,i] <- BM(x=0, t0=0, T=1, N=N)
}
par(mfrow=c(1,1), mar=c(4,4,0.2,0.2), las=1, cex.axis=1.5, cex.lab=1.5)
matplot(bm, type="l", lty=1, col=1, ylab="X(t)", xlab ="t", xaxt="n")
axis(1, at=c(0, 200, 400, 600, 800, 1000), labels=c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0))

# Simulated paths of Brownian motion with drift
X <-sde.sim(t0=0, T=1, X0=0, drift = expression(10), sigma = expression(2), pred=F, N=N, M=npaths)
Y <- X + nu*time(X)
par(mfrow=c(1,1), mar=c(4,4,0.2,0.2), las=1, cex.axis=1.5, cex.lab=1.5)
matplot(time(X),Y, type ="l",lty =1, col =" black ", ylab="X(t)", xlab ="t")


# Simulated paths of Brownian bridge
bb <- matrix(NA, N+1, npaths)
for(i in 1:npaths){
  bb[,i] <- BBridge(x=0, y=0, t0=0, T=1, N=N)
}
par(mfrow=c(1,1), mar=c(4,4,0.2,0.2), las=1, cex.axis=1.5, cex.lab=1.5)
matplot(bb, type="l", lty=1, col=1, ylab="X(t)", xlab ="t", xaxt="n")
axis(1, at=c(0, 200, 400, 600, 800, 1000), labels=c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0))

# Simulated paths of geometric Brownian motion
gbm <- matrix(NA, N+1, npaths)
for(i in 1:npaths){
  gbm[,i] <- GBM(x=1, r=0.05, sigma=0.02, T=1, N=N)
}
par(mfrow=c(1,1), mar=c(4,4,0.2,0.2), las=1, cex.axis=1.5, cex.lab=1.5)
matplot(gbm, type="l", lty=1, col=1, ylab="X(t)", xlab ="t", xaxt="n")
axis(1, at=c(0, 200, 400, 600, 800, 1000), labels=c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0))

# Homogeneous Poisson process with intensity \lambda parameter
lambda <- 1
t <- cumsum(c(0, rep(1,N)))
p <- matrix(NA, N, npaths)
for(i in 1:npaths){
  p[,i] <- cumsum(rexp(N, lambda))
}

postscript("/Users/Santiago/Dropbox/Teaching/Time Series/Slides/Pois.eps",width = 15.5, height = 8.5, horizontal = TRUE, onefile = FALSE, paper = "a4")
par(mfrow=c(1,1), mar=c(4,4,0.2,0.2), las=1, cex.axis=1.5, cex.lab=1.5)
plot(stepfun(p[,1],t), ylim=c(0,20), xlim=c(0,30), do.points= F, main="", ylab="X(t)", xlab ="t")
for(i in 2:npaths){
  lines(stepfun(p[,i],t))
}
dev.off()

# Short.memory and laong-memory processes
A <- 1
tau <- 0.5

postscript("/Users/Santiago/Dropbox/Teaching/Time Series/Slides/short_long_memory.eps",width = 15.5, height = 8.5, horizontal = TRUE, onefile = FALSE, paper = "a4")
par(mfrow=c(1,1), mar=c(4,4.7,0.2,0.2), las=1, cex.axis=1.5, cex.lab=1.5)
curve(A*tau^x, from=1, to=20, type="l", xlab="k", ylab=expression(rho(k)), ylim=c(0,1), xaxt="n")
curve(A*x^-tau, from=1, to=20, type="l", add = TRUE, lty=2)
axis(1, at=seq(1,20,1), labels=seq(1,20,1), tck=-.01)
grid(lty=2, col=gray(.9))
legend("topright", title="",legend=c("Short-memory","Long-memory"), lty=c(1,2), cex=0.9, bty="n")
dev.off()

# Symmetry of the autocorrelation function
phi <- 0.75
postscript("/Users/Santiago/Dropbox/Teaching/Time Series/Slides/rho.eps",width = 15.5, height = 8.5, horizontal = TRUE, onefile = FALSE, paper = "a4")
par(mfrow=c(1,1), mar=c(4,4.7,0.5,0.2), las=1, cex.axis=1.5, cex.lab=1.5)
curve(phi^x, from=-20, to=20, type="l", xlab="k", ylab=expression(rho(k)), ylim=c(0.035,0.965), xaxt="n")
curve(phi^-x, from=-20, to=20, type="l", add = TRUE, lty=1)
axis(1, at=seq(-20,20,4), labels=seq(-20,20,4), tck=-.01)
abline(h=0, v=0, lty=2, col="gray50")
dev.off()
