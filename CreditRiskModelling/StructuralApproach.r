
calc.EDP <- function(VE,D,muE,sigmaE,rfree,ttm) { assign("theta",c(VE,muE,sigmaE,D,rfree,ttm),env=.GlobalEnv) xinit <- c(VE+D,muE*(VE/(VE+D)),sigmaE*(VE/(VE+D)))
#xinit <- rep(100,3)
xl <- c(0,-9999.9,0.0) ; xu <- rep(9999.9,3)
cond <- list(iter.max=1000,eval.max=10000,x.tol=1.0e-20)
x <- nlminb(xinit,fcn,lower=xl,upper=xu,control=cond)
x <- x$par
d2 <- (log(x[1]/D)+(x[2]-x[3]^2/2)*ttm) / (x[3]*sqrt(ttm)) c(pnorm(-d2),x)
}
￼
fcn <- function(x) {
  d1 <- (log(x[1]/theta[4])+(x[2]+x[3]^2/2)
        *theta[6])/(x[3]*sqrt(theta[6]))
  d2 <- d1 - x[3]*sqrt(theta[6])
  a1 <- theta[1]-(x[1]*pnorm(d1)-
        theta[4]*exp(-theta[5]*theta[6])*pnorm(d2))
  a2 <- theta[1]*theta[3]-(x[1]*x[3]*pnorm(d1))
  a3 <- theta[1]*theta[2]-x[1]*x[2]
  sum(c(a1,a2,a3)^2)
}
