## Nonlinear least squares
f <- function(.pars,.dat,.fun) sum((.dat[,2]-.fun(.pars,.dat[,1]))^2)
nulFun <- function(b,x) b[1]+x*0
polyFun <- function(b,x) b[1]*x^b[2]
expFun <- function(b,x) b[1]*exp(b[2]*x)
optimFun <- function(dat){
    plot(dat)
    out <- optim(c(1,1),f,.dat=dat,.fun=nulFun);lines(dat[,1],nulFun(out$par,dat[,1]),col=1,lty=2);out.nul <- out
    out <- optim(c(1,1),f,.dat=dat,.fun=polyFun);lines(dat[,1],polyFun(out$par,dat[,1]),col=3);out.poly <- out
    out <- optim(c(1,1),f,.dat=dat,.fun=expFun);lines(dat[,1],expFun(out$par,dat[,1]),col=2);out.exp <- out

    out.. <- data.frame(do.call(rbind,list(unlist(out.nul),unlist(out.poly),unlist(out.exp))))
    out..
}

## Input data
X <- 1:10
Y <- runif(length(X))
Y <- exp(X) #red
Y <- X^-2 #green
Y <- X^2 #red
dat <- data.frame(X,Y[length(Y):1])
dat <- as.list(dat)
dat <- data.frame(dat)

