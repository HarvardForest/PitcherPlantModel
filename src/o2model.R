###Decomp model for the Pitcher Plant system
##MKLau 16Oct2014

windows <- function(x,w=1){
  y <- x[(1+w):length(x)]
  x <- x[1:(length(x)-w)]
  return(cbind(x,y))
}

f.A <- function(t=seq(0,1440,by=1),Amax=4,LCP=20,c=100,f=1/1440){
  PAR <- c*sin(2*pi*f*t)
  out <- Amax * (1 - exp(-0.3*(PAR-LCP)))
  out[PAR < LCP] <- 0
  return(out)
}

f.w <- function(t=seq(0,2880),beta=0.0031,w0=75){
  w0 * exp(-beta * t)
}

prey <- 1000
fw.75 <- f.w()
t.seq <- seq(0,2880)
d.fw <- abs(fw.75 - prey)
ti <- t.seq[d.fw==min(d.fw)][1]
tf <- d.0hrs + 1440
plot(fw.75,type='l')
abline(v=c(ti,tf),lty=c(1,2))
plot(fw.75[ti:tf],type='l')
fw.75[ti:tf][length(fw.75[ti:tf])]

library(txtplot)
opt.fw <- function(w0=75,by=0.0001,t=seq(0,2880),thresh=0.01){
  beta.i <- 0 + by
  wi <- f.w(w0=w0,beta=beta.i,t=t)
  pf <- wi[length(t)]
  plot(wi~t,type='l',ylim=c(0,w0))
  while (pf > thresh & beta.i < 1){
    beta.i <- beta.i + by
    wi <- f.w(w0=w0,beta=beta.i,t=t)
    pf <- wi[length(t)]
    lines(wi~t)
  }
  return(beta.i)
}

beta <- opt.fw()
t.w5 <- t[abs(f.w(beta=beta)-5)==min(abs(f.w(beta=beta)-5))]

plot(f.w(beta=beta),type='l',ylab='Prey Mass Remaining (ug)',xlab='Time (minutes)',ylim=c(0,75))
lines(f.w(beta=beta,w0=5)[1:1440],col='red')
lines(f.w(w0=f.w(beta=beta,w0=5)[1441]+5,beta=beta)[1:1440],col='red',lty=2)

d.max <- 1000
t.24 <- seq(0,1339)
tf <- 0
w0 <- tf + 75
for (i in 1:d.max){
  w <- f.w(beta=beta,w0=w0,t=t.24)
  tf[i] <- w[length(w)]
  w0 <- tf[i] + w0
}
plot(tf)



simPitcher <- function(days=3,t=seq(0,1440,by=1),x0=0,a0=10,a.max=2,a.min=0,s=10,d=0.5,m=1,w0=100,Kw=0.001){
  wf <- 0
  Amax <- 4
  A <- 0
  n <- 0
  a <- a0
  x <- x0
  out <- list()
  for (d. in 1:days){
    w <- f.w(t=t,w0=w0+wf)  
    for (i in 1:(length(t))){
      n[i] <- (w[i]*x[i]) / (max(w)*Amax)
      A[i] <- f.A(t=t[i],Amax=(Amax+(Amax*n[i])))
      if (i != length(t)){x[i+1] <- A[i] - m + (w[i] / (Kw + w[i]))}
      x[x < 0] <- 0
    }
    wf <- w[length(t)]
    out[[d.]] <- cbind(t=(t+(1440*(d.-1))),x,A,w,n)[-1,]
  }
  return(do.call(rbind,out))
}

