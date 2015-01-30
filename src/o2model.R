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

f.w <- function(t=seq(0,1440,by=1),beta=0.005737,w0=75){
  w0 * exp(-beta * t)
}

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

