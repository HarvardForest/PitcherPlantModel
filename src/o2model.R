###Decomp model for the Pitcher Plant system
##MKLau 16Oct2014

PAR <- function(t=seq(1,1440,by=1),LCP=20,f=1/1440,c=100,scalar=-0.3){
    1 - exp(-0.3 * (c*sin(2*pi*f*t)-LCP) * as.numeric((c*sin(2*pi*f*t)-LCP)>0))
}

aug <- function(a.max=2,a.min=0,s=10,d=0.5,n=0){
    ((a.max - a.min) / (1 + exp(-(s*n-d)))) + a.min
}

days <- 20;a <- 10
out <- list()

for (i in 1:days){
    O2 <- a * PAR()
    out[[i]] <- O2
    a <- a * aug()
}

plot(unlist(out))

f.w <- function(t=seq(0,1440,by=1),beta=0.005737,w0=75){
  w0 * exp(-beta * t)
}

simPitcher <- function(days=3,x0=0,a0=10,a.max=2,a.min=0,s=10,d=0.5,m=1,w0=100,prey.t=720,Kw=0.001){
    t=seq(0,1440,by=1)
    wf <- 0
    A <- 0
    n <- 0
    a <- a0
    x <- x0
    Amax <- a * ((a.max-a.min) / (1+exp(s*n[i]-d)) + a.min)
    out <- list()
    for (d in 1:days){
        w <- f.w(t=t,w0=wf+w0)
        if (prey.t != 0){w <- c(rep(wf,prey.t),w);w <- w[1:length(t)]}
        for (i in 1:(length(t))){
            n[i] <- (w[i]*x[i]) / 100 # arbitrary constant
            Amax <- Amax * ((a.max-a.min) / (1+exp(s*n[i]-d)) + a.min)
            A[i] <- f.A(t=t[i],Amax=Amax)
            if (i != length(t)){x[i+1] <- A[i] - m + (w[i] / (Kw + w[i]))}
            x[x < 0] <- 0
        }
        out[[d]] <- cbind(t=(t+(1440*(d-1))),x,A,w,n)[-1,]
        wf <- w[length(w)]
    }
    return(data.frame(do.call(rbind,out)))
}

