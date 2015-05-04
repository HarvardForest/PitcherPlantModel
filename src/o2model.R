###Decomp model for the Pitcher Plant system
##MKLau 16Oct2014

dxdt <- function(x,t){diff(x)/diff(t)}

flip <- function(x){x[length(x):1]}

maxima <- function(x,t=1440){
    tapply(x,sort(rep(1:(length(x)/t),t)),function(x) max(x)[1])
}

ppLag <- function(x=1:10,k=1){
    if (k >= length(x) | k < 1){warning('Check the value of k')}else{
        cbind(x[1:(length(x)-k)],x[(k+1):length(x)])
    }
}

PAR <- function(t=seq(1,1440,by=1),LCP=20,f=1/1440,c=100,scalar=-0.3){
    1 - exp(-0.3 * (c*sin(2*pi*f*t)-LCP) * as.numeric((c*sin(2*pi*f*t)-LCP)>0))
}


aug <- function(a.max=2,a.min=1,s=10,d=0.5,n=0){
    ((a.max - a.min) / (1 + exp(-(s*n-d)))) + a.min
}


decomp <- function(t=seq(1,1440,by=1),beta=0.003,w0=75){
### -1 * log((0.001 / 75),base=exp(1))/2880 -> beta 
  w0 * exp(-beta * t)
}

f.w <- function(days=10,t=seq(1,1440,by=1),beta=0.003,prey=75,w0=0,prey.t=360){
    out <- list()
    for (i in 1:days){
        w <- decomp(w0=w0+prey,beta=beta,t=t)
        out[[i]] <- c(decomp(w0=w0,beta=beta,t=t)[1:prey.t],w[1:(length(w)-prey.t)])
        w0 <- w[(length(w)-prey.t)]
    }
    unlist(out)
}

days <- 10
light <- rep(PAR(),days);w <- f.w(days=days)
m <- 1;Kw <- 100
a <- 10;n <- 0;x <- 0

a <- rep(a,length(w))
n <- rep(n,length(w))

for (i in 1:days){
    if (i == 1){minutes <- seq(2,1440)}else{minutes <- seq(1,1440)}
    for (j in minutes){
        t <- (i-1) * max(minutes) + j
        x[t] <- (a[t-1] * light[t-1]) - (m + (a[t-1] * (w[t-1]/(Kw + w[t-1]))))
#        a[t] <- a[t-1] * aug(a[t-1],n=n[t-1])
#        n[t] <- (w[t-1] * x[t-1]) / 100
    }
}

par(mfrow=c(2,2))
plot(x,type='l')
plot(light*100,type='l',col='grey');lines(w)
plot(a,type='l')
plot(n)

beta.photo <- 1


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

