###Decomp model for the Pitcher Plant system
##MKLau 16Oct2014

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
  for (d in 1:days){
    w <- f.w(t=t,w0=w0+wf)  
    for (i in 1:(length(t))){
      n[i] <- (w[i]*x[i]) / (max(w)*Amax)
      A[i] <- f.A(t=t[i],Amax=(Amax+(Amax*n[i])))
      if (i != length(t)){x[i+1] <- A[i] - m + (w[i] / (Kw + w[i]))}
      x[x < 0] <- 0
    }
    wf <- w[length(t)]
    out[[d]] <- cbind(t=(t+(1440*(d-1))),x,A,w,n)[-1,]
  }
  return(do.call(rbind,out))
}
test <- simPitcher()

t <- seq(0,2880,by=1)
beta.0 <- 0.000001
inc.beta <- 0.0000001
k <- 0
while (round(f.w(2880,beta.0,w0=75),5)>0){
k <- k + 1
if (k == 1000){plot(f.w(t,beta.0,w0=75),ylim=c(0,75));k <- 0}
beta.0 <- beta.0 + inc.beta
}

plot(f.w(t,beta=beta.0,w0=75),pch=1,cex=0.25,ylim=c(0,75),xlim=c(0,(1440*15)))
lines(f.w(seq(0,(1440*15)),beta=beta.0,w0=75))
abline(v=2880)

plot(f.w(t,beta=beta.0,w0=100),pch=1,cex=0.25,ylim=c(0,75),xlim=c(0,(1440*15)))
lines(f.w(seq(0,(1440*15)),beta=beta.0,w0=75))
abline(v=2880)

test <- 0
for (i in 1:100){
  test[i] <- f.w(1440,beta=beta.0,w0=i)
}
plot(test~seq(1,100,by=1))

w <- f.w(t)


###Optimize using Baiser 2011
##Only use bacteria only pitchers
##

## Detailed Metadata hf169-01: changes in species richness and trophic
## diversity Plant: identifier: 1-70 InitialS: species richness at the
## beginning of the experiment. Integer: 0 (bacteria only) through 9
## (all species present).  FinalS: species richness at the end of the
## experiment. Integer: 1 – 9.  InitialTD: trophic diversity at the
## beginning of the experiment (unit: dimensionless / missing value:
## NA) FinalTD: trophic diversity at the end of the experiment (unit:
## dimensionless / missing value: NA) hf169-02: decomposition of ant
## carcasses

## Plant: identifier; 1 – 70
## MassI: initial mass (unit: milligram / missing value: NA)
## MassF: final mass (unit: milligram / missing value: NA)

decomp <- read.csv('./hf169-02-decomp.csv')
fweb <- read.csv('./hf169-01-change.csv')
d. <- decomp[fweb[,2]==0,] #delimit to bacteria only pitchers
d.[,2:3] <- d.[,2:3] * 1000 #convert mg to g
                                        #
##get optimal beta value
i <- 7
plot(fp.w(R0=d.[i,2],beta=-0.01,x=x))
points(1440,d.[i,3],pch='x',col='red')


plot((decomp[,2]*1000),((decomp[,2]-decomp[,3])*1000/(decomp[,2]*1000))*100,xlab='Initial Prey (g)',ylab='Percent Prey Remaining')
abline(lm(I((decomp[,2]-decomp[,3])*1000/(decomp[,2]*1000)*100)~I(decomp[,2]*1000)))
summary(lm(I((decomp[,2]-decomp[,3])*1000/(decomp[,2]*1000)*100)~I(decomp[,2]*1000)))

