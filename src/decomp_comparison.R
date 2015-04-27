### Comparing Sirota decomp rate to Baiser 2011 decomp rates

source('./o2model.R')

test <- simPitcher()

t <- seq(0,2880,by=1)
beta.0 <- 0.000001
inc.beta <- 0.0000001
k <- 0

## replace with analytical solution
## while (round(f.w(2880,beta.0,w0=75),5)>0){
## k <- k + 1
## if (k == 1000){plot(f.w(t,beta.0,w0=75),ylim=c(0,75));k <- 0}
## beta.0 <- beta.0 + inc.beta
## }

solve.b0 <- function(t=2880,w0=75,wt=0.00001){
    ### Note: t should be the time at which there is an undetectable 
    ### amount of prey. Thus, wt is a non-zero but undetectable mass.
    (-1 * (log(wt,base=exp(1))-log(w0,base=exp(1)))) / t
}

beta.0 <- solve.b0(w0=75) # wasp in micrograms

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
plot(w)
abline(v=c(1440,(1440*2)))

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

decomp <- read.csv('http://harvardforest.fas.harvard.edu/data/p16/hf169/hf169-02-decomp.csv')
fweb <- read.csv('http://harvardforest.fas.harvard.edu/data/p16/hf169/hf169-01-change.csv')
decomp <- decomp[fweb[,2]>=0,] # delimit to food web size
decomp[,2:3] <- decomp[,2:3] * 1000 #convert mg to ug
                                        #
## Initial to final mass comparison
plot(decomp[,3]~decomp[,2])

### Fit the curve using the data
b0.fit <- apply(decomp,1,function(x) solve.b0(1440*14,x[2],x[3]))
b0.fit <- b0.fit[b0.fit!=Inf]
plot(b0.fit)
hist(b0.fit,xlim=range(c(b0.fit,beta.0)))
abline(v=mean(b0.fit))
abline(v=beta.0,lty=2)


