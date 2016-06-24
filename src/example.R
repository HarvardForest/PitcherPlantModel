### testing the funcitons for the pitcher plant model

source('./global.R')

### relations between variables
## decomposition and time

w <- 0
for (t in 2:(1440*3)){
    w[t] <- decomp(w[t-1],beta=4.5e-05)
    if (t == 720){w[t] <- 75}
}
t <- 1:(1440*3)
round(min(w[720:length(w)]),3)
plot(w~t)

## prey and nitrogen
x <- photo(3,Amin=0.1)
plot(x,ylim=c(0,1))
n <- mineralization(x,w,c=5)
plot(n~t)
abline(v=seq(1,(1440*3),by=1440),col='grey')
plot(n~w)

## nitrogen and augmentation
a <- augmentation(n,aMin=1)
plot(a)
plot(a~n)

## augmentation and oxygen
sim <- pitcherPlantSim(5,foodWeight=c(0,75,75,75,0),c=5)

plot(sim$Oxygen~sim$'Augmentation Value',col=rainbow(nrow(sim)),pch=19,cex=0.5)
abline(h=0,col='darkgrey',lty=2)

### Plot
par(mfrow=c(2,2))
plot(w~t,pch=19,cex=0.5)
plot(n~w,pch=19,cex=0.5)
plot(a~n,pch=19,cex=0.5)
plot(a~w,pch=19,cex=0.5)

par(mfrow=c(2,2))
plot(sim$'Food Amount'~sim$Minute,pch=19,cex=0.5,xlab='t',ylab='w (simulated)')
plot(sim$'Nutrients'~sim$'Food Amount',pch=19,cex=0.5,xlab='w (simulated)',ylab='n (simulated)')
plot(sim$'Augmentation Value'~sim$'Nutrients',pch=19,cex=0.5)
plot(sim$'Augmentation Value'~sim$'Food Amount',pch=19,cex=0.5)

par(mfrow=c(1,1))
plot(sim$Oxygen~sim$'Augmentation Value',col=rainbow(nrow(sim)),pch=19,cex=0.5)
