### Return Rate
source('ppmem.R')

n1 <- 5;n2 <- 3;n3 <- 30
days <- n1 + n2 + n3

s <- Sys.time()
x <- pitcherPlantSim(days,foodWeight=c(rep(0,n1),rep(1,n2),rep(0,n3)),beta=0.0001)
f <- Sys.time()

colnames(x) <- c("Minute", "Oxygen", "Photosynthesis",
                    "BOD", "Nutrients",
                    "Augmentation", "foodWeight")

par(mfrow=c(2,2))
plot(x$foodWeight/max(x$foodWeight),type='l');lines(x$BOD/max(x$BOD),col='red')
plot(x$Nutrients/max(x$Nutrients),type='l');lines(x$Augmentation/max(x$Augmentation),col='red')
plot(x$foodWeight~x$BOD);plot(x$Nutrients~x$Augmentation)

par(mfrow=c(1,1))
plot(x$Oxygen~x$Nutrients,col=rainbow(nrow(x)),pch=19)

plot(x$Oxygen,type='l')
abline(h=max(x$Oxygen[1:1440]),lty=2)

