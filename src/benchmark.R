### Benchmarking the model for running on the server

library(microbenchmark)
source('global.R')

res <- list()
for (i in 1:15){
    print(i)
    res[[i]] <- microbenchmark(pitcherPlantSim(i),times=1)
    print(res[[i]])
}

times <- (do.call(rbind,lapply(res,data.frame))[,2] * 1e-9)
times.min <- times / 60
plot(times.min)

days <- I(1:length(times))
times.spline <- lm(log(times) ~ days)
days <- seq(0,20,by=0.1)
times.spline <- exp(predict(times.spline,list(days=days)))
plot(times.spline~days)

n <- 1000

fw <- runif(n,0.001,3)
beta <- runif(n,-1,1)
d <- runif(n,0,5)
k <- runif(n,0.001,1)
factor.sample <- data.frame(fw,beta,d,k)

pairs(factor.sample)

time.total <- (times.spline * n)
time.total <- time.total / 60 # minutes
time.total <- time.total / 60 # hours

plot(time.total~days,xlab='Simulated Days',ylab='Runtime (hrs)')
abline(h=c(24,48),col='grey',lty=2)
abline(v=15,col='red',lwd=0.5)
=======

