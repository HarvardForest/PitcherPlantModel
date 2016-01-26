### Benchmarking the model for running on the server

source('global.R')
library('txtplot')

microbenchmark <- function(i=1){
    ti <- Sys.time()
    pitcherPlantSim(i)
    tf <- Sys.time()
    return(as.numeric(tf-ti))
}

res <- list()
for (i in 1:15){
    print(i)
    res[[i]] <- microbenchmark(i)
    print(res[[i]])
}

# times <- (do.call(rbind,lapply(res,data.frame))[,2] * 1e-9)
times <- unlist(res)
times.min <- times / 60
txtplot(times.min)

days <- I(1:length(times))
times.spline <- lm(log(times) ~ days)
days <- seq(0,20,by=0.1)
times.spline <- exp(predict(times.spline,list(days=days)))
txtplot(days,times.spline)

n <- 1000

fw <- runif(n,0.001,3)
beta <- runif(n,-1,1)
d <- runif(n,0,5)
k <- runif(n,0.001,1)
factor.sample <- data.frame(fw,beta,d,k)

txtplot(factor.sample[,1],factor.sample[,2],pch='.')

time.total <- (times.spline * n)
time.total <- time.total / 60 # minutes
time.total <- time.total / 60 # hours

tmp <- paste('.tmp',paste(sample(0:9,7),collapse=''),sep='')
pdf(tmp)
plot(days,time.total,xlab='Simulated Days',ylab='Runtime (hrs)')
abline(h=c(24,48),col='grey',lty=2)
abline(v=c(15,20),col='red',lwd=0.5)
dev.off()
system(paste('scp',tmp, 'matthewklau@fas.harvard.edu:public_html/benchmark.pdf'))
system(paste('rm',tmp))
