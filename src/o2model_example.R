###Example: how to use the functions from o2model.R.

source('./o2model.R')

test <- simPitcher(
                   days=5,                #number of days
                   t=seq(0,1440,by=1),    #time sequence per day
                   x0=0,                  #initial oxygen level
                   a0=10,                 #initial effect of nitrogen on photosynthesis
                   a.max=2,               #maximum impact of nitrogen on photosynthesis
                   a.min=0,               #minimum impact of nitrogen on photosynthesis
                   s=10,                  #
                   d=0.5,                 #
                   m=1,                   #
                   w0=100,                #initial decomposition
                   Kw=0.001)              #

test <- list()
for (i in 1:9){test[[i]] <- simPitcher(x0=i)}
par(mfrow=c(3,3),mar=c(0,0,0,0),mai=c(0,0,0,0))
for (i in 1:9){plot(test[[i]][,2],xaxt='n',yaxt='n')}
for (i in 1:9){plot(test[[i]][,3],xaxt='n',yaxt='n')}
for (i in 1:9){plot(test[[i]][,4],xaxt='n',yaxt='n')}

x <- test[,2]
x.col <- rainbow(length(x))
par(mfrow=c(1,2))
plot(x,col=x.col)
plot(x[1:(length(x)-1)]~x[2:length(x)],col=x.col[2:length(x)])

par(mfrow=c(1,1),mai=c(0,0,0,0),mar=c(0,0,0,0))
for (i in 1:(length(x)-1)){
  plot(windows(x,i),xaxt='n',yaxt='n',type='l')
  legend('topright',legend=c(i,round((i/(length(x)-1)),2)),bty='n')
}
