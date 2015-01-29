###Example: how to use the functions from o2model.R.

source('./o2model.R')

test <- simPitcher(
                   days=5,
                   t=seq(0,1440,by=1),
                   x0=0,
                   a0=10,
                   a.max=2,
                   a.min=0,
                   s=10,
                   d=0.5,
                   m=1,
                   w0=100,
                   Kw=0.001)

x <- test[,2]
x.col <- rainbow(length(x))
par(mfrow=c(1,2))
plot(x,col=x.col)
plot(x[1:(length(x)-1)]~x[2:length(x)],col=x.col[2:length(x)])

